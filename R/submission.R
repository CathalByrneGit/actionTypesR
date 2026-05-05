#' Submit an action for execution
#'
#' @param ctx An ActionContext.
#' @param action_type_id Action type identifier.
#' @param params Named list of parameters.
#' @param target_ids Optional target identifiers.
#' @param .dry_run If TRUE, return execution plan without executing.
#' @return An ActionResult or action plan.
#' @export
submit_action <- function(ctx, action_type_id, params, target_ids = NULL, .dry_run = FALSE) {
  action <- resolve_action(ctx, action_type_id)
  validate_params(ctx, action_type_id, params)

  if (!is.null(ctx$policy_fn)) {
    ctx$policy_fn(action, params, target_ids, ctx)
  }

  if (isTRUE(.dry_run)) {
    plan <- as_action_plan(action, params, normalize_target_ids(target_ids))
    class(plan) <- "ActionPlan"
    return(plan)
  }

  submission_id <- generate_uuid()
  submitted_at <- Sys.time()
  target_ids <- normalize_target_ids(target_ids)

  if (!is.null(ctx$gate_fn)) {
    gate_decision <- ctx$gate_fn(action, params, target_ids, ctx)
    if (identical(gate_decision, "denied")) {
      rlang::abort("Action denied by approval gate.")
    }
    if (identical(gate_decision, "pending")) {
      record_action_log(
        ctx$connection,
        list(
          submission_id    = submission_id,
          action_type_id   = action_type_id,
          params_json      = jsonlite::toJSON(params %||% list(), auto_unbox = TRUE),
          target_ids_json  = jsonlite::toJSON(target_ids %||% list(), auto_unbox = TRUE),
          status           = "pending_approval",
          error_message    = NA_character_,
          submitted_at     = submitted_at,
          completed_at     = NA,
          submitted_by     = na_if_null(Sys.info()[["user"]], NA_character_)
        )
      )
      return(ActionResult(
        submission_id  = submission_id,
        action_type_id = action_type_id,
        status         = "pending_approval",
        timestamp      = submitted_at
      ))
    }
  }

  completed_at  <- NULL
  status        <- "success"
  error_message <- NULL
  result        <- NULL

  impl_kind       <- action$impl_kind %||% action$implementation$kind
  impl_entrypoint <- action$impl_entrypoint %||% action$implementation$entrypoint

  tryCatch({
    result <- dispatch_action(
      ctx,
      action,
      impl_kind,
      impl_entrypoint,
      params,
      target_ids
    )
    completed_at <- Sys.time()
  }, error = function(err) {
    status        <<- "error"
    error_message <<- conditionMessage(err)
    completed_at  <<- Sys.time()
  })

  record_action_log(
    ctx$connection,
    list(
      submission_id    = submission_id,
      action_type_id   = action_type_id,
      params_json      = jsonlite::toJSON(params %||% list(), auto_unbox = TRUE),
      target_ids_json  = jsonlite::toJSON(target_ids %||% list(), auto_unbox = TRUE),
      status           = status,
      error_message    = na_if_null(error_message, NA_character_),
      submitted_at     = submitted_at,
      completed_at     = na_if_null(completed_at, NA),
      submitted_by     = na_if_null(Sys.info()[["user"]], NA_character_)
    )
  )

  if (identical(status, "error")) {
    rlang::abort(paste0("Action failed: ", error_message))
  }

  ActionResult(
    submission_id  = submission_id,
    action_type_id = action_type_id,
    status         = status,
    result         = result,
    error          = error_message,
    timestamp      = completed_at
  )
}

#' Re-submit a previously logged action
#'
#' Looks up the original submission by `submission_id` and calls
#' `submit_action()` again with the same parameters. A new `submission_id` is
#' generated — the original log entry is never modified.
#'
#' @param ctx An ActionContext.
#' @param submission_id The submission_id of the original action to replay.
#' @return A new ActionResult.
#' @export
resubmit_action <- function(ctx, submission_id) {
  row <- DBI::dbGetQuery(
    ctx$connection,
    "SELECT * FROM action_log WHERE submission_id = ? LIMIT 1",
    params = list(submission_id)
  )
  if (nrow(row) == 0L) {
    rlang::abort(paste0("No action found with submission_id: ", submission_id))
  }

  params <- jsonlite::fromJSON(row$params_json[[1L]], simplifyVector = FALSE)
  target_ids_raw <- row$target_ids_json[[1L]]
  target_ids <- if (!is.null(target_ids_raw) &&
                    !is.na(target_ids_raw) &&
                    nchar(target_ids_raw) > 2L) {
    jsonlite::fromJSON(target_ids_raw, simplifyVector = FALSE)
  } else {
    NULL
  }

  submit_action(ctx, row$action_type_id[[1L]], params, target_ids)
}

dispatch_action <- function(ctx, action, impl_kind, impl_entrypoint, params, target_ids) {
  if (is.null(impl_kind)) {
    rlang::abort("Action implementation kind is missing.")
  }
  switch(
    tolower(impl_kind),
    r       = dispatch_r(ctx, action, impl_entrypoint, params, target_ids),
    sql     = dispatch_sql(ctx$connection, impl_entrypoint, params),
    http    = dispatch_http(impl_entrypoint, params, target_ids,
                            ctx$http_timeout, ctx$http_retries),
    plugin  = dispatch_plugin(ctx, action, impl_entrypoint, params, target_ids),
    rlang::abort(paste0("Unsupported implementation kind: ", impl_kind))
  )
}

dispatch_r <- function(ctx, action, impl_entrypoint, params, target_ids) {
  handler <- ctx$handlers[[get_action_id(action)]]
  if (is.null(handler)) {
    handler <- ctx$registry[[impl_entrypoint]]
  }
  if (is.null(handler)) {
    caller_env <- rlang::caller_env()
    if (exists(impl_entrypoint, envir = caller_env, inherits = TRUE)) {
      handler <- get(impl_entrypoint, envir = caller_env, inherits = TRUE)
    }
  }
  if (is.null(handler)) {
    rlang::abort(paste0("No R handler registered for entrypoint: ", impl_entrypoint))
  }
  handler(ctx$connection, action, params, target_ids)
}

dispatch_sql <- function(connection, impl_entrypoint, params) {
  DBI::dbExecute(connection, impl_entrypoint, params = unname(params))
}

dispatch_http <- function(impl_entrypoint, params, target_ids,
                          timeout_sec = 30L, max_retries = 3L) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    rlang::abort("httr2 is required for HTTP action execution.")
  }
  req <- httr2::request(impl_entrypoint)
  req <- httr2::req_body_json(req, list(params = params, target_ids = target_ids))
  req <- httr2::req_timeout(req, timeout_sec)
  req <- httr2::req_retry(req, max_tries = max_retries)
  httr2::req_perform(req)
}

dispatch_plugin <- function(ctx, action, impl_entrypoint, params, target_ids) {
  handler <- ctx$handlers[[get_action_id(action)]]
  if (is.null(handler)) {
    handler <- ctx$registry[[impl_entrypoint]]
  }
  if (is.null(handler)) {
    rlang::abort(paste0("No plugin handler registered for entrypoint: ", impl_entrypoint))
  }
  handler(ctx$connection, action, params, target_ids)
}
