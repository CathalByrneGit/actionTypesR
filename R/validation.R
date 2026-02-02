#' Validate action parameters
#'
#' @param ctx An ActionContext.
#' @param action_type_id Action type identifier.
#' @param params Named list of parameter values.
#' @return TRUE if validation succeeds.
#' @export
validate_params <- function(ctx, action_type_id, params) {
  action <- resolve_action(ctx, action_type_id)
  param_defs <- action$parameters %||% list()
  params <- params %||% list()

  param_ids <- vapply(param_defs, function(def) def$id, character(1))
  required_ids <- vapply(param_defs, function(def) isTRUE(def$required), logical(1))
  required_params <- param_ids[required_ids]

  unknown_params <- setdiff(names(params), param_ids)
  if (length(unknown_params) > 0) {
    rlang::abort(paste0(
      "Unknown parameter(s) for ", action_type_id, ": ",
      paste(unknown_params, collapse = ", ")
    ))
  }

  missing_params <- required_params[!required_params %in% names(params)]
  null_required <- required_params[vapply(required_params, function(id) is.null(params[[id]]), logical(1))]
  missing_params <- unique(c(missing_params, null_required))
  if (length(missing_params) > 0) {
    rlang::abort(paste0(
      "Required parameter(s) missing for ", action_type_id, ": ",
      paste(missing_params, collapse = ", ")
    ))
  }

  for (def in param_defs) {
    value <- params[[def$id]]
    if (is.null(value)) {
      next
    }
    if (!validate_type(value, def$type %||% "string")) {
      rlang::abort(paste0(
        "Parameter '", def$id, "' must be of type ", def$type, "."
      ))
    }
  }

  TRUE
}

validate_type <- function(value, type) {
  switch(
    tolower(type),
    string = is.character(value),
    integer = is.integer(value) || (is.numeric(value) && all(value == floor(value))),
    number = is.numeric(value),
    boolean = is.logical(value),
    date = inherits(value, "Date"),
    datetime = inherits(value, "POSIXct") || inherits(value, "POSIXt"),
    json = is.list(value),
    FALSE
  )
}
