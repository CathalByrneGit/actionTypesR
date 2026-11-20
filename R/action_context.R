#' Create an action execution context
#'
#' @param bundle An ontologySpecR bundle, or anything
#'   `ontologySpecR::as_bundle()` accepts (a list or a path to a JSON file).
#' @param connection A DBI connection.
#' @param registry Optional named list of R handlers by entrypoint.
#' @param http_timeout Timeout in seconds for HTTP dispatch (default 30).
#' @param http_retries Maximum retries for HTTP dispatch (default 3).
#' @return An ActionContext object.
#' @export
action_context <- function(bundle, connection, registry = NULL,
                           http_timeout = 30L, http_retries = 3L) {
  bundle <- ontologySpecR::as_bundle(bundle)

  ensure_action_log_table(connection)

  ctx <- list(
    bundle = bundle,
    connection = connection,
    registry = registry %||% list(),
    handlers = list(),
    policy_fn = NULL,
    gate_fn = NULL,
    post_submit_hooks = list(),
    http_timeout = http_timeout,
    http_retries = http_retries
  )
  class(ctx) <- "ActionContext"
  ctx
}

#' Resolve an action type
#'
#' @param ctx An ActionContext.
#' @param action_type_id Action type identifier.
#' @return The resolved action type object.
#' @export
resolve_action <- function(ctx, action_type_id) {
  ontologySpecR::bundle_action_type(ctx$bundle, action_type_id)
}
