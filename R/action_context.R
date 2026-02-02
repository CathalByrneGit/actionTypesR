#' Create an action execution context
#'
#' @param bundle An ontologySpecR bundle or compatible list.
#' @param connection A DBI connection.
#' @param registry Optional named list of R handlers by entrypoint.
#' @return An ActionContext object.
#' @export
action_context <- function(bundle, connection, registry = NULL) {
  actions <- as_action_list(bundle)
  action_ids <- vapply(actions, get_action_id, character(1))
  action_index <- stats::setNames(actions, action_ids)

  ctx <- list(
    bundle = bundle,
    connection = connection,
    registry = registry %||% list(),
    handlers = list(),
    policy_fn = NULL,
    actions = action_index
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
  action <- ctx$actions[[action_type_id]]
  if (is.null(action)) {
    rlang::abort(paste0("Unknown action type: ", action_type_id))
  }
  action
}
