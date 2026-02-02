#' Register an R handler for an action type
#'
#' @param ctx An ActionContext.
#' @param action_type_id Action type identifier.
#' @param fn Handler function.
#' @return Updated ActionContext.
#' @export
register_handler <- function(ctx, action_type_id, fn) {
  ctx$handlers[[action_type_id]] <- fn
  ctx
}

#' Register a policy evaluation function
#'
#' @param ctx An ActionContext.
#' @param policy_fn Policy function.
#' @return Updated ActionContext.
#' @export
register_policy <- function(ctx, policy_fn) {
  ctx$policy_fn <- policy_fn
  ctx
}
