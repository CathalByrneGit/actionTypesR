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
#' The policy function is called before dispatch. It should abort (via
#' `rlang::abort()`) to deny the action. Accepts either a 3-argument signature
#' `function(action, params, target_ids)` (legacy) or a 4-argument signature
#' `function(action, params, target_ids, ctx)`.
#'
#' @param ctx An ActionContext.
#' @param policy_fn Policy function.
#' @return Updated ActionContext.
#' @export
register_policy <- function(ctx, policy_fn) {
  n <- length(formals(policy_fn))
  if (n == 3L) {
    original <- policy_fn
    policy_fn <- function(action, params, target_ids, ctx) {
      original(action, params, target_ids)
    }
  }
  ctx$policy_fn <- policy_fn
  ctx
}

#' Register an approval gate for actions requiring governance
#'
#' The gate function is called after policy checks, before dispatch. It must
#' return one of `"approved"`, `"denied"`, or `"pending"`:
#' - `"approved"` — proceed with dispatch immediately.
#' - `"denied"` — abort with an error.
#' - `"pending"` — log the action as `pending_approval` and return without
#'   dispatching.
#'
#' @param ctx An ActionContext.
#' @param gate_fn Function with signature `function(action, params, target_ids, ctx)`.
#' @return Updated ActionContext.
#' @export
register_approval_gate <- function(ctx, gate_fn) {
  ctx$gate_fn <- gate_fn
  ctx
}
