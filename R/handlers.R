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

#' Register a post-submit hook
#'
#' Post-submit hooks run after an action has been dispatched successfully and
#' recorded in the action log. They are not run for dry runs, for submissions
#' rejected by a policy or approval gate, for submissions left
#' `pending_approval`, or for actions whose handler failed.
#'
#' Each hook is called as `fn(action_type_id, target_ids, submission_id)`, where
#' `target_ids` is a character vector (empty when the action had no targets).
#' This is the signature machineryR's `mac_wire_actions()` registers.
#'
#' A hook that errors does not fail the submission: the error is surfaced as a
#' warning and recorded in the action log as a `hook_error` row sharing the
#' submission's `submission_id`.
#'
#' @param ctx An ActionContext.
#' @param fn Function with signature `function(action_type_id, target_ids, submission_id)`.
#' @return Updated ActionContext.
#' @export
register_post_submit_hook <- function(ctx, fn) {
  if (!is.function(fn)) {
    rlang::abort("`fn` must be a function.")
  }
  ctx$post_submit_hooks <- c(ctx$post_submit_hooks %||% list(), list(fn))
  ctx
}
