#' Action result
#'
#' @param submission_id Submission identifier.
#' @param action_type_id Action type identifier.
#' @param status Status string.
#' @param result Handler result.
#' @param error Error message.
#' @param timestamp Completion timestamp.
#' @return ActionResult object.
#' @export
ActionResult <- function(submission_id, action_type_id, status, result = NULL, error = NULL, timestamp = Sys.time()) {
  structure(
    list(
      submission_id = submission_id,
      action_type_id = action_type_id,
      status = status,
      result = result,
      error = error,
      timestamp = timestamp
    ),
    class = "ActionResult"
  )
}

#' @export
print.ActionResult <- function(x, ...) {
  cat("<ActionResult>", x$action_type_id, "[", x$status, "]\n")
  cat("  submission_id:", x$submission_id, "\n")
  cat("  timestamp:", format(x$timestamp), "\n")
  invisible(x)
}
