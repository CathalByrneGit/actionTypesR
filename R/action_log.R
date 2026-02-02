ensure_action_log_table <- function(connection) {
  if (!DBI::dbExistsTable(connection, "_action_log")) {
    DBI::dbExecute(
      connection,
      paste(
        "CREATE TABLE _action_log (",
        "submission_id TEXT,",
        "action_type_id TEXT,",
        "params_json TEXT,",
        "target_ids_json TEXT,",
        "status TEXT,",
        "error_message TEXT,",
        "submitted_at TIMESTAMP,",
        "completed_at TIMESTAMP,",
        "submitted_by TEXT",
        ")"
      )
    )
  }
}

#' Retrieve action submissions from the log
#'
#' @param ctx An ActionContext.
#' @param n Number of records to return.
#' @return A data frame of action submissions.
#' @export
action_log <- function(ctx, n = 100) {
  ensure_action_log_table(ctx$connection)
  DBI::dbGetQuery(
    ctx$connection,
    "SELECT * FROM _action_log ORDER BY submitted_at DESC LIMIT ?",
    params = list(n)
  )
}

record_action_log <- function(connection, submission) {
  ensure_action_log_table(connection)
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO _action_log",
      "(submission_id, action_type_id, params_json, target_ids_json, status,",
      "error_message, submitted_at, completed_at, submitted_by)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      submission$submission_id,
      submission$action_type_id,
      submission$params_json,
      submission$target_ids_json,
      submission$status,
      submission$error_message,
      submission$submitted_at,
      submission$completed_at,
      submission$submitted_by
    )
  )
}
