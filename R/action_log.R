ensure_action_log_table <- function(connection) {
  if (!DBI::dbExistsTable(connection, "action_log")) {
    DBI::dbExecute(
      connection,
      "CREATE TABLE action_log (
        submission_id   TEXT      NOT NULL,
        action_type_id  TEXT      NOT NULL,
        params_json     TEXT,
        target_ids_json TEXT,
        status          TEXT      NOT NULL,
        error_message   TEXT,
        submitted_at    TIMESTAMP NOT NULL,
        completed_at    TIMESTAMP,
        submitted_by    TEXT
      )"
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
    "SELECT * FROM action_log ORDER BY submitted_at DESC LIMIT ?",
    params = list(n)
  )
}

#' Summarise action log entries by action type and status
#'
#' @param ctx An ActionContext.
#' @param action_type_id Optional action type to filter by.
#' @param since Optional POSIXct; only include submissions at or after this time.
#' @return A data frame: action_type_id, status, count, last_submitted_at.
#' @export
action_summary <- function(ctx, action_type_id = NULL, since = NULL) {
  ensure_action_log_table(ctx$connection)

  conditions <- character(0)
  query_params <- list()

  if (!is.null(action_type_id)) {
    conditions <- c(conditions, "action_type_id = ?")
    query_params[[length(query_params) + 1L]] <- action_type_id
  }
  if (!is.null(since)) {
    conditions <- c(conditions, "submitted_at >= ?")
    query_params[[length(query_params) + 1L]] <- since
  }

  where_clause <- if (length(conditions) > 0L) {
    paste("WHERE", paste(conditions, collapse = " AND "))
  } else {
    ""
  }

  query <- paste(
    "SELECT action_type_id, status, COUNT(*) AS count,",
    "MAX(submitted_at) AS last_submitted_at",
    "FROM action_log",
    where_clause,
    "GROUP BY action_type_id, status",
    "ORDER BY action_type_id, status"
  )

  if (length(query_params) > 0L) {
    DBI::dbGetQuery(ctx$connection, query, params = query_params)
  } else {
    DBI::dbGetQuery(ctx$connection, query)
  }
}

record_action_log <- function(connection, submission) {
  ensure_action_log_table(connection)
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO action_log",
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
