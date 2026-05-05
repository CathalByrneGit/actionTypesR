library(ontologySpecR)

# ── Helper ────────────────────────────────────────────────────────────────────

make_ctx <- function(con) {
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  action_context(bundle, con)
}

make_ctx_with_handler <- function(con) {
  ctx <- make_ctx(con)
  register_handler(ctx, "UpdateAirportStatus", function(connection, action, params, target_ids) {
    DBI::dbExecute(
      connection,
      "UPDATE airports SET status = ? WHERE airport_id = ?",
      params = list(params$new_status, target_ids[[1]])
    )
  })
}

seed_airports <- function(con) {
  DBI::dbWriteTable(con, "airports", data.frame(
    airport_id = "DUB", status = "active", stringsAsFactors = FALSE
  ))
}

# ── Task 2: action_log table created by action_context() ─────────────────────

test_that("action_context creates action_log table when missing", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_false(DBI::dbExistsTable(con, "action_log"))
  make_ctx(con)
  expect_true(DBI::dbExistsTable(con, "action_log"))
})

# ── Task 3: 3-arg policy backward compat ─────────────────────────────────────

test_that("3-arg policy is wrapped and does not receive ctx argument", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  received_args <- NULL
  ctx <- make_ctx_with_handler(con)
  ctx <- register_policy(ctx, function(action, params, target_ids) {
    received_args <<- list(action = action, params = params, target_ids = target_ids)
  })

  submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB")
  expect_false(is.null(received_args))
  expect_equal(received_args$params$new_status, "closed")
})

test_that("4-arg policy receives ctx as fourth argument", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  received_ctx <- NULL
  ctx <- make_ctx_with_handler(con)
  ctx <- register_policy(ctx, function(action, params, target_ids, ctx) {
    received_ctx <<- ctx
  })

  submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB")
  expect_s3_class(received_ctx, "ActionContext")
})

# ── Task 4: HTTP timeout/retries ──────────────────────────────────────────────

test_that("action_context stores custom http_timeout and http_retries", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  ctx <- action_context(bundle, con, http_timeout = 60L, http_retries = 5L)
  expect_equal(ctx$http_timeout, 60L)
  expect_equal(ctx$http_retries, 5L)
})

test_that("dispatch_http passes timeout and retry to httr2", {
  skip_if_not_installed("httr2")

  timeout_seen  <- NULL
  retries_seen  <- NULL

  testthat::local_mocked_bindings(
    req_timeout = function(req, seconds, ...) { timeout_seen <<- seconds; req },
    req_retry   = function(req, max_tries, ...) { retries_seen <<- max_tries; req },
    req_perform = function(req, ...) {
      structure(list(status_code = 200L, headers = list(), body = raw(0)),
                class = "httr2_response")
    },
    .package = "httr2"
  )

  actionTypesR:::dispatch_http("http://example.com", list(), NULL,
                               timeout_sec = 15L, max_retries = 7L)

  expect_equal(timeout_seen, 15L)
  expect_equal(retries_seen, 7L)
})

# ── Task 5: resubmit_action() ─────────────────────────────────────────────────

test_that("resubmit_action re-executes and logs a new submission_id", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  ctx <- make_ctx_with_handler(con)

  first <- submit_action(ctx, "UpdateAirportStatus",
                         list(new_status = "maintenance"), target_ids = "DUB")
  expect_equal(first$status, "success")

  second <- resubmit_action(ctx, first$submission_id)
  expect_s3_class(second, "ActionResult")
  expect_equal(second$status, "success")
  expect_false(identical(first$submission_id, second$submission_id))

  log <- action_log(ctx)
  expect_true(nrow(log) >= 2L)
})

test_that("resubmit_action errors for unknown submission_id", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ctx <- make_ctx(con)
  expect_error(resubmit_action(ctx, "no-such-id"), "No action found")
})

# ── Task 6: register_approval_gate() ─────────────────────────────────────────

test_that("gate returning 'pending' logs pending_approval without dispatching", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  handler_called <- FALSE
  ctx <- make_ctx(con)
  ctx <- register_handler(ctx, "UpdateAirportStatus",
                          function(connection, action, params, target_ids) {
                            handler_called <<- TRUE
                          })
  ctx <- register_approval_gate(ctx, function(action, params, target_ids, ctx) "pending")

  result <- submit_action(ctx, "UpdateAirportStatus",
                          list(new_status = "closed"), target_ids = "DUB")

  expect_equal(result$status, "pending_approval")
  expect_false(handler_called)

  log <- action_log(ctx)
  expect_true(any(log$status == "pending_approval"))
})

test_that("gate returning 'denied' aborts with error", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  ctx <- make_ctx(con)
  ctx <- register_approval_gate(ctx, function(action, params, target_ids, ctx) "denied")

  expect_error(
    submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB"),
    "denied by approval gate"
  )
})

test_that("gate returning 'approved' allows dispatch to proceed", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  ctx <- make_ctx_with_handler(con)
  ctx <- register_approval_gate(ctx, function(action, params, target_ids, ctx) "approved")

  result <- submit_action(ctx, "UpdateAirportStatus",
                          list(new_status = "closed"), target_ids = "DUB")
  expect_equal(result$status, "success")
})

# ── Task 7: action_summary() ─────────────────────────────────────────────────

test_that("action_summary aggregates by action_type_id and status", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  ctx <- make_ctx_with_handler(con)

  submit_action(ctx, "UpdateAirportStatus", list(new_status = "maintenance"), target_ids = "DUB")
  submit_action(ctx, "UpdateAirportStatus", list(new_status = "active"),      target_ids = "DUB")

  summary <- action_summary(ctx)
  expect_true(is.data.frame(summary))
  expect_true(all(c("action_type_id", "status", "count", "last_submitted_at") %in% names(summary)))

  row <- summary[summary$action_type_id == "UpdateAirportStatus" &
                   summary$status == "success", ]
  expect_true(nrow(row) >= 1L)
  expect_true(row$count >= 2L)
})

test_that("action_summary filters by action_type_id", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  ctx <- make_ctx_with_handler(con)
  submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB")

  summary <- action_summary(ctx, action_type_id = "UpdateAirportStatus")
  expect_true(all(summary$action_type_id == "UpdateAirportStatus"))

  empty <- action_summary(ctx, action_type_id = "NonExistent")
  expect_equal(nrow(empty), 0L)
})

test_that("action_summary filters by since", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  seed_airports(con)

  ctx <- make_ctx_with_handler(con)
  submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB")

  future <- as.POSIXct("2099-01-01", tz = "UTC")
  empty  <- action_summary(ctx, since = future)
  expect_equal(nrow(empty), 0L)
})
