library(ontologySpecR)

test_that("submit_action executes R handlers", {
  skip_if_not_installed("duckdb")
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "airports", data.frame(
    airport_id = "DUB",
    status = "active",
    stringsAsFactors = FALSE
  ))

  ctx <- action_context(bundle, con)
  ctx <- register_handler(ctx, "UpdateAirportStatus", function(connection, action, params, target_ids) {
    DBI::dbExecute(connection, "UPDATE airports SET status = ? WHERE airport_id = ?", params = list(
      params$new_status,
      target_ids[[1]]
    ))
  })

  result <- submit_action(ctx, "UpdateAirportStatus", list(new_status = "maintenance"), target_ids = "DUB")
  expect_s3_class(result, "ActionResult")
  expect_equal(result$status, "success")

  updated <- DBI::dbGetQuery(con, "SELECT status FROM airports WHERE airport_id = 'DUB'")
  expect_equal(updated$status[[1]], "maintenance")
})

test_that("submit_action executes SQL handlers", {
  skip_if_not_installed("duckdb")
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  sql_action <- action_type(
    id = "UpdateAirportStatusSql",
    targets = c("Airport"),
    parameters = list(
      parameter_def(id = "new_status", type = "string", required = TRUE),
      parameter_def(id = "airport_id", type = "string", required = TRUE)
    ),
    effects = list(effect_def(kind = "update", object_type_id = "Airport", notes = "")),
    impl_kind = "sql",
    impl_entrypoint = "UPDATE airports SET status = ? WHERE airport_id = ?",
    policy = list(),
    extensions = list()
  )
  bundle$actions <- c(bundle$actions, list(sql_action))

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "airports", data.frame(
    airport_id = "JFK",
    status = "active",
    stringsAsFactors = FALSE
  ))

  ctx <- action_context(bundle, con)
  result <- submit_action(
    ctx,
    "UpdateAirportStatusSql",
    list(new_status = "closed", airport_id = "JFK")
  )

  expect_equal(result$status, "success")
  updated <- DBI::dbGetQuery(con, "SELECT status FROM airports WHERE airport_id = 'JFK'")
  expect_equal(updated$status[[1]], "closed")
})

test_that("policy rejection halts submission", {
  skip_if_not_installed("duckdb")
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ctx <- action_context(bundle, con)
  ctx <- register_policy(ctx, function(action, params, target_ids) {
    rlang::abort("Policy denied")
  })

  expect_error(
    submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB"),
    "Policy denied"
  )
})

test_that("handler errors are recorded", {
  skip_if_not_installed("duckdb")
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ctx <- action_context(bundle, con)
  ctx <- register_handler(ctx, "UpdateAirportStatus", function(connection, action, params, target_ids) {
    stop("Boom")
  })

  expect_error(
    submit_action(ctx, "UpdateAirportStatus", list(new_status = "closed"), target_ids = "DUB"),
    "Action failed"
  )

  log <- action_log(ctx)
  expect_true(any(log$status == "error"))
})
