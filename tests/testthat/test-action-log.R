library(ontologySpecR)

test_that("action_log records submissions", {
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

  submit_action(ctx, "UpdateAirportStatus", list(new_status = "maintenance"), target_ids = "DUB")

  log <- action_log(ctx)
  expect_true(nrow(log) >= 1)
  expect_true(any(log$action_type_id == "UpdateAirportStatus"))
})
