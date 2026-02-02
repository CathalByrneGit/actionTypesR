library(ontologySpecR)

test_that("validate_params enforces schema", {
  skip_if_not_installed("duckdb")
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ctx <- action_context(bundle, con)

  expect_true(validate_params(ctx, "UpdateAirportStatus", list(new_status = "closed")))
  expect_error(
    validate_params(ctx, "UpdateAirportStatus", list(wrong_param = "oops")),
    "Unknown parameter"
  )
  expect_error(
    validate_params(ctx, "UpdateAirportStatus", list()),
    "Required parameter"
  )
})
