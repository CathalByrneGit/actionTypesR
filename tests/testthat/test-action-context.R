test_that("action_context indexes actions", {
  skip_if_not_installed("duckdb")
  bundle <- ontologySpecR::spec_example_bundle()
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ctx <- action_context(bundle, con)

  expect_s3_class(ctx, "ActionContext")
  action <- resolve_action(ctx, "UpdateAirportStatus")
  expect_true(is.list(action))
  expect_error(resolve_action(ctx, "MissingAction"), "Unknown action type")
})
