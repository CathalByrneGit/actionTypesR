library(ontologySpecR)

test_that("dry run returns plan without execution", {
  skip_if_not_installed("duckdb")
  bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ctx <- action_context(bundle, con)
  plan <- submit_action(
    ctx,
    "UpdateAirportStatus",
    list(new_status = "closed"),
    target_ids = "JFK",
    .dry_run = TRUE
  )

  expect_true(inherits(plan, "ActionPlan"))
  log <- action_log(ctx)
  expect_equal(nrow(log), 0)
})
