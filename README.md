# actionTypesR

Action orchestration and submission lifecycle engine for ontology-defined actions. It
turns ontology action specifications into runnable workflows with built-in resolution,
validation, execution, and logging.

## Features

- **Resolve → validate → execute → record** lifecycle for ontology actions.
- Multiple backends: R, SQL, HTTP, and plugin dispatch.
- Parameter validation with typed schemas (string/integer/number/boolean/date/datetime/json).
- Append-only action log stored in the connected database.
- Policy hooks for authorization checks.

## Installation

```r
# From source
remotes::install_local("path/to/actionTypesR")
```

## Quick start

```r
library(ontologySpecR)
library(actionTypesR)

# Load the aviation ontology bundle
bundle <- read_bundle(system.file("examples", "aviation-demo.json", package = "ontologySpecR"))

# Connect to a database (DuckDB used here for demo)
con <- DBI::dbConnect(duckdb::duckdb())

DBI::dbWriteTable(con, "airports", data.frame(
  airport_id = c("DUB", "JFK"),
  name = c("Dublin", "John F Kennedy"),
  country = c("Ireland", "USA"),
  status = c("active", "active"),
  latitude = c(53.4213, 40.6413),
  longitude = c(-6.2701, -73.7781),
  stringsAsFactors = FALSE
))

# Create the runtime context
ctx <- action_context(bundle, con)

# Register an R handler
ctx <- register_handler(ctx, "UpdateAirportStatus", function(conn, action, params, target_ids) {
  DBI::dbExecute(conn,
    "UPDATE airports SET status = ? WHERE airport_id = ?",
    params = list(params$new_status, target_ids[[1]])
  )
})

# Submit an action
result <- submit_action(ctx, "UpdateAirportStatus",
  params = list(new_status = "maintenance"),
  target_ids = "DUB"
)
result

# Inspect the action log
action_log(ctx)

# Dry run (validates but does not execute)
submit_action(ctx, "UpdateAirportStatus",
  params = list(new_status = "closed"),
  target_ids = "JFK",
  .dry_run = TRUE
)

DBI::dbDisconnect(con, shutdown = TRUE)
```

## Core API

- `action_context(bundle, connection, registry = NULL)`
- `resolve_action(ctx, action_type_id)`
- `validate_params(ctx, action_type_id, params)`
- `submit_action(ctx, action_type_id, params, target_ids = NULL, .dry_run = FALSE)`
- `action_log(ctx, n = 100)`
- `register_handler(ctx, action_type_id, fn)`
- `register_policy(ctx, policy_fn)`

## Vignette

See `vignettes/concept-overview.Rmd` for a full, end-to-end example including
custom actions, dry runs, validation errors, and policy checks.
