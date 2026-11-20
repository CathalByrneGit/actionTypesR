# actionTypesR

Action orchestration and submission lifecycle engine for ontology-defined actions. It
turns ontology action specifications into runnable workflows with built-in resolution,
validation, execution, and logging.

## Features

- **Resolve → validate → execute → record** lifecycle for ontology actions.
- Multiple backends: R, SQL, HTTP, and plugin dispatch.
- Parameter validation with typed schemas (string/integer/number/boolean/date/datetime/json).
- Append-only `action_log` table created automatically in the connected database.
- Policy hooks for authorization checks (3-arg legacy form supported).
- Approval gate hook for governance integration (approved / denied / pending).
- HTTP dispatch with configurable timeout and retry.
- Post-submit hooks (used by machineryR to fire action-triggered transitions).
- Replay any logged submission with `resubmit_action()`.
- Aggregated log summary via `action_summary()`.

## Installation

```r
# From source
remotes::install_local("path/to/actionTypesR")
```

## Quick start

```r
library(ontologySpecR)
library(actionTypesR)

# Load the aviation ontology bundle (any ontologySpecR bundle, list or JSON path works)
bundle <- spec_example_bundle()

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

**Context**
- `action_context(bundle, connection, registry = NULL, http_timeout = 30L, http_retries = 3L)`
- `resolve_action(ctx, action_type_id)`
- `validate_params(ctx, action_type_id, params)`

**Submission**
- `submit_action(ctx, action_type_id, params, target_ids = NULL, .dry_run = FALSE)`
- `resubmit_action(ctx, submission_id)`

**Log**
- `action_log(ctx, n = 100)`
- `action_summary(ctx, action_type_id = NULL, since = NULL)`

**Hooks**
- `register_handler(ctx, action_type_id, fn)`
- `register_policy(ctx, policy_fn)` — 3-arg `(action, params, target_ids)` or 4-arg `(..., ctx)`
- `register_approval_gate(ctx, gate_fn)` — returns `"approved"`, `"denied"`, or `"pending"`
- `register_post_submit_hook(ctx, fn)` — `fn(action_type_id, target_ids, submission_id)`

## Approval gate

An approval gate lets an external system (e.g. `auditR`) intercept actions
before dispatch. Register a gate function that returns `"approved"`,
`"denied"`, or `"pending"`:

```r
ctx <- register_approval_gate(ctx, function(action, params, target_ids, ctx) {
  if (action$id == "UpdateAirportStatus" && params$new_status == "closed") {
    return("pending")   # queue for human review
  }
  "approved"
})

result <- submit_action(ctx, "UpdateAirportStatus",
  params = list(new_status = "closed"),
  target_ids = "DUB"
)
result$status  # "pending_approval"
```

## Post-submit hooks

Hooks run after an action has been dispatched and logged successfully. This is
how machineryR's `mac_wire_actions()` fires action-triggered transitions.

```r
ctx <- register_post_submit_hook(ctx, function(action_type_id, target_ids, submission_id) {
  message("Submitted ", action_type_id, " for ", paste(target_ids, collapse = ", "))
})
```

- `target_ids` is a character vector (empty if the action had no targets).
- Hooks do **not** run for dry runs, handler errors, policy/gate denials, or
  actions left `pending_approval`. `resubmit_action()` does run them.
- A hook that errors never fails the submission: it raises a warning and adds a
  `hook_error` row (same `submission_id`) to `action_log`.

## Resubmitting logged actions

```r
# Original submission
first <- submit_action(ctx, "UpdateAirportStatus",
  params = list(new_status = "maintenance"),
  target_ids = "DUB"
)

# Replay with a fresh submission_id
second <- resubmit_action(ctx, first$submission_id)
```

## Log summary

```r
action_summary(ctx)
# action_type_id      status  count  last_submitted_at
# UpdateAirportStatus success     2  2024-01-15 10:30:00

action_summary(ctx, action_type_id = "UpdateAirportStatus",
               since = as.POSIXct("2024-01-01"))
```

## Vignette

See `vignettes/concept-overview.Rmd` for a full, end-to-end example including
custom actions, dry runs, validation errors, policy checks, and approval gates.
