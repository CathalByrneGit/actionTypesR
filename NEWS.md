# actionTypesR 0.2.0

## Breaking changes

* ontologySpecR (>= 0.2.0) moved from `Depends` to `Imports`; it is no longer
  attached when actionTypesR is loaded. Call `library(ontologySpecR)` yourself
  if you use its constructors.
* `action_context()` now runs `ontologySpecR::as_bundle()` on its input and
  reads actions through `bundle_action_types()` / `bundle_action_type()`.
  actionTypesR no longer guesses legacy bundle keys (`actionTypes`,
  `action_types`, `action_id`, flat `impl_kind` / `impl_entrypoint`);
  `as_bundle()` maps legacy keys (with a warning) instead.
* `ctx$actions` (the id-indexed action list) is removed. Use
  `resolve_action(ctx, id)` or `ontologySpecR::bundle_action_types(ctx$bundle)`.
* `resolve_action()` errors now come from ontologySpecR and list the valid ids.

## New features

* `register_post_submit_hook(ctx, fn)`: `fn(action_type_id, target_ids,
  submission_id)` runs after each successful, logged submission (including
  `resubmit_action()`). Failing hooks produce a warning and a `hook_error` row in
  `action_log`; they never fail the submission. This is the hook machineryR's
  `mac_wire_actions()` looks for.

## Earlier in this cycle

* Action log table is `action_log`, created by `action_context()`.
* Policy functions receive `ctx` as a fourth argument (3-argument policies are
  still accepted).
* `register_approval_gate()`, `resubmit_action()`, `action_summary()`, and HTTP
  timeout / retry settings on the context.
