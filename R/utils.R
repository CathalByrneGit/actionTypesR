`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_target_ids <- function(target_ids) {
  if (is.null(target_ids)) {
    return(NULL)
  }
  if (is.character(target_ids)) {
    return(as.list(target_ids))
  }
  if (is.list(target_ids)) {
    return(target_ids)
  }
  return(as.list(target_ids))
}

generate_uuid <- function() {
  if (requireNamespace("uuid", quietly = TRUE)) {
    return(uuid::UUIDgenerate())
  }
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample(100000:999999, 1))
}

na_if_null <- function(value, default = NA) {
  if (is.null(value)) {
    return(default)
  }
  value
}

as_action_plan <- function(action_type, params, target_ids) {
  list(
    action_type_id = action_type$id,
    impl_kind = action_type$implementation$kind,
    impl_entrypoint = action_type$implementation$entrypoint,
    params = params,
    target_ids = target_ids
  )
}
