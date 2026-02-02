`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

as_action_list <- function(bundle) {
  if (!is.null(bundle$actions)) {
    return(bundle$actions)
  }
  if (!is.null(bundle$actionTypes)) {
    return(bundle$actionTypes)
  }
  if (!is.null(bundle$action_types)) {
    return(bundle$action_types)
  }
  rlang::abort("Bundle does not contain actions.")
}

get_action_id <- function(action) {
  if (!is.null(action$id)) {
    return(action$id)
  }
  if (!is.null(action$action_id)) {
    return(action$action_id)
  }
  rlang::abort("Action is missing an id field.")
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
    action_type_id = get_action_id(action_type),
    impl_kind = action_type$impl_kind %||% action_type$implementation$kind,
    impl_entrypoint = action_type$impl_entrypoint %||% action_type$implementation$entrypoint,
    params = params,
    target_ids = target_ids
  )
}
