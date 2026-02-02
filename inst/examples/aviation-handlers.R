update_airport_status <- function(connection, action_type, params, target_ids) {
  DBI::dbExecute(
    connection,
    "UPDATE airports SET status = ? WHERE airport_id = ?",
    params = list(params$new_status, target_ids[[1]])
  )
}

create_flight_route <- function(connection, action_type, params, target_ids) {
  DBI::dbExecute(
    connection,
    "INSERT INTO routes (route_id, origin, destination, status) VALUES (?, ?, ?, ?)",
    params = list(params$route_id, params$origin, params$destination, params$status)
  )
}
