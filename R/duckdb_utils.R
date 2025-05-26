#' Open a DuckDB connection
#' @param data_file_path Path to the DuckDB file
#' @param read_only Whether to open in read-only mode
#' @return A DuckDB connection
#' @export
open_duckdb_connection <- function(data_file_path, read_only = TRUE) {
  if (!file.exists(data_file_path)) {
    stop(sprintf("DuckDB file not found at: %s", data_file_path))
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = data_file_path, read_only = read_only)
  return(con)
}

#' Close a DuckDB connection
#' @param con The DuckDB connection to close
#' @export
close_duckdb_connection <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

#' Decorator for DuckDB validators
#' @param validator_fn The core validator function (conn, table_name, column_name, ...)
#' @return A function that can take either a connection or a path
#' @export
duckdb_validator_wrapper <- function(validator_fn) {
  function(conn_or_path, column_name, table_name = "data", ...) {
    is_connection <- function(x) inherits(x, "duckdb_connection")
    conn <- NULL
    opened_here <- FALSE
    if (is_connection(conn_or_path)) {
      conn <- conn_or_path
    } else {
      conn <- open_duckdb_connection(conn_or_path)
      opened_here <- TRUE
    }
    
    # Check if table exists
    tables <- DBI::dbListTables(conn)
    if (!(table_name %in% tables)) {
      if (opened_here) close_duckdb_connection(conn)
      return(list(
        status = "fail",
        message = sprintf("Table '%s' not found in database.", table_name)
      ))
    }
    
    # Check if column exists
    table_cols <- DBI::dbListFields(conn, table_name)
    if (!(column_name %in% table_cols)) {
      if (opened_here) close_duckdb_connection(conn)
      return(list(
        status = "fail",
        message = sprintf("Column '%s' not found in table '%s'.", column_name, table_name)
      ))
    }
    
    # Call the core validator
    result <- validator_fn(conn, table_name, column_name, ...)
    
    if (opened_here) close_duckdb_connection(conn)
    return(result)
  }
} 