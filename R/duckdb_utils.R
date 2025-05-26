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


#' Get database statistics from a DuckDB file
#'
#' @param data_file_path Path to the DuckDB data file
#' @param table_name Name of the table to analyze (default: "data")
#' @return A list containing:
#'   \item{row_count}{Total number of rows}
#'   \item{column_types}{Data frame with column type information}
#'   \item{column_stats}{Data frame with column statistics}
#' @export
get_duckdb_stats <- function(data_file_path, table_name = "data") {
  con <- open_duckdb_connection(data_file_path)
  on.exit(close_duckdb_connection(con))
  
  # Get row count
  row_count <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) as count FROM %s", table_name))$count
  
  # Get column types (already a data frame from PRAGMA)
  column_types <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", table_name))
  rownames(column_types) <- NULL
  
  # Get column statistics and ensure it's a data frame
  column_stats <- data.frame(
    column_name = column_types$name,
    nulls = sapply(column_types$name, function(col) {
      stats <- DBI::dbGetQuery(con, sprintf("
        SELECT COUNT(CASE WHEN %s IS NULL THEN 1 END) as nulls
        FROM %s
      ", col, table_name))
      stats$nulls
    }),
    unique_values = sapply(column_types$name, function(col) {
      stats <- DBI::dbGetQuery(con, sprintf("
        SELECT COUNT(DISTINCT %s) as unique_values
        FROM %s
      ", col, table_name))
      stats$unique_values
    }),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  list(
    row_count = row_count,
    column_types = column_types,
    column_stats = column_stats
  )
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

#' Get total row count from a DuckDB table
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table
#' @return Total number of rows
#' @export
get_duckdb_row_count <- function(duckdb_conn, table_name) {
  count_query <- sprintf("SELECT COUNT(*) as total FROM %s", table_name)
  DBI::dbGetQuery(duckdb_conn, count_query)$total
} 