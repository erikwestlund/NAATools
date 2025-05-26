#' Validate that a column has values, but only warn if missing
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters including:
#'   - message: Custom warning message (optional)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of invalid rows (empty if all valid)
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - na_count: Number of NULL values
#'   - empty_count: Number of empty string values (for string columns only)
#'   - is_valid: Boolean indicating if all values are valid
#'   - message: Description of validation result
#'   - status: "warn" if missing values, "success" if all present
#' @export
validate_recommended <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Get column type
  type_query <- sprintf("
    SELECT type 
    FROM pragma_table_info('%s') 
    WHERE name = '%s'
  ", table_name, var)
  col_type <- DBI::dbGetQuery(duckdb_conn, type_query)$type
  
  # Build query based on column type
  query <- if (grepl("VARCHAR|TEXT|CHAR", col_type, ignore.case = TRUE)) {
    sprintf("
      SELECT row_no, %s as value
      FROM %s
      WHERE %s IS NULL OR TRIM(%s) = ''
    ", var, table_name, var, var)
  } else {
    sprintf("
      SELECT row_no, %s as value
      FROM %s
      WHERE %s IS NULL
    ", var, table_name, var)
  }
  
  # Define helper functions
  counts_fn <- function(result) {
    na_count <- if (nrow(result) > 0) sum(is.na(result$value)) else 0
    empty_count <- if (nrow(result) > 0 && grepl("VARCHAR|TEXT|CHAR", col_type, ignore.case = TRUE)) {
      sum(result$value == "", na.rm = TRUE)
    } else {
      0
    }
    list(
      na_count = na_count,
      empty_count = empty_count
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (!is.null(params$message)) {
      params$message
    } else if (nrow(result) == 0) {
      sprintf("All recommended values in column '%s' are present", var)
    } else if (grepl("VARCHAR|TEXT|CHAR", col_type, ignore.case = TRUE)) {
      sprintf("Recommended column '%s' has %d missing values (%d NULL, %d empty)", 
              var, nrow(result), counts$na_count, counts$empty_count)
    } else {
      sprintf("Recommended column '%s' has %d NULL values", 
              var, counts$na_count)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "warn"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
} 