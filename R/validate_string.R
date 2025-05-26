#' Validate that a column contains only string values
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters (not used for string validation)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of invalid rows (empty if all valid)
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - non_string_count: Number of non-string values
#'   - is_valid: Boolean indicating if all values are valid
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_string <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Build validation query - only check non-empty values
  query <- sprintf("
    SELECT row_no, %s as value
    FROM %s
    WHERE %s IS NOT NULL  -- Skip NULL values
    AND TRY_CAST(%s AS VARCHAR) IS NULL  -- Only check non-string values
  ", var, table_name, var, var)
  
  # Define helper functions
  counts_fn <- function(result) {
    non_string_count <- if (nrow(result) > 0) nrow(result) else 0
    list(
      non_string_count = non_string_count
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All non-empty values in column '%s' are valid strings", var)
    } else {
      sprintf("Column '%s' contains %d non-string values", 
              var, counts$non_string_count)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
} 