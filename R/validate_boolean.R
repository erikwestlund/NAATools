#' Validate that a column contains valid boolean values
#' 
#' Valid boolean values are: TRUE, FALSE, T, F, 1, 0, "true", "false", "yes", "no"
#' (case insensitive for string values)
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters (not used for this validator)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with non-boolean values
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - invalid_boolean_count: Number of non-boolean values
#'   - is_valid: Boolean indicating if all values are valid booleans
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_boolean <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Build validation query - check for non-boolean values (excluding NULL and empty)
  query <- sprintf("
    WITH boolean_check AS (
      SELECT row_no, %s as value,
        CASE 
          WHEN %s IS NULL THEN 'valid'  -- NULL is allowed
          WHEN CAST(%s AS VARCHAR) = '' THEN 'valid'  -- Empty string is allowed
          WHEN UPPER(CAST(%s AS VARCHAR)) IN ('TRUE', 'FALSE', 'T', 'F', '1', '0', 'YES', 'NO', 'Y', 'N') THEN 'valid'
          WHEN TRY_CAST(%s AS BOOLEAN) IS NOT NULL THEN 'valid'
          ELSE 'not_boolean'
        END as status
      FROM %s
    )
    SELECT row_no, CAST(value AS VARCHAR) as value
    FROM boolean_check
    WHERE status = 'not_boolean'
    ORDER BY row_no
  ", var, var, var, var, var, table_name)
  
  # Define helper functions
  counts_fn <- function(result) {
    list(
      invalid_boolean_count = nrow(result)
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All non-empty values in column '%s' are valid boolean values", var)
    } else {
      sprintf("Column '%s' contains %d non-boolean values", 
              var, counts$invalid_boolean_count)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}