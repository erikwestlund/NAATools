#' Validate that a column contains valid integer values
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters (not used for this validator)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with non-integer values
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - invalid_integer_count: Number of non-integer values
#'   - is_valid: Boolean indicating if all values are integers
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_integer <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Build validation query - check for non-integer values (excluding NULL and empty)
  query <- sprintf("
    WITH integer_check AS (
      SELECT row_no, %s as value,
        CASE 
          WHEN %s IS NULL THEN 'valid'  -- NULL is allowed
          WHEN CAST(%s AS VARCHAR) = '' THEN 'valid'  -- Empty string is allowed
          WHEN TRY_CAST(%s AS BIGINT) IS NULL THEN 'not_integer'
          -- Check if it's a whole number (no decimal part)
          WHEN TRY_CAST(%s AS DOUBLE) IS NOT NULL 
               AND TRY_CAST(%s AS DOUBLE) != ROUND(TRY_CAST(%s AS DOUBLE)) THEN 'not_integer'
          ELSE 'valid'
        END as status
      FROM %s
    )
    SELECT row_no, CAST(value AS VARCHAR) as value
    FROM integer_check
    WHERE status = 'not_integer'
    ORDER BY row_no
  ", var, var, var, var, var, var, var, table_name)
  
  # Define helper functions
  counts_fn <- function(result) {
    list(
      invalid_integer_count = nrow(result)
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All non-empty values in column '%s' are valid integers", var)
    } else {
      sprintf("Column '%s' contains %d non-integer values", 
              var, counts$invalid_integer_count)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}