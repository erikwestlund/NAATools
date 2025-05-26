#' Validate that a column contains valid dates
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters:
#'   - date_format: String format (default: "%Y-%m-%d")
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of invalid rows (empty if all valid)
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - na_count: Number of NULL values
#'   - invalid_date_count: Number of invalid date values
#'   - is_valid: Boolean indicating if all values are valid
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_date <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Get date format from params
  date_format <- params$date_format %||% "%Y-%m-%d"
  
  # Build validation query - only check non-empty values
  query <- sprintf("
    WITH string_check AS (
      SELECT row_no, %s as value,
        CASE 
          WHEN TRY_CAST(%s AS VARCHAR) IS NULL THEN 'not_string'
          WHEN TRY_CAST(%s AS VARCHAR) = '' THEN 'valid'  -- Skip empty strings
          WHEN TRY_CAST(TRY_CAST(%s AS VARCHAR) AS DATE) IS NULL THEN 'invalid_date'
          ELSE 'valid'
        END as status
      FROM %s
      WHERE %s IS NOT NULL  -- Skip NULL values
    )
    SELECT row_no, value, status
    FROM string_check
    WHERE status != 'valid'
  ", var, var, var, var, table_name, var)
  
  # Define helper functions
  counts_fn <- function(result) {
    if (nrow(result) == 0) {
      return(list(
        not_string_count = 0,
        invalid_date_count = 0
      ))
    }
    
    not_string_count <- sum(result$status == "not_string", na.rm = TRUE)
    invalid_date_count <- sum(result$status == "invalid_date", na.rm = TRUE)
    
    list(
      not_string_count = not_string_count,
      invalid_date_count = invalid_date_count
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All non-empty values in column '%s' are valid dates", var)
    } else {
      sprintf("Column '%s' contains %d invalid dates (format: %s) and %d non-string values", 
              var, counts$invalid_date_count, date_format, counts$not_string_count)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
} 