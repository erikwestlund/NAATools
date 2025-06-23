#' Validate that a column contains valid ID values
#' 
#' IDs must be non-empty string values. This validator checks that values
#' can be cast to strings and are not empty after trimming whitespace.
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters (not used for this validator)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with invalid ID values
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - not_string_count: Number of values that cannot be cast to string
#'   - empty_count: Number of empty string values
#'   - is_valid: Boolean indicating if all values are valid IDs
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_id <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Build validation query - check for non-string or empty values
  query <- sprintf("
    WITH id_check AS (
      SELECT row_no, %s as value,
        CASE 
          WHEN %s IS NULL THEN 'null'
          WHEN TRY_CAST(%s AS VARCHAR) IS NULL THEN 'not_string'
          WHEN TRIM(CAST(%s AS VARCHAR)) = '' THEN 'empty'
          ELSE 'valid'
        END as status
      FROM %s
    )
    SELECT row_no, CAST(value AS VARCHAR) as value, status
    FROM id_check
    WHERE status != 'valid'
    ORDER BY row_no
  ", var, var, var, var, table_name)
  
  # Define helper functions
  counts_fn <- function(result) {
    if (nrow(result) == 0) {
      return(list(
        null_count = 0,
        not_string_count = 0,
        empty_count = 0
      ))
    }
    
    null_count <- sum(result$status == "null", na.rm = TRUE)
    not_string_count <- sum(result$status == "not_string", na.rm = TRUE)
    empty_count <- sum(result$status == "empty", na.rm = TRUE)
    
    list(
      null_count = null_count,
      not_string_count = not_string_count,
      empty_count = empty_count
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All values in column '%s' are valid IDs", var)
    } else {
      message_parts <- c()
      if (counts$null_count > 0) {
        message_parts <- c(message_parts, sprintf("%d NULL values", counts$null_count))
      }
      if (counts$not_string_count > 0) {
        message_parts <- c(message_parts, sprintf("%d non-string values", counts$not_string_count))
      }
      if (counts$empty_count > 0) {
        message_parts <- c(message_parts, sprintf("%d empty values", counts$empty_count))
      }
      
      sprintf("Column '%s' contains invalid ID values: %s", 
              var, paste(message_parts, collapse = ", "))
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}