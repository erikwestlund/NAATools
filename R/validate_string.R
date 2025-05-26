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
#'   - na_count: Number of NULL values
#'   - non_string_count: Number of non-string values
#'   - is_valid: Boolean indicating if all values are valid
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_string <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Use DuckDB's TRY_CAST to attempt string conversion
  # This will return NULL for any values that can't be cast to string
  query <- sprintf("
    SELECT row_no, %s as value
    FROM %s
    WHERE TRY_CAST(%s AS VARCHAR) IS NULL
  ", var, table_name, var)
  
  result <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Check if we found any invalid rows
  if (nrow(result) > 0) {
    # Count NA values separately
    na_count <- sum(is.na(result$value))
    non_string_count <- nrow(result) - na_count
    
    counts <- list(
      na_count = na_count,
      non_string_count = non_string_count
    )
    
    # Construct message based on what was found
    message <- sprintf("Column '%s' contains %d non-string values and %d NULL values", 
                      var, non_string_count, na_count)
    
    return(create_error_result(
      total_rows = total_rows,
      invalid_rows = result,
      counts = counts,
      message = message
    ))
  }
  
  # All values are valid strings
  counts <- list(
    na_count = 0,
    non_string_count = 0
  )
  
  message <- sprintf("All values in column '%s' are valid strings", var)
  
  return(create_success_result(
    total_rows = total_rows,
    counts = counts,
    message = message
  ))
} 