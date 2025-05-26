#' Validate that a column has no NULL or empty string values
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters (not used for required validation)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of invalid rows (empty if all valid)
#'   - na_count: Number of NULL values
#'   - empty_count: Number of empty string values (for string columns only)
#'   - is_valid: Boolean indicating if all values are valid
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_required <- function(duckdb_conn, table_name = "data", var, params) {
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
  if (grepl("VARCHAR|TEXT|CHAR", col_type, ignore.case = TRUE)) {
    # For string columns, check both NULL and empty strings
    query <- sprintf("
      SELECT row_no, %s as value
      FROM %s
      WHERE %s IS NULL OR TRIM(%s) = ''
    ", var, table_name, var, var)
  } else {
    # For numeric columns, only check NULL
    query <- sprintf("
      SELECT row_no, %s as value
      FROM %s
      WHERE %s IS NULL
    ", var, table_name, var)
  }
  
  result <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Check if we found any invalid rows
  if (nrow(result) > 0) {
    # Count NULL values
    na_count <- sum(is.na(result$value))
    
    # Only count empty strings for string columns
    empty_count <- if (grepl("VARCHAR|TEXT|CHAR", col_type, ignore.case = TRUE)) {
      sum(result$value == "", na.rm = TRUE)
    } else {
      0
    }
    
    counts <- list(
      na_count = na_count,
      empty_count = empty_count
    )
    
    # Construct message based on column type and what was found
    if (grepl("VARCHAR|TEXT|CHAR", col_type, ignore.case = TRUE)) {
      message <- sprintf("Required column '%s' has %d missing values (%d NULL, %d empty)", 
                        var, nrow(result), na_count, empty_count)
    } else {
      message <- sprintf("Required column '%s' has %d NULL values", 
                        var, na_count)
    }
    
    return(create_error_result(
      total_rows = total_rows,
      invalid_rows = result,
      counts = counts,
      message = message
    ))
  }
  
  # All values are present
  counts <- list(
    na_count = 0,
    empty_count = 0
  )
  
  message <- sprintf("All values in required column '%s' are present", var)
  
  return(create_success_result(
    total_rows = total_rows,
    counts = counts,
    message = message
  ))
} 