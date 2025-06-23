#' Validate that a column contains values within a specified range
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params List containing:
#'   - min: Minimum allowed value
#'   - max: Maximum allowed value
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with values outside range
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - not_numeric_count: Number of non-numeric values
#'   - out_of_range_count: Number of values outside the specified range
#'   - is_valid: Boolean indicating if all values are within range
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_range <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Validate params
  if (is.null(params$min) || is.null(params$max)) {
    stop("Both 'min' and 'max' parameters are required for range validation")
  }
  
  min_val <- params$min
  max_val <- params$max
  
  # Build validation query - check for values outside range (excluding NULL and empty)
  query <- sprintf("
    WITH range_check AS (
      SELECT row_no, %s as value,
        CASE 
          WHEN %s IS NULL THEN 'valid'  -- NULL is allowed
          WHEN CAST(%s AS VARCHAR) = '' THEN 'valid'  -- Empty string is allowed
          WHEN TRY_CAST(%s AS DOUBLE) IS NULL THEN 'not_numeric'
          WHEN TRY_CAST(%s AS DOUBLE) < %f OR TRY_CAST(%s AS DOUBLE) > %f THEN 'out_of_range'
          ELSE 'valid'
        END as status
      FROM %s
    )
    SELECT row_no, CAST(value AS VARCHAR) as value, status
    FROM range_check
    WHERE status != 'valid'
    ORDER BY row_no
  ", var, var, var, var, var, min_val, var, max_val, table_name)
  
  # Define helper functions
  counts_fn <- function(result) {
    if (nrow(result) == 0) {
      return(list(
        not_numeric_count = 0,
        out_of_range_count = 0
      ))
    }
    
    not_numeric_count <- sum(result$status == "not_numeric", na.rm = TRUE)
    out_of_range_count <- sum(result$status == "out_of_range", na.rm = TRUE)
    
    list(
      not_numeric_count = not_numeric_count,
      out_of_range_count = out_of_range_count
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All non-empty values in column '%s' are within range [%g - %g]", 
              var, min_val, max_val)
    } else {
      message_parts <- c()
      if (counts$not_numeric_count > 0) {
        message_parts <- c(message_parts, sprintf("%d non-numeric values", counts$not_numeric_count))
      }
      if (counts$out_of_range_count > 0) {
        message_parts <- c(message_parts, sprintf("%d values outside range [%g - %g]", 
                                                  counts$out_of_range_count, min_val, max_val))
      }
      
      sprintf("Column '%s' contains invalid values: %s", 
              var, paste(message_parts, collapse = ", "))
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}