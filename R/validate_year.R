#' Validate that a column contains valid year values
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params List containing optional parameters:
#'   - min_year: Minimum allowed year (default: 1000)
#'   - max_year: Maximum allowed year (default: current year + 10)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with invalid year values
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - not_numeric_count: Number of non-numeric values
#'   - out_of_range_count: Number of values outside valid year range
#'   - is_valid: Boolean indicating if all values are valid years
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_year <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Set default year range
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  min_year <- ifelse(is.null(params$min_year), 1000, params$min_year)
  max_year <- ifelse(is.null(params$max_year), current_year + 10, params$max_year)
  
  # Build validation query - check for non-year values (excluding NULL and empty)
  query <- sprintf("
    WITH year_check AS (
      SELECT row_no, %s as value,
        CASE 
          WHEN %s IS NULL THEN 'valid'  -- NULL is allowed
          WHEN CAST(%s AS VARCHAR) = '' THEN 'valid'  -- Empty string is allowed
          WHEN TRY_CAST(%s AS INTEGER) IS NULL THEN 'not_numeric'
          WHEN TRY_CAST(%s AS INTEGER) < %d OR TRY_CAST(%s AS INTEGER) > %d THEN 'out_of_range'
          ELSE 'valid'
        END as status
      FROM %s
    )
    SELECT row_no, CAST(value AS VARCHAR) as value, status
    FROM year_check
    WHERE status != 'valid'
    ORDER BY row_no
  ", var, var, var, var, var, min_year, var, max_year, table_name)
  
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
      sprintf("All non-empty values in column '%s' are valid years between %d and %d", 
              var, min_year, max_year)
    } else {
      message_parts <- c()
      if (counts$not_numeric_count > 0) {
        message_parts <- c(message_parts, sprintf("%d non-numeric values", counts$not_numeric_count))
      }
      if (counts$out_of_range_count > 0) {
        message_parts <- c(message_parts, sprintf("%d values outside range [%d-%d]", 
                                                  counts$out_of_range_count, min_year, max_year))
      }
      
      sprintf("Column '%s' contains invalid years: %s", 
              var, paste(message_parts, collapse = ", "))
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}