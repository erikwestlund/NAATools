#' Validate that a column contains only allowed enumeration values
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params List containing:
#'   - allowed_values: List of allowed values (can be simple values or list with value/description)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with values outside allowed set
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - invalid_value_count: Number of invalid values
#'   - is_valid: Boolean indicating if all values are in allowed set
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_enum_allowed_values <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Extract allowed values from params
  if (is.null(params$allowed_values)) {
    stop("allowed_values parameter is required for enum validation")
  }
  
  # Process allowed values - handle both simple values and value/description pairs
  allowed_values <- sapply(params$allowed_values, function(v) {
    if (is.list(v) && !is.null(v$value)) {
      as.character(v$value)
    } else {
      as.character(v)
    }
  })
  
  # Create a string list for SQL IN clause
  allowed_values_sql <- paste0("'", gsub("'", "''", allowed_values), "'", collapse = ", ")
  
  # Build validation query - find all values not in allowed set (excluding empty/NULL)
  query <- sprintf("
    SELECT row_no, CAST(%s AS VARCHAR) as value
    FROM %s
    WHERE %s IS NOT NULL 
      AND CAST(%s AS VARCHAR) != ''
      AND CAST(%s AS VARCHAR) NOT IN (%s)
    ORDER BY row_no
  ", var, table_name, var, var, var, allowed_values_sql)
  
  # Define helper functions
  counts_fn <- function(result) {
    if (nrow(result) == 0) {
      return(list(invalid_value_count = 0))
    }
    
    # Count unique invalid values
    invalid_values <- length(unique(result$value))
    
    list(
      invalid_value_count = invalid_values,
      invalid_row_count = nrow(result)
    )
  }
  
  # Format allowed values for display
  format_allowed_values <- function() {
    formatted <- sapply(params$allowed_values, function(v) {
      if (is.list(v) && !is.null(v$value) && !is.null(v$description) && v$value != v$description) {
        sprintf("'%s' (%s)", v$value, v$description)
      } else if (is.list(v) && !is.null(v$value)) {
        sprintf("'%s'", v$value)
      } else {
        sprintf("'%s'", v)
      }
    })
    paste(formatted, collapse = ", ")
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All values in column '%s' are within allowed set", var)
    } else {
      sprintf("Column '%s' contains %d values outside allowed set: [%s]", 
              var, counts$invalid_value_count, format_allowed_values())
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}