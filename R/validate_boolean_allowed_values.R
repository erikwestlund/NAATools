#' Validate that a column contains only allowed boolean representations
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params List containing:
#'   - True: List of values representing TRUE (e.g., c("1", "Y", "Yes"))
#'   - False: List of values representing FALSE (e.g., c("0", "N", "No"))
#'   - Unknown: List of values representing UNKNOWN/NA (e.g., c("U", "Unknown"))
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with values outside allowed sets
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - invalid_value_count: Number of invalid values
#'   - is_valid: Boolean indicating if all values are in allowed sets
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_boolean_allowed_values <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Extract allowed values from params
  true_values <- as.character(params$`True` %||% params$true %||% c())
  false_values <- as.character(params$`False` %||% params$false %||% c())
  unknown_values <- as.character(params$Unknown %||% params$unknown %||% c())
  
  # Combine all allowed values
  all_allowed_values <- c(true_values, false_values, unknown_values)
  
  if (length(all_allowed_values) == 0) {
    stop("At least one of 'True', 'False', or 'Unknown' must be specified in params")
  }
  
  # Create a string list for SQL IN clause
  allowed_values_sql <- paste0("'", gsub("'", "''", all_allowed_values), "'", collapse = ", ")
  
  # Build validation query - find all values not in allowed sets (excluding empty/NULL)
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
    parts <- c()
    
    if (length(true_values) > 0) {
      parts <- c(parts, sprintf("True: [%s]", 
                               paste0("'", true_values, "'", collapse = ", ")))
    }
    if (length(false_values) > 0) {
      parts <- c(parts, sprintf("False: [%s]", 
                               paste0("'", false_values, "'", collapse = ", ")))
    }
    if (length(unknown_values) > 0) {
      parts <- c(parts, sprintf("Unknown: [%s]", 
                               paste0("'", unknown_values, "'", collapse = ", ")))
    }
    
    paste0("{ ", paste(parts, collapse = ", "), " }")
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All values in column '%s' are within allowed boolean representations", var)
    } else {
      sprintf("Column '%s' contains %d values outside allowed set: %s", 
              var, counts$invalid_value_count, format_allowed_values())
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}