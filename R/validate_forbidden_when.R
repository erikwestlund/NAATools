#' Validate that a column is forbidden when another column meets certain conditions
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params List containing one of:
#'   - absent: Column name - value is forbidden when this column is absent/empty
#'   - present: Column name - value is forbidden when this column is present/non-empty
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows where forbidden condition is violated
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - forbidden_value_count: Number of forbidden values present
#'   - is_valid: Boolean indicating if all conditional restrictions are met
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_forbidden_when <- function(duckdb_conn, table_name = "data", var, params) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Validate params
  absent <- params$absent
  present <- params$present
  
  if ((is.null(absent) && is.null(present)) || (!is.null(absent) && !is.null(present))) {
    stop("Exactly one of 'absent' or 'present' must be provided in params")
  }
  
  # Build validation query based on condition type
  if (!is.null(absent)) {
    # Value is forbidden when 'absent' column is NULL or empty
    query <- sprintf("
      SELECT row_no, 
             CAST(%s AS VARCHAR) as value,
             CAST(%s AS VARCHAR) as condition_value
      FROM %s
      WHERE (%s IS NULL OR CAST(%s AS VARCHAR) = '')  -- Condition column is absent/empty
        AND %s IS NOT NULL 
        AND CAST(%s AS VARCHAR) != ''  -- But target column has a value (forbidden)
      ORDER BY row_no
    ", var, absent, table_name, absent, absent, var, var)
    
    condition_desc <- sprintf("when '%s' is absent", absent)
  } else {
    # Value is forbidden when 'present' column is NOT NULL and NOT empty
    query <- sprintf("
      SELECT row_no, 
             CAST(%s AS VARCHAR) as value,
             CAST(%s AS VARCHAR) as condition_value
      FROM %s
      WHERE %s IS NOT NULL 
        AND CAST(%s AS VARCHAR) != ''  -- Condition column is present
        AND %s IS NOT NULL 
        AND CAST(%s AS VARCHAR) != ''  -- But target column also has a value (forbidden)
      ORDER BY row_no
    ", var, present, table_name, present, present, var, var)
    
    condition_desc <- sprintf("when '%s' is present", present)
  }
  
  # Define helper functions
  counts_fn <- function(result) {
    list(
      forbidden_value_count = nrow(result)
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("Column '%s' meets all conditional restrictions", var)
    } else {
      sprintf("Column '%s' has values in %d rows where they are forbidden %s", 
              var, counts$forbidden_value_count, condition_desc)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}