#' Validate that a column contains no duplicate values
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params Additional parameters (not used for this validator)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with duplicate values
#'   - invalid_catalog: Catalog of duplicate values and their row numbers
#'   - duplicate_count: Number of duplicate values
#'   - is_valid: Boolean indicating if all values are unique
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_no_duplicates <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Get total row count
  total_rows <- get_duckdb_row_count(duckdb_conn, table_name)
  
  # Build validation query - find all duplicate values excluding empty/NULL
  query <- sprintf("
    WITH duplicate_values AS (
      SELECT %s as value, COUNT(*) as occurrence_count
      FROM %s
      WHERE %s IS NOT NULL AND CAST(%s AS VARCHAR) != ''
      GROUP BY %s
      HAVING COUNT(*) > 1
    ),
    duplicate_rows AS (
      SELECT t.row_no, CAST(t.%s AS VARCHAR) as value
      FROM %s t
      INNER JOIN duplicate_values dv ON t.%s = dv.value
      WHERE t.%s IS NOT NULL AND CAST(t.%s AS VARCHAR) != ''
      ORDER BY t.%s, t.row_no
    )
    SELECT row_no, value
    FROM duplicate_rows
  ", var, table_name, var, var, var, var, table_name, var, var, var, var)
  
  # Define helper functions
  counts_fn <- function(result) {
    if (nrow(result) == 0) {
      return(list(duplicate_count = 0))
    }
    
    # Count unique duplicate values (not total duplicate rows)
    duplicate_values <- length(unique(result$value))
    
    list(
      duplicate_count = duplicate_values,
      duplicate_row_count = nrow(result)
    )
  }
  
  message_fn <- function(result, counts, var) {
    if (nrow(result) == 0) {
      sprintf("All values in column '%s' are unique", var)
    } else {
      sprintf("Column '%s' contains %d duplicate values affecting %d rows", 
              var, counts$duplicate_count, counts$duplicate_row_count)
    }
  }
  
  status_fn <- function(result, counts) {
    if (nrow(result) == 0) "success" else "error"
  }
  
  # Run validation
  run_validation(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows)
}