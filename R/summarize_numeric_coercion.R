#' Analyze what percentage of string values can be coerced to numbers
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table containing the variable
#' @param var Name of the column to analyze
#' @param params Optional parameters (not used for numeric coercion analysis)
#' @return A list containing:
#'   \item{status}{"success" or "error"}
#'   \item{message}{Description of any error}
#'   \item{results}{A list containing:
#'     \item{coercion_stats}{Statistics about numeric coercion:
#'       \item{can_coerce}{Percentage of values that can be coerced to numbers}
#'       \item{cannot_coerce}{Percentage of values that cannot be coerced to numbers}
#'       \item{description}{Explanation of the coercion statistics}
#'     }
#'   }
#' @export
summarize_numeric_coercion <- function(duckdb_conn, table_name, var, params = NULL) {
  # Check if column exists
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table", var)
    ))
  }
  
  # Get total rows for percentage calculations
  total_rows <- DBI::dbGetQuery(duckdb_conn, sprintf("SELECT COUNT(*) as count FROM %s", table_name))$count
  
  # Get coercion statistics
  query <- sprintf("
    WITH stats AS (
      SELECT 
        COUNT(*) as total,
        COUNT(CASE WHEN TRY_CAST(%s AS DOUBLE) IS NOT NULL THEN 1 END) as can_coerce
      FROM %s
      WHERE %s IS NOT NULL
    )
    SELECT 
      total,
      can_coerce,
      total - can_coerce as cannot_coerce
    FROM stats
  ", var, table_name, var)
  
  # Helper functions
  results_fn <- function(result) {
    # Calculate percentages
    total <- result$total[1]
    can_coerce_pct <- round(result$can_coerce[1] / total * 100, 1)
    cannot_coerce_pct <- round(result$cannot_coerce[1] / total * 100, 1)
    
    # Format coercion statistics
    coercion_stats <- list(
      can_coerce = sprintf("%.1f%%", can_coerce_pct),
      cannot_coerce = sprintf("%.1f%%", cannot_coerce_pct),
      description = "These percentages show how many values can or cannot be converted to numbers. Values that can be coerced include integers, decimals, and scientific notation."
    )
    
    list(coercion_stats = coercion_stats)
  }
  
  message_fn <- function(result, processed_results, var) {
    NULL  # No message needed for coercion stats
  }
  
  status_fn <- function(result, processed_results) {
    "success"
  }
  
  # Run the summary
  run_summary(duckdb_conn, query, var, "numeric_coercion", results_fn, message_fn, status_fn, total_rows)
} 