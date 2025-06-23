#' Calculate basic statistics for a column
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table containing the variable
#' @param var Name of the column to analyze
#' @param params Optional parameters (not used for basic stats)
#' @return A list containing:
#'   \item{status}{"success" or "error"}
#'   \item{message}{Description of any error}
#'   \item{results}{A list containing:
#'     \item{count}{Total non-null values}
#'     \item{unique}{Number of unique values}
#'     \item{empty}{Number of NULL values}
#'     \item{present}{Number of non-NULL values}
#'     \item{empty_pct}{Percentage of NULL values}
#'   }
#' @export
summarize_basic_stats <- function(duckdb_conn, table_name, var, params = NULL) {
  # Check if column exists
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table", var)
    ))
  }
  
  # Get total rows for percentage calculations
  total_rows <- DBI::dbGetQuery(duckdb_conn, sprintf("SELECT COUNT(*) as count FROM %s", table_name))$count
  
  # Get all basic stats in one query
  query <- sprintf("
    SELECT 
      COUNT(*) as total,
      COUNT(%s) as present,
      COUNT(*) - COUNT(%s) as empty,
      COUNT(DISTINCT %s) as unique
    FROM %s
  ", var, var, var, table_name)
  
  # Helper functions
  results_fn <- function(result) {
    # Calculate empty percentage
    empty_pct <- if (result$total > 0) {
      round(result$empty / result$total * 100, 1)
    } else {
      0
    }
    
    # Format all numbers with commas
    list(
      count = prettyNum(result$present, big.mark = ","),
      unique = prettyNum(result$unique, big.mark = ","),
      empty = prettyNum(result$empty, big.mark = ","),
      present = prettyNum(result$present, big.mark = ","),
      empty_pct = sprintf("%.1f%%", empty_pct)
    )
  }
  
  message_fn <- function(result, processed_results, var) {
    NULL  # No message needed for basic stats
  }
  
  status_fn <- function(result, processed_results) {
    "success"
  }
  
  # Run the summary
  run_summary(duckdb_conn, query, var, "basic_stats", results_fn, message_fn, status_fn, total_rows)
} 