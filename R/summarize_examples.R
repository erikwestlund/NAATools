#' Show example values from a column
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table containing the variable
#' @param var Name of the column to analyze
#' @param params Optional parameters:
#'   \item{null}{Number of examples to show (default: 10)}
#' @return A list containing:
#'   \item{status}{"success" or "error"}
#'   \item{message}{Description of any error}
#'   \item{results}{A list containing:
#'     \item{examples}{Data frame of example values with their counts}
#'   }
#' @export
summarize_examples <- function(duckdb_conn, table_name, var, params = NULL) {
  # Check if column exists
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table", var)
    ))
  }
  
  # Get total rows for percentage calculations
  total_rows <- DBI::dbGetQuery(duckdb_conn, sprintf("SELECT COUNT(*) as count FROM %s", table_name))$count
  
  # Set default number of examples if not provided
  n_examples <- if (!is.null(params$null)) as.integer(params$null) else 10L
  
  # Get example values
  query <- sprintf("
    SELECT 
      %s as value,
      COUNT(*) as count
    FROM %s
    WHERE %s IS NOT NULL
    GROUP BY %s
    ORDER BY count DESC
    LIMIT %d
  ", var, table_name, var, var, n_examples)
  
  # Helper functions
  results_fn <- function(result) {
    # Format examples
    examples <- data.frame(
      value = result$value,
      count = prettyNum(result$count, big.mark = ","),
      pct = sprintf("%.1f%%", result$count / total_rows * 100),
      stringsAsFactors = FALSE
    )
    
    list(examples = examples)
  }
  
  message_fn <- function(result, processed_results, var) {
    NULL  # No message needed for examples
  }
  
  status_fn <- function(result, processed_results) {
    "success"
  }
  
  # Run the summary
  run_summary(duckdb_conn, query, var, "examples", results_fn, message_fn, status_fn, total_rows)
} 