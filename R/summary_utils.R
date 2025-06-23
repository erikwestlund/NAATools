#' Helper function to run summary operations
#' @param duckdb_conn DuckDB connection object
#' @param query SQL query to execute
#' @param var Name of the variable being summarized
#' @param summarizer_name Name of the summarizer function
#' @param results_fn Function to process the query results
#' @param message_fn Function to generate summary message
#' @param status_fn Function to determine summary status
#' @param total_rows Total number of rows in the table
#' @return A list containing the summary results
run_summary <- function(duckdb_conn, query, var, summarizer_name, results_fn, message_fn, status_fn, total_rows) {
  # Run query and get results
  result <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Process results
  processed_results <- results_fn(result)
  
  # Get status and message
  status <- status_fn(result, processed_results)
  message <- message_fn(result, processed_results, var)
  
  # Format the summary result
  format_summary_result(
    summarizer_name = summarizer_name,
    status = status,
    message = message,
    value = processed_results
  )
}

#' Format summary results for display
#' @param summarizer_name Name of the summarizer
#' @param status Status of the summary ("success" or "error")
#' @param message Optional message about the summary
#' @param value The summary value
#' @param value_rendered Optional rendered version of the value
#' @return A list containing the formatted summary results
#' @export
format_summary_result <- function(summarizer_name, status, message, value, value_rendered = NULL) {
  # Create the result list
  result <- list(
    summarizer = summarizer_name,
    status = status,
    message = message,
    value = value
  )
  
  # Add rendered value if provided
  if (!is.null(value_rendered)) {
    result$value_rendered <- value_rendered
  }
  
  # Add class for custom printing
  class(result) <- c("summary_result", "list")
  
  result
} 