#' Summarize numeric variable values as a histogram
#'
#' @param duckdb_conn DuckDB connection
#' @param table_name Name of the table
#' @param var Name of the variable to summarize
#' @param params Additional parameters (optional list with bins = number of bins)
#'
#' @return List containing status, message, and results with ggplot object
#' @export
summarize_histogram <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Check if column exists
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table '%s'", var, table_name),
      value = NULL
    ))
  }
  
  # Get numeric values only (excluding NULL and non-numeric)
  query <- sprintf("
    SELECT CAST(%s AS DOUBLE) as value
    FROM %s
    WHERE %s IS NOT NULL
      AND TRY_CAST(%s AS DOUBLE) IS NOT NULL
  ", var, table_name, var, var)
  
  values <- DBI::dbGetQuery(duckdb_conn, query)
  
  if (nrow(values) == 0) {
    return(list(
      status = "warn",
      message = "No numeric values found for histogram",
      value = NULL
    ))
  }
  
  # Determine number of bins (default to 30, or use params if provided)
  n_bins <- if (!is.null(params$bins)) params$bins else min(30, ceiling(sqrt(nrow(values))))
  
  # Create the histogram
  p <- ggplot2::ggplot(values, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = n_bins, fill = "steelblue", color = "white", alpha = 0.8) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(labels = scales::comma)
  
  # Get summary statistics for subtitle
  stats_query <- sprintf("
    SELECT 
      MIN(CAST(%s AS DOUBLE)) as min_val,
      MAX(CAST(%s AS DOUBLE)) as max_val,
      AVG(CAST(%s AS DOUBLE)) as mean_val,
      COUNT(*) as n
    FROM %s
    WHERE %s IS NOT NULL
      AND TRY_CAST(%s AS DOUBLE) IS NOT NULL
  ", var, var, var, table_name, var, var)
  
  stats <- DBI::dbGetQuery(duckdb_conn, stats_query)
  
  subtitle <- sprintf("n = %s | Range: [%s, %s] | Mean: %s",
                      format(stats$n, big.mark = ","),
                      format(stats$min_val, big.mark = ",", scientific = FALSE),
                      format(stats$max_val, big.mark = ",", scientific = FALSE),
                      format(round(stats$mean_val, 2), big.mark = ",", scientific = FALSE))
  
  # Apply theme
  p <- NAATools::apply_naa_theme(
    p = p,
    title = "Distribution",
    subtitle = subtitle,
    x_label = "Value",
    y_label = "Count"
  )
  
  # Return results with the plot
  list(
    status = "success",
    message = NULL,
    value = p
  )
}