#' Summarize a numeric variable
#' @param duckdb_conn A DuckDB connection
#' @param table_name The name of the table containing the variable
#' @param var The name of the variable to summarize
#' @param params Optional parameters for the summarizer
#' @return A list containing summary statistics
#' @export
summarize_number <- function(duckdb_conn, table_name, var, params = NULL) {
  # Check if variable exists and is numeric
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table", var)
    ))
  }
  
  # Get column type
  type_query <- sprintf("
    SELECT type 
    FROM pragma_table_info('%s') 
    WHERE name = '%s'
  ", table_name, var)
  col_type <- DBI::dbGetQuery(duckdb_conn, type_query)$type
  
  # Get basic statistics - use TRY_CAST to handle string columns containing numeric values
  stats_query <- sprintf("
    SELECT 
      COUNT(*) as count,
      COUNT(DISTINCT %s) as unique_count,
      COUNT(*) FILTER (WHERE %s IS NULL) as null_count,
      MIN(TRY_CAST(%s AS DOUBLE)) as min_val,
      MAX(TRY_CAST(%s AS DOUBLE)) as max_val,
      AVG(TRY_CAST(%s AS DOUBLE)) as mean_val,
      PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY TRY_CAST(%s AS DOUBLE)) as median_val,
      STDDEV(TRY_CAST(%s AS DOUBLE)) as std_dev
    FROM %s
  ", var, var, var, var, var, var, var, table_name)
  
  stats <- DBI::dbGetQuery(duckdb_conn, stats_query)
  
  # Calculate additional statistics
  range_val <- stats$max_val - stats$min_val
  
  # Calculate outliers using 3 standard deviations
  outliers_query <- sprintf("
    WITH stats AS (
      SELECT 
        AVG(TRY_CAST(%s AS DOUBLE)) as mean_val,
        STDDEV(TRY_CAST(%s AS DOUBLE)) as std_dev
      FROM %s
      WHERE %s IS NOT NULL
        AND TRY_CAST(%s AS DOUBLE) IS NOT NULL
    )
    SELECT TRY_CAST(%s AS DOUBLE) as value
    FROM %s, stats
    WHERE %s IS NOT NULL
      AND TRY_CAST(%s AS DOUBLE) IS NOT NULL
      AND (TRY_CAST(%s AS DOUBLE) < mean_val - 3 * std_dev OR TRY_CAST(%s AS DOUBLE) > mean_val + 3 * std_dev)
    ORDER BY TRY_CAST(%s AS DOUBLE)
    LIMIT 10
  ", var, var, table_name, var, var, var, table_name, var, var, var, var, var)
  
  outliers <- DBI::dbGetQuery(duckdb_conn, outliers_query)
  
  # Return summary results
  list(
    status = "success",
    results = list(
      count = stats$count,
      unique_count = stats$unique_count,
      null_count = stats$null_count,
      null_percentage = (stats$null_count / stats$count) * 100,
      min = stats$min_val,
      max = stats$max_val,
      mean = stats$mean_val,
      median = stats$median_val,
      std_dev = stats$std_dev,
      range = range_val,
      outliers = if (nrow(outliers) > 0) outliers$value else NULL
    )
  )
}

#' Format number summary results for display
#' @param summary_result The result from summarize_number
#' @param db_stats Database statistics for context
#' @return A formatted summary result
#' @export
format_number_summary <- function(summary_result, db_stats) {
  if (summary_result$status == "error") {
    return(list(
      status = "error",
      message = summary_result$message
    ))
  }
  
  results <- summary_result$results
  
  # Create a data frame for display
  summary_df <- data.frame(
    Metric = c(
      "Count",
      "Unique Values",
      "Missing Values",
      "Missing (%)",
      "Minimum",
      "Maximum",
      "Mean",
      "Median",
      "Standard Deviation",
      "Range"
    ),
    Value = c(
      format(results$count, big.mark = ","),
      format(results$unique_count, big.mark = ","),
      format(results$null_count, big.mark = ","),
      sprintf("%.1f%%", results$null_percentage),
      format(results$min, digits = 2),
      format(results$max, digits = 2),
      format(results$mean, digits = 2),
      format(results$median, digits = 2),
      format(results$std_dev, digits = 2),
      format(results$range, digits = 2)
    ),
    stringsAsFactors = FALSE
  )
  
  # Add outliers if present
  if (!is.null(results$outliers)) {
    summary_df <- rbind(summary_df, data.frame(
      Metric = "Potential Outliers",
      Value = paste(results$outliers, collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }
  
  list(
    status = "success",
    summary_table = summary_df
  )
}
