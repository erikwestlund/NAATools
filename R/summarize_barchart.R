#' Summarize variable values as a bar chart
#'
#' @param duckdb_conn DuckDB connection
#' @param table_name Name of the table
#' @param var Name of the variable to summarize
#' @param params Additional parameters (not used)
#'
#' @return List containing status, message, and results with ggplot object
#' @export
summarize_barchart <- function(duckdb_conn, table_name = "data", var, params = NULL) {
  # Get all value counts
  query <- sprintf("
    SELECT %s as value, COUNT(*) as count
    FROM %s
    WHERE %s IS NOT NULL
    GROUP BY %s
    ORDER BY count DESC
  ", var, table_name, var, var)
  
  all_counts <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Calculate total count for percentages
  total_count <- sum(all_counts$count)
  
  # Get top 9 values
  top_values <- head(all_counts, 9)
  
  # Add percentage column
  top_values$percentage <- top_values$count/total_count * 100
  
  # Only create "Other" category if we have more than 9 values
  if (nrow(all_counts) > 9) {
    # Get remaining values for "Other"
    other_values <- all_counts[10:nrow(all_counts), ]
    other_count <- sum(other_values$count)
    
    # Create Other row
    other_row <- data.frame(
      value = "Other",
      count = other_count,
      percentage = other_count/total_count * 100,
      stringsAsFactors = FALSE
    )
    
    # Combine top 9 with "Other"
    value_counts <- rbind(top_values, other_row)
  } else {
    # If we have 9 or fewer values, just use all of them
    value_counts <- top_values
  }
  
  # Create the bar chart
  p <- ggplot2::ggplot(value_counts, ggplot2::aes(x = stats::reorder(value, count), y = count)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", percentage)),
      hjust = -0.5,
      size = 3
    )
  
  # Apply theme
  p <- NAATools::apply_naa_theme(
    p = p,
    title = "Top Values",
    y_label = "Count"
  ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
  
  # Return results with the plot
  list(
    status = "success",
    message = NULL,
    value = p
  )
} 