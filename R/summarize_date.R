#' Analyze date column characteristics
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table containing the variable
#' @param var Name of the column to analyze
#' @param params Optional parameters (not used for date analysis)
#' @return A list containing:
#'   \item{status}{"success" or "error"}
#'   \item{message}{Description of any error}
#'   \item{value}{A list containing:
#'     \item{date_stats}{Min/max date and total count}
#'     \item{date_distribution}{Distribution of dates in bins}
#'     \item{month_distribution}{Distribution of months}
#'     \item{common_dates}{Most common dates}
#'     \item{histogram}{ggplot object for date distribution}
#'   }
#' @export
summarize_date <- function(duckdb_conn, table_name, var, params = NULL) {
  # Check if column exists
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table", var)
    ))
  }
  
  # Get date statistics and distribution
  query <- sprintf("
    WITH date_stats AS (
      SELECT 
        MIN(%s) as min_date,
        MAX(%s) as max_date,
        COUNT(*) as total
      FROM %s
      WHERE %s IS NOT NULL
    ),
    date_distribution AS (
      SELECT 
        %s as date,
        COUNT(*) as count
      FROM %s
      WHERE %s IS NOT NULL
      GROUP BY %s
      ORDER BY date
    )
    SELECT 
      s.*,
      d.date as bin_date,
      d.count as bin_count,
      ROUND(100.0 * d.count / s.total, 1) as bin_pct
    FROM date_stats s
    CROSS JOIN date_distribution d
    ORDER BY d.date
  ", var, var, table_name, var, 
     var, table_name, var, var)
  
  # Execute query
  results <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Calculate date range and create bins
  min_date <- as.Date(results$min_date[1])
  max_date <- as.Date(results$max_date[1])
  date_range <- as.numeric(max_date - min_date)
  
  # Create histogram data
  hist_data <- data.frame(
    date = as.Date(results$bin_date),
    count = results$bin_count
  )
  
  # Create histogram plot with 10 bins
  p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = date, weight = count)) +
    ggplot2::geom_histogram(bins = 10, fill = "steelblue") +
    ggplot2::scale_x_date(
      date_breaks = "4 years",
      date_labels = "%Y",
      limits = c(min_date, max_date)
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  # Apply NAA theme
  p <- NAATools::apply_naa_theme(
    p = p,
    title = "Date Distribution",
    x_label = "Year",
    y_label = "Count",
    rotate_x_labels = TRUE
  )
  
  # Format results
  list(
    status = "success",
    message = NULL,
    value = list(
      date_stats = list(
        min_date = as.character(results$min_date[1]),
        max_date = as.character(results$max_date[1]),
        total = format(results$total[1], big.mark = ","),
        date_range = date_range,
        bin_size = "4 years",
        n_bins = 10
      ),
      date_distribution = lapply(seq_len(nrow(hist_data)), function(i) {
        list(
          date = as.character(hist_data$date[i]),
          count = format(hist_data$count[i], big.mark = ","),
          pct = sprintf("%.1f%%", hist_data$count[i] / results$total[1] * 100)
        )
      }),
      histogram = p
    )
  )
} 