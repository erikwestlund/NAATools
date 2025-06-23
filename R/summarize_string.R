#' Analyze string column characteristics
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table containing the variable
#' @param var Name of the column to analyze
#' @param params Optional parameters (not used for string analysis)
#' @return A list containing:
#'   \item{status}{"success" or "error"}
#'   \item{message}{Description of any error}
#'   \item{results}{A list containing:
#'     \item{length_stats}{Min/max/avg length statistics}
#'     \item{top_values}{Most common values}
#'     \item{pattern_stats}{Pattern matching statistics:
#'       \item{letters_only}{Percentage of values containing only letters (ignoring spaces)}
#'       \item{numbers_only}{Percentage of values containing only numbers (ignoring spaces)}
#'       \item{letters_and_numbers}{Percentage of values containing both letters and numbers (ignoring spaces)}
#'       \item{other}{Percentage of values containing symbols or other characters}
#'       \item{description}{Explanation of the pattern statistics}
#'     }
#'   }
#' @export
summarize_string <- function(duckdb_conn, table_name, var, params = NULL) {
  # Check if column exists
  if (!NAATools::column_exists(duckdb_conn, table_name, var)) {
    return(list(
      status = "error",
      message = sprintf("Column '%s' not found in table", var)
    ))
  }
  
  # Get total rows for percentage calculations
  total_rows <- DBI::dbGetQuery(duckdb_conn, sprintf("SELECT COUNT(*) as count FROM %s", table_name))$count
  
  # Get string statistics
  query <- sprintf("
    WITH stats AS (
      SELECT 
        MIN(LENGTH(CAST(%s AS VARCHAR))) as min_len,
        MAX(LENGTH(CAST(%s AS VARCHAR))) as max_len,
        AVG(LENGTH(CAST(%s AS VARCHAR))) as avg_len,
        COUNT(*) as total,
        COUNT(CASE WHEN REGEXP_REPLACE(CAST(%s AS VARCHAR), '\\s+', '', 'g') ~ '^[A-Za-z]+$' THEN 1 END) as letters_only,
        COUNT(CASE WHEN REGEXP_REPLACE(CAST(%s AS VARCHAR), '\\s+', '', 'g') ~ '^[0-9]+$' THEN 1 END) as numbers_only,
        COUNT(CASE WHEN REGEXP_REPLACE(CAST(%s AS VARCHAR), '\\s+', '', 'g') ~ '^[A-Za-z0-9]+$' 
                   AND REGEXP_REPLACE(CAST(%s AS VARCHAR), '\\s+', '', 'g') ~ '[A-Za-z]' 
                   AND REGEXP_REPLACE(CAST(%s AS VARCHAR), '\\s+', '', 'g') ~ '[0-9]' THEN 1 END) as letters_and_numbers
      FROM %s
      WHERE %s IS NOT NULL
    ),
    top_values AS (
      SELECT CAST(%s AS VARCHAR) as value, COUNT(*) as count
      FROM %s
      WHERE %s IS NOT NULL
      GROUP BY CAST(%s AS VARCHAR)
      ORDER BY count DESC
      LIMIT 5
    )
    SELECT 
      s.*,
      t.value as top_value,
      t.count as top_count
    FROM stats s
    CROSS JOIN top_values t
  ", var, var, var, var, var, var, var, var, table_name, var, var, table_name, var, var)
  
  # Execute query
  results <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Calculate percentages
  total <- results$total[1]
  letters_only_pct <- results$letters_only[1] / total * 100
  numbers_only_pct <- results$numbers_only[1] / total * 100
  letters_and_numbers_pct <- results$letters_and_numbers[1] / total * 100
  other_pct <- 100 - letters_only_pct - numbers_only_pct - letters_and_numbers_pct
  
  # Format results
  list(
    status = "success",
    message = NULL,
    value = list(
      length_stats = list(
        min = as.character(results$min_len[1]),
        max = as.character(results$max_len[1]),
        avg = sprintf("%.1f", results$avg_len[1])
      ),
      pattern_stats = list(
        letters_only = sprintf("%.1f%%", letters_only_pct),
        numbers_only = sprintf("%.1f%%", numbers_only_pct),
        letters_and_numbers = sprintf("%.1f%%", letters_and_numbers_pct),
        other = sprintf("%.1f%%", other_pct),
        description = "These percentages show how many values contain only letters, only numbers, both letters and numbers, or other characters (symbols, etc.). Spaces are ignored in the pattern matching. The percentages sum to 100%."
      ),
      top_values = lapply(1:nrow(results), function(i) {
        list(
          value = results$top_value[i],
          count = format(results$top_count[i], big.mark = ",")
        )
      })
    )
  )
}
