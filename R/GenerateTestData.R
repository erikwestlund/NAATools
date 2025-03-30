#' Generate a synthetic dataset based on a frequency table
#'
#' This function generates a synthetic dataset based on a frequency table.
#' If `n = NA`, it returns one row per unique frequency combination.
#' If `n >= total`, it generates data proportionally based on row counts.
#' If `n < total`, it samples while trying to maintain proportions.
#'
#' @param freq_table A data frame or data.table containing the frequency table.
#' @param n The number of rows to generate (if NA, generates 1 row per unique frequency combo).
#' @param extraCols A named list where each key is a column name to add and
#'   the value is a vector of predefined values that will be recycled or sampled.
#'
#' @return A data.table with `n` rows based on the frequency table.
#' @importFrom data.table as.data.table setDT copy
#' @export
#'
#' @examples
#' library(data.table)
#' freq_table <- data.table::data.table(
#'   category = c("A", "B", "C"),
#'   type = c("X", "Y", "X"),
#'   group = c(1, 2, 1)
#' )
#'
#' test_data <- generateTestData(freq_table, n = 100)
#' print(test_data)
generateTestData <- function(freq_table, n = NA, extraCols = list()) {
  stopifnot(data.table::is.data.table(freq_table) || is.data.frame(freq_table), is.list(extraCols))

  if (!data.table::is.data.table(freq_table)) {
    data.table::setDT(freq_table)
  }

  if (is.null(n) || identical(n, "") || length(n) == 0) {
    n <- NA
  }
  n <- suppressWarnings(as.numeric(n))

  if (!is.na(n) && n < 1) stop("`n` must be a positive number.")
  if (nrow(freq_table) == 0) stop("`freq_table` is empty. Cannot generate synthetic data.")

  freq_table <- data.table::copy(freq_table)

  # Count frequency of unique rows and sort by descending count
  freq_table[, freq_count := .N, by = names(freq_table)]
  freq_table <- unique(freq_table)
  data.table::setorder(freq_table, -freq_count)
  freq_table[, freq_count := NULL]

  total_rows <- nrow(freq_table)

  if (is.na(n)) {
    message("`n` is NA. Returning one row per unique frequency combination.")
    sampled_data <- freq_table
    n <- nrow(sampled_data)
  } else if (n >= total_rows) {
    samples <- rep(seq_len(nrow(freq_table)), length.out = n)
    sampled_data <- freq_table[samples, ]
  } else {
    samples <- sample(seq_len(nrow(freq_table)), size = n, replace = TRUE)
    sampled_data <- freq_table[samples, ]
  }

  # Add extraCols
  if (length(extraCols) > 0) {
    for (colName in names(extraCols)) {
      value <- extraCols[[colName]]
      if (identical(colName, "id")) {
        sampled_data[, (colName) := seq_len(n)]
      } else {
        valueLength <- length(value)
        if (valueLength == n || valueLength == 1) {
          sampled_data[, (colName) := rep(value, length.out = n)]
        } else if (n %% valueLength == 0) {
          sampled_data[, (colName) := rep(value, length.out = n)]
        } else {
          stop(sprintf("Length of extraCols[[%s]] (%d) cannot be recycled to length n (%d)", colName, valueLength, n))
        }
      }
    }
  }

  return(data.table::as.data.table(sampled_data))
}
