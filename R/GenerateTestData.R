#' Generate a synthetic dataset based on a frequency table
#'
#' This function generates a synthetic dataset based on a frequency table.
#' If `n = NA`, it returns one row per unique frequency combination.
#' If `n >= total`, it generates data proportionally based on row counts.
#' If `n < total`, it samples while trying to maintain proportions.
#'
#' @param freq_table A data frame containing the frequency table.
#' @param n The number of rows to generate (if NA, generates 1 row per unique frequency combo).
#' @param extraCols A named list where each key is a column name to add and
#'   the value is a vector of predefined values that will be recycled or sampled.
#'
#' @return A data frame with `n` rows based on the frequency table.
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' freq_table <- data.frame(
#'   category = c("A", "B", "C"),
#'   type = c("X", "Y", "X"),
#'   group = c(1, 2, 1),
#'   category_n = c(10, 20, 30),  # These should be removed
#'   category_pct = c(20, 40, 40) # These should be removed
#' )
#'
#' test_data <- generateTestData(freq_table, n = 100, extraCols = list(
#'   name = c("Alice", "Bob", "Charlie"),
#'   patientId = c() # Will be blank
#' ))
#' print(test_data)
generateTestData <- function(freq_table, n = NA, extraCols = list()) {
  stopifnot(
    is.data.frame(freq_table),
    is.numeric(n) || is.na(n),
    is.list(extraCols)
  )

  # Remove any `_n` and `_pct` columns if they exist
  freq_table <- freq_table[, !grepl("(_n|_pct)$", colnames(freq_table)), drop = FALSE]

  total_rows <- nrow(freq_table)

  if (is.na(n)) {
    # Case 1: `n = NA`, return one row per unique value combo
    sampled_data <- freq_table
  } else if (n >= total_rows) {
    # Case 2: `n >= total_rows`, generate proportionally
    sampled_data <- freq_table[rep(seq_len(nrow(freq_table)), length.out = n), , drop = FALSE]
  } else {
    # Case 3: `n < total_rows`, sample with proportion
    sampled_data <- freq_table[sample(seq_len(nrow(freq_table)), size = n, replace = TRUE), , drop = FALSE]
  }

  # Add extra columns with predefined values
  for (col_name in names(extraCols)) {
    values <- extraCols[[col_name]]

    if (length(values) == 0) {
      # If no values provided, create blank character column
      sampled_data[[col_name]] <- rep("", nrow(sampled_data))
    } else {
      # Otherwise, recycle or sample values
      sampled_data[[col_name]] <- rep(values, length.out = nrow(sampled_data))
    }
  }

  return(sampled_data)
}
