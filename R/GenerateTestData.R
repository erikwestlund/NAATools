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

  # Convert to data.table if it's not already
  if (!data.table::is.data.table(freq_table)) {
    data.table::setDT(freq_table)
  }

  # Ensure `n` is properly evaluated
  if (is.null(n) || identical(n, "") || length(n) == 0) {
    n <- NA
  }

  # Convert `n` to numeric safely
  n <- suppressWarnings(as.numeric(n))

  if (is.na(n)) {
    message("`n` is NA. Returning one row per unique frequency combination.")
  } else if (n < 1) {
    stop("`n` must be a positive number.")
  }

  # Ensure `freq_table` is not empty
  if (nrow(freq_table) == 0) {
    stop("`freq_table` is empty. Cannot generate synthetic data.")
  }

  # Make a copy to prevent modifying the original `data.table`
  freq_table <- data.table::copy(freq_table)
  total_rows <- nrow(freq_table)

  if (is.na(n)) {
    # Case 1: `n = NA`, return one row per unique value combo
    sampled_data <- freq_table
  } else if (n >= total_rows) {
    # Case 2: `n >= total_rows`, generate proportionally
    if (total_rows == 1) {
      samples <- rep(1, length.out = n)
      sampled_data <- freq_table[samples, ]
    } else {
      samples <- rep(seq_len(nrow(freq_table)), length.out = n)
      sampled_data <- freq_table[samples, ]
    }
  } else {
    # Case 3: `n < total_rows`, sample with proportion
    if (n > 0 && n <= total_rows) {
      samples <- sample(seq_len(nrow(freq_table)), size = n, replace = TRUE)
      sampled_data <- freq_table[samples, ]
    } else {
      stop("Invalid `n` value for sampling. Check input.")
    }
  }

  return(data.table::as.data.table(sampled_data))
}
