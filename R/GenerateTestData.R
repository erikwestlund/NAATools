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
generateTestData <- function(freq_table, n = NA, extraCols = list(), countCol = NULL) {
  stopifnot(data.table::is.data.table(freq_table) || is.data.frame(freq_table), is.list(extraCols))

  if (!data.table::is.data.table(freq_table)) {
    data.table::setDT(freq_table)
  }

  freq_table <- data.table::copy(freq_table)

  # Infer or check count column
  if (is.null(countCol)) {
    possibleCols <- c("count", "freq", "n", "frequency")
    countCol <- names(freq_table)[tolower(names(freq_table)) %in% possibleCols]
    if (length(countCol) > 1) stop("Multiple possible count columns found. Please specify `countCol` explicitly.")
    if (length(countCol) == 0) countCol <- NULL
  } else if (!countCol %in% names(freq_table)) {
    stop(sprintf("countCol '%s' not found in freq_table", countCol))
  }

  # Sort by frequency if countCol is present
  if (!is.null(countCol)) {
    data.table::setorder(freq_table, -get(countCol))
  }

  # Convert n
  if (is.null(n) || identical(n, "") || length(n) == 0) {
    n <- NA_real_
  } else {
    n <- suppressWarnings(as.numeric(n))
  }

  if (!is.na(n) && (n < 1 || is.nan(n))) stop("`n` must be a positive number.")
  if (nrow(freq_table) == 0) stop("`freq_table` is empty. Cannot generate synthetic data.")

  # Sampling
  if (is.na(n)) {
    message("`n` is NA. Returning one row per unique frequency combination.")
    sampled_data <- freq_table
  } else if (!is.null(countCol)) {
    weights <- freq_table[[countCol]]
    samples <- sample(seq_len(nrow(freq_table)), size = n, replace = TRUE, prob = weights)
    sampled_data <- freq_table[samples, ]
  } else if (n >= nrow(freq_table)) {
    samples <- rep(seq_len(nrow(freq_table)), length.out = n)
    sampled_data <- freq_table[samples, ]
  } else {
    samples <- sample(seq_len(nrow(freq_table)), size = n, replace = TRUE)
    sampled_data <- freq_table[samples, ]
  }

  true_n <- nrow(sampled_data)

  # Add extraCols
  if (length(extraCols) > 0) {
    for (colName in names(extraCols)) {
      value <- extraCols[[colName]]

      if (is.character(value) && length(value) == 1 && value == "id") {
        sampled_data[, (colName) := seq_len(true_n)]
        next
      }

      if (is.list(value)) value <- unlist(value)

      valueLength <- length(value)
      if (valueLength == 1 || valueLength == true_n || (true_n %% valueLength == 0)) {
        sampled_data[, (colName) := rep(value, length.out = true_n)]
      } else {
        stop(sprintf("Length of extraCols[['%s']] (%d) cannot be recycled to match row count (%d)", colName, valueLength, true_n))
      }
    }
  }

  return(data.table::as.data.table(sampled_data))
}
