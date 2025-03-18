#' Generate a synthetic dataset based on a frequency table
#'
#' This function generates a synthetic dataset based on a frequency table.
#' If `n = NA`, it returns one row per unique frequency combination.
#' If `n >= total`, it generates data proportionally based on `_pct`.
#' If `n < total`, it samples while trying to maintain proportions.
#'
#' @param freq_table A data frame containing the frequency table.
#' @param n The number of rows to generate (if NA, generates 1 row per unique frequency combo).
#' @param extraCols A named list where each key is a column name to add and
#'   the value is a vector of predefined values that will be recycled or sampled.
#'
#' @return A data frame with `n` rows based on the frequency table.
#' @importFrom dplyr sample_n select mutate
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' freq_table <- data.frame(
#'   category = c("A", "B", "C"),
#'   category_n = c(50, 30, 20),
#'   category_pct = c(50, 30, 20)
#' )
#'
#' test_data <- generateTestData(freq_table, n = 100, extraCols = list(
#'   name = c("Alice", "Bob", "Charlie"),
#'   age = c(25, 30, 35)
#' ))
#' print(test_data)
generateTestData <- function(freq_table, n = NA, extraCols = list()) {
  stopifnot(
    is.data.frame(freq_table),
    is.numeric(n) || is.na(n),
    is.list(extraCols)
  )

  # Identify the count and percentage columns
  count_col <- grep("_n$", colnames(freq_table), value = TRUE)
  pct_col <- grep("_pct$", colnames(freq_table), value = TRUE)

  if (length(count_col) != 1 || length(pct_col) != 1) {
    stop("Frequency table must contain exactly one `_n` and one `_pct` column.")
  }

  # Drop the `_n` column to avoid conflicts
  freq_table <- freq_table[, !grepl("_n$", colnames(freq_table)), drop = FALSE]

  # Extract unique value columns
  value_cols <- setdiff(colnames(freq_table), pct_col)

  total_rows <- sum(freq_table[[pct_col]]) / 100  # Total expected rows from freq table

  if (is.na(n)) {
    # Case 1: `n = NA`, return one row per unique value combo
    sampled_data <- freq_table[value_cols]
  } else if (n >= total_rows) {
    # Case 2: `n >= total_rows`, generate proportionally using _pct
    freq_table$generated_n <- round(n * freq_table[[pct_col]] / 100)
    sampled_data <- freq_table[rep(seq_len(nrow(freq_table)), freq_table$generated_n), value_cols, drop = FALSE]
  } else {
    # Case 3: `n < total_rows`, sample but maintain proportion
    sampled_data <- freq_table[value_cols]
    sampled_data <- sampled_data[sample(nrow(sampled_data), size = n, replace = TRUE, prob = freq_table[[pct_col]]), , drop = FALSE]
  }

  # Add extra columns with predefined values
  for (col_name in names(extraCols)) {
    values <- extraCols[[col_name]]
    if (length(values) < nrow(sampled_data)) {
      # Recycle values if fewer than needed
      sampled_data[[col_name]] <- rep(values, length.out = nrow(sampled_data))
    } else {
      # Sample from provided values if there are more than needed
      sampled_data[[col_name]] <- sample(values, size = nrow(sampled_data), replace = TRUE)
    }
  }

  return(sampled_data)
}
