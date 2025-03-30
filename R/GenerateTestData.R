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
generateTestData <- function(freq_table, n = NA, extraCols = list(), countCol = NULL, colOrder = NULL) {
  stopifnot(data.table::is.data.table(freq_table) || is.data.frame(freq_table), is.list(extraCols))

  if (!data.table::is.data.table(freq_table)) {
    data.table::setDT(freq_table)
  }

  freq_table <- data.table::copy(freq_table)

  # Normalize column name: rename "n" to "count" if present
  if ("n" %in% names(freq_table)) {
    data.table::setnames(freq_table, "n", "count")
  }

  # Auto-detect or validate countCol
  if (is.null(countCol)) {
    countCol <- intersect("count", names(freq_table))
    if (length(countCol) == 0) countCol <- NULL
  } else if (!countCol %in% names(freq_table)) {
    stop(sprintf("countCol '%s' not found in freq_table", countCol))
  }

  # Sort by frequency if countCol is present
  if (!is.null(countCol)) {
    data.table::setorderv(freq_table, cols = countCol, order = -1)
  }

  # Convert n safely
  if (is.null(n) || identical(n, "") || length(n) == 0) {
    n <- NA_real_
  } else {
    n <- suppressWarnings(as.numeric(n))
  }

  if (!is.na(n) && (n < 1 || is.nan(n))) stop("`n` must be a positive number.")
  if (nrow(freq_table) == 0) stop("`freq_table` is empty. Cannot generate synthetic data.")

  # Sampling
  if (is.na(n)) {
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

      # Special generator: ID as character
      if (is.character(value) && length(value) == 1 && value == "id") {
        sampled_data[, (colName) := as.character(seq_len(true_n))]
        next
      }

      # Special generator: date
      if (is.list(value) && !is.null(value$type) && value$type == "date") {
        minDate <- as.Date(value$minDate)
        maxDate <- as.Date(value$maxDate)
        if (is.na(minDate) || is.na(maxDate)) {
          stop(sprintf("Invalid minDate or maxDate for column '%s'", colName))
        }
        if (maxDate < minDate) {
          stop(sprintf("maxDate must be after minDate for column '%s'", colName))
        }
        nDays <- as.integer(maxDate - minDate)
        dates <- minDate + sample.int(nDays + 1, size = true_n, replace = TRUE)

        formatString <- value$format %||% "%Y-%m-%d"
        sampled_data[, (colName) := format(dates, format = formatString)]
        next
      }

      # Otherwise: vector value or literal
      if (is.list(value)) value <- unlist(value)

      valueLength <- length(value)
      if (valueLength == 1 || valueLength == true_n || (true_n %% valueLength == 0)) {
        sampled_data[, (colName) := rep(value, length.out = true_n)]
      } else {
        stop(sprintf("Length of extraCols[['%s']] (%d) cannot be recycled to match row count (%d)", colName, valueLength, true_n))
      }
    }
  }

  # Handle colOrder: enforce order, add missing columns, then bind extras
  if (!is.null(colOrder)) {
    missingCols <- setdiff(colOrder, names(sampled_data))
    for (col in missingCols) {
      sampled_data[, (col) := rep("", true_n)]
    }

    # Ensure all requested columns come first, rest follow
    finalCols <- c(colOrder, setdiff(names(sampled_data), colOrder))
    sampled_data <- sampled_data[, ..finalCols]
  }

  # Remove count, pct columns
  final_df <- data.table::as.data.table(sampled_data) |>
    dplyr::select(-count, -pct)

  return(final_df)
}
