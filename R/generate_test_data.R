#' Generate a synthetic dataset based on a frequency table
#'
#' This function generates a synthetic dataset based on a frequency table.
#' If `n = NA`, it returns one row per unique frequency combination.
#' If `n >= total`, it generates data proportionally based on row counts.
#' If `n < total`, it samples while trying to maintain proportions.
#'
#' @param freq_table A data frame or data.table containing the frequency table.
#' @param n The number of rows to generate (if NA, generates 1 row per unique frequency combo).
#' @param extra_cols A named list where each key is a column name to add and
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
#' test_data <- generate_test_data(freq_table, n = 100)
#' print(test_data)
generate_test_data <- function(freq_table, n = NA, extra_cols = list(), count_col = NULL, col_order = NULL) {
  stopifnot(data.table::is.data.table(freq_table) || is.data.frame(freq_table), is.list(extra_cols))

  if (!data.table::is.data.table(freq_table)) {
    data.table::setDT(freq_table)
  }

  freq_table <- data.table::copy(freq_table)

  # Normalize column name: rename "n" to "count" if present
  if ("n" %in% names(freq_table)) {
    data.table::setnames(freq_table, "n", "count")
  }

  # Auto-detect or validate count_col
  if (is.null(count_col)) {
    count_col <- intersect("count", names(freq_table))
    if (length(count_col) == 0) count_col <- NULL
  } else if (!count_col %in% names(freq_table)) {
    stop(sprintf("count_col '%s' not found in freq_table", count_col))
  }

  # Sort by frequency if count_col is present
  if (!is.null(count_col)) {
    data.table::setorderv(freq_table, cols = count_col, order = -1)
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
  } else if (!is.null(count_col)) {
    weights <- freq_table[[count_col]]
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

  # Add extra_cols
  if (length(extra_cols) > 0) {
    for (col_name in names(extra_cols)) {
      value <- extra_cols[[col_name]]

      # Special generator: ID as character
      if (is.character(value) && length(value) == 1 && value == "id") {
        sampled_data[, (col_name) := as.character(seq_len(true_n))]
        next
      }

      # Special generator: date
      if (is.list(value) && !is.null(value$type) && value$type == "date") {
        min_date <- as.Date(value$min_date)
        max_date <- as.Date(value$max_date)
        if (is.na(min_date) || is.na(max_date)) {
          stop(sprintf("Invalid min_date or max_date for column '%s'", col_name))
        }
        if (max_date < min_date) {
          stop(sprintf("max_date must be after min_date for column '%s'", col_name))
        }
        n_days <- as.integer(max_date - min_date)
        dates <- min_date + sample.int(n_days + 1, size = true_n, replace = TRUE)

        format_string <- value$format %||% "%Y-%m-%d"
        sampled_data[, (col_name) := format(dates, format = format_string)]
        next
      }

      # Otherwise: vector value or literal
      if (is.list(value)) value <- unlist(value)

      value_length <- length(value)
      if (value_length == 1 || value_length == true_n || (true_n %% value_length == 0)) {
        sampled_data[, (col_name) := rep(value, length.out = true_n)]
      } else {
        stop(sprintf("Length of extra_cols[['%s']] (%d) cannot be recycled to match row count (%d)", col_name, value_length, true_n))
      }
    }
  }

  # Handle col_order: enforce order, add missing columns, then bind extras
  if (!is.null(col_order)) {
    missing_cols <- setdiff(col_order, names(sampled_data))
    for (col in missing_cols) {
      sampled_data[, (col) := rep("", true_n)]
    }

    # Ensure all requested columns come first, rest follow
    final_cols <- c(col_order, setdiff(names(sampled_data), col_order))
    sampled_data <- sampled_data[, ..final_cols]
  }

  # Remove count, pct columns
  final_df <- data.table::as.data.table(sampled_data) |>
    dplyr::select(-count, -pct)

  return(final_df)
}
