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
