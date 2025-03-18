#' Characterize a Data Frame
#'
#' Analyzes a data frame’s metadata, column types, and frequency distributions.
#'
#' @param df A data frame to be analyzed.
#' @param meta A list containing metadata details. Must include:
#'   - `types`: Expected column types.
#'   - `freq_cols`: Columns to compute frequency distributions.
#'
#' @return A list with three elements:
#'   \item{meta}{A list containing metadata, including row count and column types.}
#'   \item{freqs}{A data frame containing unique value combinations of specified columns with frequency counts and percentages.}
#'   \item{characterizations}{A list characterizing all columns, including unique counts, missing values, and validation percentages.}
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   col1 = c("123", "456", "abc", NA),
#'   col2 = c("A", "B", "C", "D")
#' )
#' meta_info <- list(
#'   types = c("col1" = "numeric", "col2" = "character"),
#'   freq_cols = c("col1", "col2"),
#'   required_cols = c("col1", "col3")
#' )
#' result <- characterizeDf(df, meta_info)
#' print(result$meta)
#' print(result$freqs)
#' print(result$characterizations)
#' }
characterizeDf <- function(df, meta) {
  stopifnot(
    is.data.frame(df),
    is.list(meta),
    "types" %in% names(meta),
    "freq_cols" %in% names(meta)
  )

  meta_info <- list(
    n = nrow(df),
    types = inferColumnTypes(df, meta$types)
  )

  freqs <- calculateFrequencies(df, meta$freq_cols, meta$required_cols %||% NULL)

  characterizations <- characterizeColumns(df, colnames(df))  # Characterizing ALL columns

  list(meta = meta_info, freqs = freqs, characterizations = characterizations)
}


#' Characterize a File
#'
#' Reads a CSV file and analyzes its metadata, column types, and frequency distributions
#' by passing the data frame to `characterizeDf()`.
#'
#' @param file A string specifying the path to the CSV file to be read.
#' @param meta A list containing metadata details such as column types, required columns, and frequency analysis columns.
#'
#' @return A list with two elements:
#'   \item{meta}{A list containing metadata, including row count, column types, and characterizations.}
#'   \item{freqs}{A data frame containing unique value combinations of specified columns with frequency counts and percentages.}
#' @export
#'
#' @examples
#' \dontrun{
#' meta_info <- list(
#'   types = c("col1" = "numeric", "col2" = "character"),
#'   required_cols = c("col1", "col3"),
#'   char_cols = c("col2"),
#'   freq_cols = c("col1", "col2")
#' )
#' result <- characterizeFile("data.csv", meta_info)
#' print(result$meta)
#' print(result$freqs)
#' }
characterizeFile <- function(file, meta) {
  stopifnot(is.character(file), file.exists(file))

  df <- readr::read_csv(file, show_col_types = FALSE)
  characterizeDf(df, meta)
}



# Infer column types based on provided types and validate numeric columns
inferColumnTypes <- function(data, expected_types) {
  inferred_types <- sapply(data, class)

  if (!is.null(expected_types)) {
    for (col in names(expected_types)) {
      if (col %in% colnames(data) && expected_types[[col]] == "numeric") {
        data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
        fail_rate <- mean(is.na(data[[col]]))
        if (fail_rate > 0) {
          inferred_types[[col]] <- paste0("numeric (", round((1 - fail_rate) * 100, 2), "% valid)")
        }
      }
    }
  }

  as.list(inferred_types)
}


# Perform detailed characterization of specified character columns
#' Characterize Columns
#'
#' Analyzes the given columns in a data frame, computing unique values, count of unique values,
#' missing values, and percentages of values that validate as numeric and character.
#'
#' @param data A data frame.
#' @param cols A character vector specifying which columns to analyze.
#'
#' @return A named list where each element contains:
#'   - `unique_values`: Unique values in the column.
#'   - `n_unique`: Number of unique values.
#'   - `n_missing`: Number of missing (NA) values.
#'   - `n_na`: Alias for `n_missing`, same value.
#'   - `percent_numeric`: Percentage of values that are valid numerics.
#'   - `percent_character`: Percentage of values that are valid characters.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   col1 = c("123", "456", "abc", NA),
#'   col2 = c("A", "B", "C", "D")
#' )
#' result <- characterizeColumns(df, c("col1", "col2"))
#' print(result)
characterizeColumns <- function(data, cols) {
  if (is.null(cols)) return(NULL)

  lapply(cols, function(col) {
    if (col %in% colnames(data)) {
      values <- data[[col]]
      non_na_values <- values[!is.na(values)]

      percent_numeric <- mean(suppressWarnings(!is.na(as.numeric(non_na_values)))) * 100
      percent_character <- mean(!is.na(non_na_values)) * 100  # Everything non-NA is character in R
      n_missing <- sum(is.na(values))

      list(
        unique_values = unique(values),
        n_unique = length(unique(values)),
        n_missing = n_missing,
        n_na = n_missing,  # Alias
        percent_numeric = percent_numeric,
        percent_character = percent_character
      )
    } else {
      NULL
    }
  })
}


# Calculate unique value frequencies for specified columns
calculateFrequencies <- function(data, freq_cols, required_cols) {
  # Ensure required columns exist in data
  all_columns <- unique(c(colnames(data), required_cols))
  missing_cols <- setdiff(required_cols, colnames(data))
  for (col in missing_cols) {
    data[[col]] <- NA_character_
  }

  # Get unique combinations
  freq_table <- data[, freq_cols, drop = FALSE]
  freq_table <- na.omit(freq_table)
  freq_table <- as.data.frame(table(freq_table))

  # Rename count column
  colnames(freq_table)[ncol(freq_table)] <- "freq_n"

  # Calculate percentages
  freq_table$freq_pct <- round(freq_table$freq_n / sum(freq_table$freq_n) * 100, 2)

  freq_table
}
