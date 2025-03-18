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

  freqs <- calculateFrequencies(df, meta$freq_cols,  meta$required_cols %||% NULL, meta$types)

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


#' Characterize Columns
#'
#' Analyzes the given columns in a data frame, computing unique values, count of unique values,
#' missing values, and percentages of values that validate as numeric and character.
#'
#' @param data A data frame.
#' @param cols A character vector specifying which columns to analyze.
#'
#' @return A named list where each element (named after the column) contains:
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
#'   col2 = c("A", "B", "C", "D"),
#'   col3 = c(1, 2, 3, NA)
#' )
#' result <- characterizeColumns(df, colnames(df))
#' print(result$col1)
characterizeColumns <- function(data, cols) {
  if (is.null(cols)) return(NULL)

  result <- lapply(cols, function(col) {
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

  setNames(result, cols)  # Name the list items by column names
}


#' Calculate unique value frequencies per column with type enforcement
#'
#' Computes frequency counts and percentages for each unique value in the specified columns,
#' ensuring that the columns match the specified types.
#'
#' @param data A data frame.
#' @param freq_cols A character vector specifying which columns to analyze.
#' @param required_cols A character vector of required columns that should exist in the data (missing ones are created as NA).
#' @param col_types A named character vector specifying expected column types (e.g., `c("col1" = "numeric", "col2" = "character")`).
#'
#' @return A data frame where each unique value in `freq_cols` has:
#'   - `{col}` (column name with unique values).
#'   - `{col}_n` (count of occurrences in the data).
#'   - `{col}_pct` (percentage of occurrences in the data).
#' @importFrom dplyr distinct count mutate bind_rows all_of
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' df <- data.frame(
#'   category = c("A", "A", "B", "B", "C"),
#'   type = c("X", "Y", "X", "Y", "X"),
#'   group = c(1, 2, 1, 2, 3)
#' )
#' col_types <- c("category" = "character", "type" = "character", "group" = "numeric")
#' result <- calculateFrequencies(df, c("category", "type", "group"), NULL, col_types)
#' print(result)
calculateFrequencies <- function(data, freq_cols, required_cols, col_types) {
  stopifnot(
    is.data.frame(data),
    is.character(freq_cols),
    length(freq_cols) > 0,
    is.null(col_types) || is.list(col_types) || is.character(col_types)
  )

  # Ensure all columns in `col_types` exist in the data, creating them if missing
  missing_cols <- setdiff(names(col_types), colnames(data))
  for (col in missing_cols) {
    data[[col]] <- NA_character_
  }

  # Enforce column types and replace NAs with ""
  for (col in names(col_types)) {
    if (col %in% colnames(data)) {
      if (col_types[[col]] == "numeric") {
        data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      } else {
        data[[col]] <- as.character(data[[col]])
      }
      data[[col]][is.na(data[[col]])] <- ""  # Replace NA with empty string
    }
  }

  # Initialize an empty list to store frequency tables for each column
  freq_list <- lapply(freq_cols, function(col) {
    if (col %in% colnames(data)) {
      # Step 1: Get unique values for the column
      unique_values <- dplyr::distinct(data, dplyr::all_of(col))

      # Step 2: Count occurrences of each unique value in the original dataset
      value_counts <- dplyr::count(data, dplyr::all_of(col), name = paste0(col, "_n"))

      # Step 3: Calculate percentages
      value_counts <- dplyr::mutate(
        value_counts,
        !!paste0(col, "_pct") := round(.data[[paste0(col, "_n")]] / sum(.data[[paste0(col, "_n")]]) * 100, 2)
      )

      # Step 4: Combine unique values and their counts
      final_table <- dplyr::bind_cols(unique_values, value_counts[, -1, drop = FALSE])

      return(final_table)
    } else {
      return(NULL)
    }
  })

  # Combine all column-wise frequency tables into a single data frame
  freq_table_final <- dplyr::bind_rows(freq_list)

  return(freq_table_final)
}
