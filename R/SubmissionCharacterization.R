#' Characterize a Data Frame
#'
#' Analyzes a data frame’s metadata, column types, and frequency distributions.
#'
#' @param df A data frame to be analyzed.
#' @param meta A list containing metadata details such as column types, required columns, and frequency analysis columns.
#'
#' @return A list with two elements:
#'   \item{meta}{A list containing metadata, including row count, column types, and characterizations.}
#'   \item{freqs}{A data frame containing unique value combinations of specified columns with frequency counts and percentages.}
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read.csv("data.csv", stringsAsFactors = FALSE)
#' meta_info <- list(
#'   types = c("col1" = "numeric", "col2" = "character"),
#'   required_cols = c("col1", "col3"),
#'   char_cols = c("col2"),
#'   freq_cols = c("col1", "col2")
#' )
#' result <- characterizeDf(df, meta_info)
#' print(result$meta)
#' print(result$freqs)
#' }
characterizeDf <- function(df, meta) {
  stopifnot(is.data.frame(df))

  meta_info <- list(
    n = nrow(df),
    types = inferColumnTypes(df, meta$types),
    characterizations = characterizeColumns(df, meta$char_cols)
  )

  freqs <- NULL
  if (!is.null(meta$freq_cols)) {
    freqs <- calculateFrequencies(df, meta$freq_cols, meta$required_cols)
  }

  list(meta = meta_info, freqs = freqs)
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
characterizeColumns <- function(data, char_cols) {
  if (is.null(char_cols)) return(NULL)

  lapply(char_cols, function(col) {
    if (col %in% colnames(data)) {
      list(
        unique_values = unique(data[[col]]),
        n_unique = length(unique(data[[col]])),
        n_na = sum(is.na(data[[col]]))
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
