#' Combine Multiple CSV Files into a Single Data Frame
#'
#' Reads multiple CSV files using `readr::read_csv()`, ensuring that column types
#' default to character in case of type mismatches, and combines them using `dplyr::bind_rows()`.
#'
#' @param filePaths A character vector of file paths to CSV files.
#' @return A tibble containing all rows from the input files, with an additional `source_file` column
#'         indicating the origin of each row.
#' @importFrom readr read_csv
#' @importFrom purrr map_dfr
#' @export
combineCsvFiles <- function(filePaths) {
  stopifnot(is.character(filePaths), length(filePaths) > 0)

  purrr::map_dfr(filePaths, ~ readr::read_csv(.x, col_types = readr::cols(.default = "c")), .id = "source_file")
}
