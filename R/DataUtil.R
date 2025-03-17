#' Combine Multiple CSV Files into a Single Data Frame
#'
#' Reads multiple CSV files using `readr::read_csv()`, combines them using `dplyr::bind_rows()`,
#' and returns a single data frame.
#'
#' @param filePaths A character vector of file paths to CSV files.
#' @return A tibble containing all rows from the input files, with an additional `source_file` column
#'         indicating the origin of each row.
#' @importFrom readr read_csv
#' @importFrom purrr map_dfr
#' @export
combineCsvFiles <- function(filePaths) {
  stopifnot(is.character(filePaths), length(filePaths) > 0)

  purrr::map_dfr(filePaths, readr::read_csv, .id = "source_file")
}
