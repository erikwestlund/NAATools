#' Save an object to an RDS file
#'
#' @param object The R object to save.
#' @param file_path The path where the RDS file should be saved.
#' @export
saveFileSummary <- function(object, file_path) {
  saveRDS(object, file_path)
}

#' Load an object from an RDS file
#'
#' @param file_path The path to the RDS file.
#' @return The loaded R object.
#' @export
loadFileSummary <- function(file_path) {
  readRDS(file_path)
}
