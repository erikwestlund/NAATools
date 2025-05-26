#' Save an object to an RDS file
#'
#' @param object The R object to save.
#' @param file_path The path where the RDS file should be saved.
#' @export
save_file_summary <- function(object, file_path) {
  saveRDS(object, file_path)
}

#' Load an object from an RDS file
#'
#' @param file_path The path to the RDS file.
#' @return The loaded R object.
#' @export
load_file_summary <- function(file_path) {
  readRDS(file_path)
}

#' Read a JSON definition file
#'
#' Reads a JSON file and returns its contents as an R object. This function is useful
#' for reading configuration files, schema definitions, or other structured data stored
#' in JSON format.
#'
#' @param file_path The path to the JSON file to read. Can be a full path or relative path.
#' @param simplifyVector Logical. If TRUE, JSON arrays are converted to R vectors. If FALSE,
#'   they are converted to lists. Defaults to FALSE to preserve the original structure.
#' @return The parsed JSON content as an R object (typically a list).
#' @export
#' @examples
#' \dontrun{
#' # Read a definition file
#' def <- read_definition("path/to/definition.json")
#' 
#' # Read with vector simplification
#' def <- read_definition("path/to/definition.json", simplifyVector = TRUE)
#' }
read_definition <- function(file_path, simplifyVector = FALSE) {
  if (!file.exists(file_path)) {
    stop(sprintf("Definition file not found at: %s", file_path))
  }
  jsonlite::fromJSON(file_path, simplifyVector = simplifyVector)
}
