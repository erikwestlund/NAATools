#' Read a definition JSON file
#' @param path Path to the JSON file
#' @return A list of variable definitions
#' @export
read_definition <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Definition file not found at: %s", path))
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Summarize a definition (return data frame of variable names and types)
#' @param def The definition list (as returned by read_definition)
#' @return A data.frame with columns: name, type, description
#' @export
summarize_definition <- function(def) {
  data.frame(
    name = sapply(def, function(var) var$name),
    type = sapply(def, function(var) if (!is.null(var$type)) var$type else NA),
    description = sapply(def, function(var) if (!is.null(var$description)) var$description else NA),
    stringsAsFactors = FALSE
  )
} 