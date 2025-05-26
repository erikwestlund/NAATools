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

#' Get a specific variable definition from a table definition
#' @param table_definition The table definition list
#' @param var_name The name of the variable to get
#' @return The variable definition for the specified variable
#' @export
get_var_definition <- function(table_definition, var_name) {
  table_definition[sapply(table_definition, function(x) x$name == var_name)][[1]]
} 