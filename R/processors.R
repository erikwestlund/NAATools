#' Get type-specific functions for a column
#' @param type The type of the column
#' @return List of validators, summarizers, and visualizers for the type
#' @export
get_type_functions <- function(type) {
  type_validators <- get_type_validators()
  if (is.null(type) || !(type %in% names(type_validators))) {
    return(list(
      validators = list(),
      summarizers = list(),
      visualizers = list()
    ))
  }
  return(type_validators[[type]])
}

#' Collect all validation rules for a definition
#' @param def The definition list (as returned by read_definition)
#' @return A list of validation rules, with each element containing:
#'         - column: column name
#'         - validator: validator name
#'         - params: validator parameters
#'         - context: validator context/message
#' @export
collect_validation_rules <- function(def) {
  rules <- list()

  # Process each column in the definition
  for (var in def) {
    col_name <- var$name
    col_type <- var$type

    # Get type-specific functions
    type_funcs <- get_type_functions(col_type)

    # Add type-specific validators
    for (validator_name in names(type_funcs$validators)) {
      rules[[length(rules) + 1]] <- list(
        column = col_name,
        validator = validator_name,
        params = list(type = col_type),
        context = sprintf("Must be valid %s", col_type)
      )
    }

    # Add explicit column validators
    if (!is.null(var$validators)) {
      validators <- var$validators
      if (is.character(validators)) {
        # Single string validator
        rules[[length(rules) + 1]] <- list(
          column = col_name,
          validator = validators,
          params = NULL,
          context = ""
        )
      } else if (is.list(validators)) {
        # List of validators
        for (v in validators) {
          if (is.character(v)) {
            rules[[length(rules) + 1]] <- list(
              column = col_name,
              validator = v,
              params = NULL,
              context = ""
            )
          } else if (!is.null(v$name)) {
            rules[[length(rules) + 1]] <- list(
              column = col_name,
              validator = v$name,
              params = v$params,
              context = get_validator_context(v)
            )
          }
        }
      }
    }
  }

  # Convert to data frame for easier viewing
  if (length(rules) > 0) {
    rules_df <- do.call(rbind, lapply(rules, function(r) {
      data.frame(
        column = r$column,
        validator = r$validator,
        params = if (!is.null(r$params)) jsonlite::toJSON(r$params, auto_unbox = TRUE) else "",
        context = r$context,
        stringsAsFactors = FALSE
      )
    }))
    return(rules_df)
  } else {
    return(data.frame(
      column = character(),
      validator = character(),
      params = character(),
      context = character(),
      stringsAsFactors = FALSE
    ))
  }
}

#' Collect all processors (validators, summarizers, visualizers) for a definition
#' @param def The definition list (as returned by read_definition)
#' @return A list where each element is a variable with its processors
#' @export
collect_processors <- function(def) {
  processors <- list()

  for (var in def) {
    col_name <- var$name
    col_type <- var$type
 
    # Get type-specific functions
    type_funcs <- get_type_functions(col_type)

    # Initialize variable processors
    processors[[col_name]] <- list(
      validators = list(),
      summarizers = list(),
      visualizers = list()
    )

    # Add type-specific processors
    processors[[col_name]]$validators <- type_funcs$validators
    processors[[col_name]]$summarizers <- type_funcs$summarizers
    processors[[col_name]]$visualizers <- type_funcs$visualizers

    # Add explicit validators from definition
    if (!is.null(var$validators)) {
      validators <- var$validators
      if (is.character(validators)) {
        # Single string validator
        processors[[col_name]]$validators[[validators]] <- function(value) TRUE # Placeholder
      } else if (is.list(validators)) {
        # List of validators
        for (v in validators) {
          if (is.character(v)) {
            processors[[col_name]]$validators[[v]] <- function(value) TRUE # Placeholder
          } else if (!is.null(v$name)) {
            processors[[col_name]]$validators[[v$name]] <- function(value) TRUE # Placeholder
          }
        }
      }
    }
  }

  return(processors)
} 