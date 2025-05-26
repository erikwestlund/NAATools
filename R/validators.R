#' Helper to parse params for a validator
#' @param params List of validator parameters
#' @return Formatted string of parameters
#' @export
parse_validator_params <- function(params) {
  if (is.null(params)) {
    return("")
  }
  # Custom formatting for known param types
  out <- c()
  # If there's a message, skip it here (handled separately)
  for (pn in setdiff(names(params), "message")) {
    val <- params[[pn]]
    if (is.character(val) && length(val) > 1) {
      val_str <- paste0("[", paste(val, collapse = ", "), "]")
    } else if (is.list(val) && length(val) > 1) {
      val_str <- paste0("[", paste(unlist(val), collapse = ", "), "]")
    } else if (is.character(val)) {
      val_str <- paste0("\"", val, "\"")
    } else {
      val_str <- as.character(val)
    }
    out <- c(out, paste0(pn, "=", val_str))
  }
  paste(out, collapse = ", ")
}

#' Get context string for a validator (including message if present)
#' @param validator Validator object or string
#' @return Formatted context string
#' @export
get_validator_context <- function(validator) {
  if (is.character(validator)) {
    return("")
  }
  if (!is.null(validator$params)) {
    msg <- ""
    if (!is.null(validator$params$message)) {
      msg <- paste0(validator$params$message, "\n")
    }
    params_str <- parse_validator_params(validator$params)
    context <- paste0(msg, params_str)
    return(trimws(context))
  }
  return("")
}

#' Summarize all validators in a definition, including parameters
#' @param def The definition list (as returned by read_definition)
#' @return A data.frame with columns: name, type, validator, context
#' @export
summarize_validators <- function(def) {
  rows <- do.call(rbind, lapply(def, function(var) {
    v <- var$validators
    if (is.null(v)) {
      return(NULL)
    }
    if (is.character(v)) {
      data.frame(
        name = var$name,
        type = if (!is.null(var$type)) var$type else "",
        validator = v,
        context = "",
        stringsAsFactors = FALSE
      )
    } else if (is.list(v)) {
      do.call(rbind, lapply(v, function(x) {
        if (is.character(x)) {
          data.frame(
            name = var$name,
            type = if (!is.null(var$type)) var$type else "",
            validator = x,
            context = "",
            stringsAsFactors = FALSE
          )
        } else if (!is.null(x$name)) {
          data.frame(
            name = var$name,
            type = if (!is.null(var$type)) var$type else "",
            validator = x$name,
            context = get_validator_context(x),
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      }))
    } else {
      NULL
    }
  }))
  if (is.null(rows)) {
    return(data.frame(name = character(), type = character(), validator = character(), context = character()))
  }
  rows <- rows[!is.na(rows$validator) & rows$validator != "", ]
  rownames(rows) <- NULL
  rows
}

#' Get validators for a variable definition
#'
#' This function processes a variable definition and returns a list of validators that should be applied
#' to validate the variable. It handles type-based validators, enum validators, boolean validators,
#' explicit validators, and required field validation.
#'
#' @param var_definition list. A variable definition containing:
#'   \itemize{
#'     \item type: The data type (string, number, int, float, year, date, boolean)
#'     \item allowed_values: Optional list of allowed values for enum/boolean validation
#'     \item validators: Optional list of explicit validators to apply
#'     \item value_optional: Optional boolean indicating if the field is optional
#'     \item value_required: Optional boolean indicating if the field is required
#'     \item date_format: Optional date format string (used with date type)
#'   }
#'
#' @return A list of validators, where each validator is a list containing:
#'   \itemize{
#'     \item name: The name of the validator
#'     \item params: Optional parameters for the validator
#'   }
#' @export
get_var_validators <- function(var_definition) {
  # Initialize empty list to store validators
  validators <- list()

  # Define type-specific validators for each supported data type
  # Each validator is a list with a name and optional parameters
  type_validators <- list(
    "string" = list(name = "string", params = NULL),
    "number" = list(name = "number", params = NULL),
    "int" = list(name = "int", params = NULL),
    "float" = list(name = "float", params = NULL),
    "year" = list(name = "year", params = NULL),
    "date" = list(name = "date", params = list(var_definition$date_format))
  )

  # Add type-specific validator if the variable has a supported type
  if (var_definition$type %in% names(type_validators)) {
    validators <- c(validators, list(type_validators[[var_definition$type]]))
  }

  # Add enum validator if the variable has allowed_values defined
  # This ensures values are restricted to the specified set
  if (!is.null(var_definition$allowed_values)) {
    validators <- c(validators, list(list(
      name = "enum_allowed_values",
      params = var_definition$allowed_values
    )))
  }

  # Add boolean validator if the variable is of type boolean and has allowed_values
  # This is a special case for boolean fields that need specific value validation
  if (!is.null(var_definition$allowed_values) && var_definition$type == "boolean") {
    validators <- c(validators, list(list(
      name = "boolean_allowed_values",
      params = var_definition$allowed_values
    )))
  }

  # Add any explicit validators specified in the variable definition
  # These can be either simple strings or complex validator objects
  if (!is.null(var_definition$validators)) {
    for (validator in var_definition$validators) {
      if (is.character(validator)) {
        # Simple string validator (no parameters)
        validators <- c(validators, list(list(
          name = validator,
          params = NULL
        )))
      } else {
        # Complex validator object (with name and parameters)
        validators <- c(validators, list(validator))
      }
    }
  }

  # Determine if the field is optional based on value_optional or value_required flags
  is_optional <- isTRUE(var_definition$value_optional) ||
    (isTRUE(!is.null(var_definition$value_required)) && !isTRUE(var_definition$value_required))

  # Check if a required validator is already present
  has_required_validator <- any(sapply(validators, function(v) v$name %in% c("required_when", "required")))

  # Add required validator if the field is not optional and doesn't already have a required validator
  if (!is_optional && !has_required_validator) {
    validators <- c(validators, list(list(
      name = "required",
      params = TRUE
    )))
  }

  validators
} 