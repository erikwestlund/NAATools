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