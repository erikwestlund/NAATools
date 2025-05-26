#' Create a validation result with standardized structure
#' @param total_rows Total number of rows in the table
#' @param invalid_rows Data frame of invalid rows
#' @param counts Named list of counts for different types of invalid values
#' @param is_valid Whether the validation passed
#' @param message Description of validation result
#' @param status Status level ("error", "warn", or "success")
#' @return Standardized validation result list
create_validation_result <- function(total_rows, invalid_rows, counts, is_valid, message, status) {
  c(
    list(
      total_rows = total_rows,
      invalid_rows = invalid_rows,
      is_valid = is_valid,
      message = message,
      status = status
    ),
    counts
  )
}

#' Create a success validation result
#' @param total_rows Total number of rows in the table
#' @param counts Named list of counts (all should be 0 for success)
#' @param message Success message
#' @return Standardized validation result list
create_success_result <- function(total_rows, counts, message) {
  create_validation_result(
    total_rows = total_rows,
    invalid_rows = data.frame(),
    counts = counts,
    is_valid = TRUE,
    message = message,
    status = "success"
  )
}

#' Create an error validation result
#' @param total_rows Total number of rows in the table
#' @param invalid_rows Data frame of invalid rows
#' @param counts Named list of counts for different types of invalid values
#' @param message Error message
#' @return Standardized validation result list
create_error_result <- function(total_rows, invalid_rows, counts, message) {
  create_validation_result(
    total_rows = total_rows,
    invalid_rows = invalid_rows,
    counts = counts,
    is_valid = FALSE,
    message = message,
    status = "error"
  )
}

#' Format a message about invalid values
#' @param counts Named list of counts for different types of invalid values
#' @param var Column name
#' @param prefix Message prefix (e.g., "Found" or "Required")
#' @return Formatted message
format_validation_message <- function(counts, var, prefix = "Found") {
  message_parts <- c()
  for (name in names(counts)) {
    if (counts[[name]] > 0) {
      # Convert count name to readable format (e.g., "na_count" -> "NULL values")
      readable_name <- switch(name,
        "na_count" = "NULL values",
        "null_count" = "NULL values",
        "non_string_count" = "non-string values",
        "empty_count" = "empty strings",
        name
      )
      message_parts <- c(message_parts, sprintf("%d %s", counts[[name]], readable_name))
    }
  }
  
  if (length(message_parts) > 0) {
    sprintf("%s %s in column '%s'", 
            prefix,
            paste(message_parts, collapse = " and "),
            var)
  } else {
    sprintf("No issues found in column '%s'", var)
  }
}

#' Create a catalog of invalid values
#' 
#' @param invalid_rows Data frame containing row_no and value columns
#' 
#' @return A list where each element contains:
#'   \itemize{
#'     \item invalid_value: The value that failed validation
#'     \item row_nos: Vector of row numbers where this value appears
#'     \item count: Number of times this value appears
#'   }
#' @export
create_invalid_catalog <- function(invalid_rows) {
  if (nrow(invalid_rows) == 0) {
    return(list())
  }
  
  # Handle NA values separately
  na_rows <- invalid_rows[is.na(invalid_rows$value), ]
  non_na_rows <- invalid_rows[!is.na(invalid_rows$value), ]
  
  catalog <- list()
  
  # Add NA entries if present
  if (nrow(na_rows) > 0) {
    catalog[["NA"]] <- list(
      invalid_value = NA,
      row_nos = na_rows$row_no,
      count = nrow(na_rows)
    )
  }
  
  # Add non-NA entries
  if (nrow(non_na_rows) > 0) {
    non_na_catalog <- lapply(split(non_na_rows, non_na_rows$value), function(group) {
      list(
        invalid_value = group$value[1],
        row_nos = group$row_no,
        count = length(group$row_no)
      )
    })
    catalog <- c(catalog, non_na_catalog)
  }
  
  catalog
}

#' Create a standardized validation result
#' 
#' @param validator_name Name of the validator
#' @param valid Whether the validation passed
#' @param message Description of the validation result
#' @param invalid_rows Data frame of invalid rows (if any)
#' @param invalid_catalog Catalog of invalid values (if any)
#' @param summary Summary statistics
#' @param severity Severity level ("error" or "warn")
#' @param max_examples Maximum number of example rows to return (default: 5)
#' 
#' @return A standardized validation result list
#' @export
format_validation_result <- function(
  validator_name,
  valid,
  message,
  invalid_rows = NULL,
  invalid_catalog = NULL,
  summary = NULL,
  severity = "error",
  max_examples = 5
) {
  # If we have invalid rows, limit them to max_examples
  example_rows <- NULL
  if (!is.null(invalid_rows) && nrow(invalid_rows) > 0) {
    example_rows <- invalid_rows[1:min(nrow(invalid_rows), max_examples), ]
  }
  
  list(
    validator_name = validator_name,
    valid = valid,
    message = message,
    status = severity,
    example_invalid_rows = example_rows,
    invalid_catalog = invalid_catalog,
    summary = summary
  )
}

#' Calculate validation summary statistics
#' 
#' @param total_rows Total number of rows in the table
#' @param invalid_rows Data frame containing invalid rows
#' @param invalid_catalog List of invalid values with their row numbers
#' 
#' @return A list containing summary statistics about the validation results
#' @export
calculate_validation_summary <- function(total_rows, invalid_rows, invalid_catalog) {
  if (nrow(invalid_rows) == 0) {
    return(list(
      total_rows = total_rows,
      invalid_rows = 0,
      invalid_percent = 0.00,
      unique_invalid_values = 0
    ))
  }
  
  # Count NA and non-NA values
  na_count <- sum(is.na(invalid_rows$value))
  non_na_count <- nrow(invalid_rows) - na_count
  
  list(
    total_rows = total_rows,
    invalid_rows = nrow(invalid_rows),
    invalid_percent = sprintf("%.2f", round(nrow(invalid_rows) / total_rows * 100, 2)),
    unique_invalid_values = length(invalid_catalog),
    na_count = na_count,
    non_na_count = non_na_count,
    most_common_invalid = {
      if (length(invalid_catalog) == 0) {
        return(NULL)
      }
      
      # Get counts, handling NA specially
      counts <- vapply(invalid_catalog, function(x) x$count, numeric(1))
      if (length(counts) == 0 || all(is.na(counts))) {
        return(NULL)
      }
      
      max_count <- max(counts, na.rm = TRUE)
      if (is.infinite(max_count)) {
        return(NULL)
      }
      
      most_common <- invalid_catalog[counts == max_count]
      if (length(most_common) == 0) {
        return(NULL)
      }
      
      list(
        value = most_common[[1]]$invalid_value,
        count = max_count,
        percent = sprintf("%.2f", round(max_count / total_rows * 100, 2))
      )
    }
  )
}

