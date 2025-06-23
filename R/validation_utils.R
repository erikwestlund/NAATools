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
#' @param max_rows Maximum number of rows to process (default: 1000)
#' 
#' @return A list containing:
#'   \itemize{
#'     \item message: Description of the catalog (including limit info if applicable)
#'     \item entries: Array of invalid value entries, each containing:
#'       \itemize{
#'         \item invalid_value: The value that failed validation
#'         \item count: Number of times this value appears
#'       }
#'   }
#' @export
create_invalid_catalog <- function(invalid_rows, max_rows = 1000) {
  if (nrow(invalid_rows) == 0) {
    return(list(
      message = "No invalid values found",
      entries = list()
    ))
  }
  
  # Create message about row limit if applicable
  message <- if (nrow(invalid_rows) >= max_rows) {
    sprintf("Showing first %d invalid rows out of %d total invalid rows", 
            max_rows, nrow(invalid_rows))
  } else {
    sprintf("Found %d invalid rows", nrow(invalid_rows))
  }
  
  # Limit rows for catalog processing
  if (nrow(invalid_rows) > max_rows) {
    invalid_rows <- invalid_rows[1:max_rows, ]
  }
  
  # Handle missing values (NULL or empty)
  missing_rows <- invalid_rows[is.na(invalid_rows$value) | invalid_rows$value == "", ]
  other_rows <- invalid_rows[!is.na(invalid_rows$value) & invalid_rows$value != "", ]
  
  entries <- list()
  
  # Add missing entries if present
  if (nrow(missing_rows) > 0) {
    entries[[length(entries) + 1]] <- list(
      type = "missing",
      invalid_value = "(Missing)",
      count = nrow(missing_rows),
      row_ranges = format_row_ranges(missing_rows$row_no)
    )
  }
  
  # Add other invalid entries
  if (nrow(other_rows) > 0) {
    for (value in unique(other_rows$value)) {
      group <- other_rows[other_rows$value == value, ]
      entries[[length(entries) + 1]] <- list(
        type = "invalid",
        invalid_value = value,
        count = nrow(group),
        row_ranges = format_row_ranges(group$row_no)
      )
    }
  }
  
  list(
    message = message,
    entries = entries
  )
}

# Helper function to format row ranges
format_row_ranges <- function(row_nos) {
  if (length(row_nos) == 0) return("")
  
  # Convert to character, handling NA values
  formatted <- sapply(row_nos, function(x) {
    if (is.na(x)) "NA" else as.character(x)
  })
  
  # Join with commas
  paste(formatted, collapse = ", ")
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

#' Run a validation query and process results
#' 
#' @param duckdb_conn DuckDB connection object
#' @param query SQL query to run
#' @param var Column name being validated
#' @param counts_fn Function to calculate counts from results
#' @param message_fn Function to generate message from results and counts
#' @param status_fn Function to determine status from results
#' @param total_rows Total number of rows in the table
#' @param max_invalid_catalog_rows Maximum number of rows to include in the invalid catalog (default: 1000)
#' 
#' @return A standardized validation result
#' @export
run_validation <- function(duckdb_conn, query, var, counts_fn, message_fn, status_fn, total_rows, max_invalid_catalog_rows = 1000) {
  # Run query and get results
  result <- DBI::dbGetQuery(duckdb_conn, query)
  
  # Calculate counts using full result for accurate totals
  counts <- counts_fn(result)
  
  # Create invalid catalog from limited rows
  invalid_catalog <- create_invalid_catalog(result, max_rows = max_invalid_catalog_rows)
  
  # Create validation result
  result <- create_validation_result(
    total_rows = total_rows,
    invalid_rows = result,  # Use full result for invalid_rows
    counts = counts,
    is_valid = nrow(result) == 0,  # Use full result for is_valid
    message = message_fn(result, counts, var),  # Use full result for message
    status = status_fn(result, counts)  # Use full result for status
  )
  
  # Add invalid catalog to result
  result$invalid_catalog <- invalid_catalog
  
  result
}

