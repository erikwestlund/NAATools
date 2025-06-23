#' Validate that a column contains valid enumeration values
#' 
#' This is an alias for validate_enum_allowed_values but may have simpler usage.
#' Validates that values are from a predefined set of allowed values.
#' 
#' @param duckdb_conn DuckDB connection object
#' @param table_name Name of the table to validate (default: "data")
#' @param var Name of the column to validate
#' @param params List containing:
#'   - allowed_values: List of allowed values (can be simple values or list with value/description)
#' 
#' @return A list containing:
#'   - total_rows: Total number of rows in the table
#'   - invalid_rows: Data frame of rows with values outside allowed set
#'   - invalid_catalog: Catalog of invalid values and their row numbers
#'   - invalid_value_count: Number of invalid values
#'   - is_valid: Boolean indicating if all values are in allowed set
#'   - message: Description of validation result
#'   - status: "error", "warn", or "success"
#' @export
validate_enum <- function(duckdb_conn, table_name = "data", var, params) {
  # Delegate to validate_enum_allowed_values
  validate_enum_allowed_values(duckdb_conn, table_name, var, params)
}