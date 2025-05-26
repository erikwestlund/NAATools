#' Create canonical value lookup DuckDB database
#'
#' @param tables Character vector of table names to scaffold.
#' @param save_location Optional file path for the DuckDB database. If NULL, creates an in-memory DB.
#'
#' @return A DBI connection to the DuckDB database.
#' @importFrom DBI dbConnect dbExecute
#' @importFrom duckdb duckdb
#' @importFrom jsonlite toJSON fromJSON
#' @export
create_lookup_db <- function(tables, save_location = NULL) {
  con <- if (is.null(save_location)) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(duckdb::duckdb(), dbdir = save_location)
  }

  for (table in tables) {
    sql <- sprintf(
      "
        CREATE TABLE IF NOT EXISTS %s (
          id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
          \"column\" TEXT NOT NULL,
          value TEXT NOT NULL,
          canonical BOOLEAN DEFAULT FALSE NOT NULL,
          parent_id INTEGER NULL REFERENCES %s(id),
          comment TEXT NULL,
          meta TEXT NULL,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          deactivated_at TIMESTAMP NULL
        );
      ",
      table, table
    )

    DBI::dbExecute(con, sql)
  }

  DBI::dbDisconnect(con, shutdown = TRUE)

  print("Lookup DB created successfully.")
}


#' Add a canonical or alias value to a DuckDB lookup table
#'
#' @param db_path Path to the DuckDB database file.
#' @param table The name of the lookup table (e.g., "medications").
#' @param column The column name in the dataset (e.g., "medicationName").
#' @param value The string value to add (canonical or alias).
#' @param canonical TRUE if this is a canonical value, FALSE if alias.
#' @param parent_value Optional string name of the canonical value to link to (used instead of parent_id).
#' @param comment Optional free-text comment.
#' @param meta Optional named list to serialize as JSON.
#'
#' @return Invisibly returns NULL
#' @export
add_to_lookup_table <- function(db_path,
                             table,
                             column,
                             value,
                             canonical = FALSE,
                             parent_value = NA,
                             comment = NULL,
                             meta = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Validate table exists
  if (!table %in% DBI::dbListTables(con)) {
    stop(sprintf("❌ Table '%s' does not exist in the database.", table))
  }

  # Case-insensitive check for existing value
  query <- sprintf("SELECT * FROM %s WHERE \"column\" = ? AND LOWER(value) = LOWER(?)", table)
  existing <- DBI::dbGetQuery(con, query, params = list(column, value))

  if (nrow(existing) > 0) {
    message(sprintf("⚠️  '%s' already exists in '%s' (column: %s)", value, table, column))
    return(invisible(NULL))
  }

  # Look up parent_id from parent_value (if provided)
  parent_id <- NULL
  if (!is.null(parent_value) && !is.na(parent_value)) {
    lookup_sql <- sprintf(
      "SELECT id FROM %s WHERE \"column\" = ? AND LOWER(value) = LOWER(?) AND canonical = TRUE",
      table
    )
    parent_result <- DBI::dbGetQuery(con, lookup_sql, params = list(column, parent_value))

    if (nrow(parent_result) == 0) {
      stop(sprintf("❌ No canonical value found matching '%s' in '%s'", parent_value, table))
    }
    parent_id <- parent_result$id[1]
  }

  # Convert meta to JSON string if it's a list
  if (is.list(meta)) {
    meta <- as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null"))
  } else if (!is.null(meta) && !is.character(meta)) {
    stop("meta must be a list, character, or NULL")
  }

  parent_id <- if (is.null(parent_id)) NA_integer_ else parent_id
  comment <- if (is.null(comment)) NA_character_ else comment
  meta <- if (is.null(meta)) NA_character_ else meta

  # Insert new record
  insert_sql <- sprintf(
    "INSERT INTO %s (\"column\", value, canonical, parent_id, comment, meta, created_at, updated_at)
     VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)", table
  )
  print(str(list(column, value, canonical, parent_id, comment, meta)))
  # Ensure params are in the correct order: column, value, canonical, parent_id, comment, meta
  DBI::dbExecute(
    con,
    insert_sql,
    params = list(column, value, canonical, parent_id, comment, meta)
  )

  message(sprintf("✅ Inserted '%s' into '%s' (column: %s)", value, table, column))
  invisible(NULL)
}
