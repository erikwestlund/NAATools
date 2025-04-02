library(NAATools)

db_file = "~/Desktop/lookupDb.duckdb"

addToLookupTable(
  db_path = db_file,
  table = "medication",
  column = "medicationName",
  value = "Didanosine",
  canonical = TRUE,
  comment = "Commercial name",
  meta = list(source = "VA 2022", verified = TRUE)
)



addToLookupTable(
  db_path = db_file,
  table = "medication",
  column = "medicationName",
  value = "Videx",
  canonical = FALSE,
  parent_id = 1,
  comment = "Commercial name",
  meta = list(source = "VA 2022", verified = TRUE)
)


cleanDf(df, table)


addToMedicatiion(name="Official", c("A", "B"))




con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "identity_test.duckdb")

DBI::dbExecute(con, "DROP TABLE IF EXISTS test_table")

DBI::dbExecute(con, "
  CREATE TABLE test_table (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT
  );
")

DBI::dbExecute(con, "INSERT INTO test_table (name) VALUES ('A'), ('B')")

print(DBI::dbReadTable(con, "test_table"))
DBI::dbDisconnect(con, shutdown = TRUE)
