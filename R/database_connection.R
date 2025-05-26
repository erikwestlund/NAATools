#' Get Depot Connection Details
#'
#' This function returns a list of database connection details for a MariaDB connection.
#'
#' @param host A string specifying the database host.
#' @param user A string specifying the database username.
#' @param password A string specifying the database password.
#' @param dbname A string specifying the database name.
#' @param port An integer specifying the database port (default: 3306).
#'
#' @return A named list containing the connection details required for MariaDB.
#' @export
#'
#' @examples
#' connection_details <- get_depot_connection_details(
#'   host = "localhost",
#'   user = "myuser",
#'   password = "mypassword",
#'   dbname = "mydatabase"
#' )
get_depot_connection_details <- function(host, user, password, dbname, port = 3306) {
  list(
    driver = RMariaDB::MariaDB(),
    host = host,
    user = user,
    password = password,
    dbname = dbname,
    port = port
  )
}

#' Get a Database Connection
#'
#' This function establishes a database connection using the details provided.
#'
#' @param connection_details A list of database connection details, as returned by `get_depot_connection_details()`.
#'
#' @return A `DBIConnection` object that represents the database connection.
#' @export
#'
#' @examples
#' \dontrun{
#' connection_details <- get_depot_connection_details(
#'   host = "localhost",
#'   user = "myuser",
#'   password = "mypassword",
#'   dbname = "mydatabase"
#' )
#' conn <- get_depot_connection(connection_details)
#' DBI::dbDisconnect(conn)
#' }
get_depot_connection <- function(connection_details) {
  DBI::dbConnect(
    drv = connection_details$driver,
    host = connection_details$host,
    user = connection_details$user,
    password = connection_details$password,
    dbname = connection_details$dbname,
    port = connection_details$port
  )
}
