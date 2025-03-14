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
#' connectionDetails <- getDepotConnectionDetails(
#'   host = "localhost",
#'   user = "myuser",
#'   password = "mypassword",
#'   dbname = "mydatabase"
#' )
getDepotConnectionDetails <- function(host, user, password, dbname, port = 3306) {
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
#' @param connectionDetails A list of database connection details, as returned by `getDepotConnectionDetails()`.
#'
#' @return A `DBIConnection` object that represents the database connection.
#' @export
#'
#' @examples
#' \dontrun{
#' connectionDetails <- getDepotConnectionDetails(
#'   host = "localhost",
#'   user = "myuser",
#'   password = "mypassword",
#'   dbname = "mydatabase"
#' )
#' conn <- getDepotConnection(connectionDetails)
#' DBI::dbDisconnect(conn)
#' }
getDepotConnection <- function(connectionDetails) {
  DBI::dbConnect(
    drv = connectionDetails$driver,
    host = connectionDetails$host,
    user = connectionDetails$user,
    password = connectionDetails$password,
    dbname = connectionDetails$dbname,
    port = connectionDetails$port
  )
}
