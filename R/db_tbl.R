#' Get a table from a database
#' 
#' @description If your target table is in a database, the `db_tbl()` function
#' is a handy way of accessing it. This function simplifies the process of
#' getting a `tbl_dbi` object, which usually involves a combination of building
#' a connection to a database and using the `dplyr::tbl()` function with the
#' connection and the table name (or a reference to a table in a schema). A
#' better option is to use this function as the `read_fn` parameter in
#' [create_agent()] and [create_informant()]. This can be done by using a
#' leading `~` (e.g,. `read_fn = ~db_tbl(...)`).
#'
#' The username and password are supplied though environment variables. If
#' desired, these can be supplied directly by enclosing those values in `I()`.
#' 
#' @param db Either an appropriate driver function (e.g.,
#'   `RPostgres::Postgres()`) or a shorthand name for the database type. Valid
#'   names are: `"postgresql"`, `"postgres"`, or `"pgsql"` (PostgreSQL, using
#'   the `RPostgres::Postgres()` driver function); `"mysql"` (MySQL, using
#'   `RMySQL::MySQL()`); `"maria"` or `"mariadb"` (MariaDB, using
#'   `RMariaDB::MariaDB()`); and `"sqlite"` (SQLite, using `RSQLite::SQLite()`).
#' @param dbname The database name.
#' @param table The name of the table, or, a reference to a table in a schema
#'   (two-element vector with the names of schema and table). Alternatively,
#'   this can be supplied as a data table to copy into an in-memory database
#'   connection. This only works if: (1) the `db` is either `"sqlite"` or
#'   `"duckdb"`, (2) the `dbname` was chosen as `":memory:"`, and (3) the
#'   `data_tbl` is a data frame or a tibble object.
#' @param user,password The environment variables used to access the username
#'   and password for the database.
#' @param host,port The database host and optional port number.
#'   
#' @return A `tbl_dbi` object.
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-6
#'
#' @export
db_tbl <- function(db,
                   dbname,
                   table,
                   user,
                   password,
                   host = NULL,
                   port = NULL) {
  
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Accessing a database table requires the DBI package:\n",
         " * It can be installed with `install.packages(\"DBI\")`.",
         call. = FALSE)
  }
  
  if (is.character(db)) {
    
    db <- tolower(db)
    
    driver_function <- 
      switch(
        db,
        postgresql =,
        postgres =,
        pgsql = RPostgres_driver(),
        mysql = RMySQL_driver(),
        maria = ,
        mariadb = RMariaDB_driver(),
        duckdb = DuckDB_driver(),
        sqlite = RSQLite_driver(),
        unknown_driver()
      )
    
  } else {
    driver_function <- db
  }

  # Create the DB connection object
  connection <-
    DBI::dbConnect(
      drv = driver_function,
      user = ifelse(inherits(user, "AsIs"), user, Sys.getenv(user)),
      password = ifelse(inherits(password, "AsIs"), password, Sys.getenv(password)),
      host = host,
      dbname = dbname
    )
  
  # Insert data if is supplied, in the right format, and
  # if the DB connection is in-memory
  if (dbname == ":memory:" &&
      is.data.frame(table) && 
      tolower(db) %in% c("duckdb", "sqlite")) {

    # Obtain the name of the data table
    tbl_name <- table_stmt <- deparse(match.call()$table)
    
    # Copy the tabular data into the `connection` object
    dplyr::copy_to(
      dest = connection, 
      df = table,
      name = tbl_name,
      temporary = FALSE
    )
  }
  
  if (is.character(table)) {
    if (length(table) == 1) {
      table_stmt <- table
    } else if (length(table) == 2) {
      table_stmt <- dbplyr::in_schema(schema = table[1], table = table[2])
    } else {
      stop("The length of `table` should be either 1 or 2.",
           call. = FALSE)
    }
  }
  
  dplyr::tbl(src = connection, table_stmt)
}


RPostgres_driver <- function() {
  
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Accessing a PostgreSQL table requires the RPostgres package:\n",
         " * It can be installed with `install.packages(\"RPostgres\")`.",
         call. = FALSE)
  }
  
  RPostgres::Postgres()
}

RMySQL_driver <- function() {
  
  if (!requireNamespace("RMySQL", quietly = TRUE)) {
    stop("Accessing a MariaDB or MySQL table requires the RMySQL package:\n",
         " * It can be installed with `install.packages(\"RMySQL\")`.",
         call. = FALSE)
  }
  
  RMySQL::MySQL()
}

RMariaDB_driver <- function() {
  
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Accessing a MariaDB or MySQL table requires the RMariaDB package:\n",
         " * It can be installed with `install.packages(\"RMariaDB\")`.",
         call. = FALSE)
  }
  
  RMariaDB::MariaDB()
}

DuckDB_driver <- function() {
  
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Accessing a DuckDB table requires the duckdb package:\n",
         " * It can be installed with `install.packages(\"duckdb\")`.",
         call. = FALSE)
  }
  
  duckdb::duckdb()
}

RSQLite_driver <- function() {
  
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Accessing a SQLite table requires the RSQLite package:\n",
         " * It can be installed with `install.packages(\"RSQLite\")`.",
         call. = FALSE)
  }
  
  RSQLite::SQLite()
}

unknown_driver <- function() {
    stop("The supplied value for `db` doesn't correspond to database type:\n",
         " * Acceptable values are: \"pgsql\", \"mysql\", \"mariadb\", and \"sqlite\"", 
         call. = FALSE)
}
