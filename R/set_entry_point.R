#' Acquire information on the coordinates of
#' a remote table
#' @description If a table is remote (i.e.,
#' in a database), this function will be
#' invoked to set an entry point for the
#' interrogation query.
#' @return an tbl object for a remote table.
#' @importFrom DBI dbListConnections dbListResults dbClearResult dbDisconnect
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom dplyr src_postgres tbl
#' @export set_entry_point

set_entry_point <- function(table,
                            db_type = NULL,
                            credentials_file = NULL) {
  
  if (is.null(db_type) & inherits(table, "data.frame")) {
    
    # Create table entry object
    tbl_entry <- table
  }
  
  if (!is.null(db_type)) {
    
    if (db_type == "PostgreSQL") {
      
      # Establish a new PostgreSQL connection
      if (!is.null(credentials_file)) {
        
        # Disconnect any existing PostgreSQL connections
        cons <- DBI::dbListConnections(RPostgreSQL::PostgreSQL())
        
        for (con in cons) {
          if (length(DBI::dbListResults(con)) != 0) {
            DBI::dbClearResult(DBI::dbListResults(con)[[1]])}
          DBI::dbDisconnect(con)
        }
        
        # Serialize the credentials RDS file
        credentials <- readRDS(credentials_file)
        
        # Establish the connection with the serialized RDS object
        connection <-
          dplyr::src_postgres(
            dbname = credentials[1],
            host = credentials[2],
            port = credentials[3],
            user = credentials[4],
            password = credentials[5])
      } else if (is.null(credentials_file)) {
        stop("A credentials RDS file is required.")
      }
      
      # Create table entry object
      tbl_entry <- dplyr::tbl(src = connection, table)  
      
    }
  }
  
  invisible(tbl_entry)
}
