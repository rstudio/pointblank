#' Create a file with DB access credentials
#' @description Creates an RDS file with
#' access credentials for a database.
#' @export create_creds_file

create_creds_file <- function(file,
                              dbname,
                              host,
                              port,
                              user,
                              password) {
  
  # Collect all credential values into a
  # named vector
  credentials <- c(
    dbname = as.character(dbname),
    host = as.character(host),
    port = as.character(port),
    user = as.character(user),
    password = as.character(password))
  
  saveRDS(credentials, file = file)
}
  