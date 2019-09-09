#' Create a file with DB access credentials
#'
#' Creates a file containing access credentials for a database.
#' 
#' @param file A file path for the credentials file to be stored on disk.
#' @param dbname The database name.
#' @param host The `host` name.
#' @param port The port number.
#' @param user The username for the database
#' @param password The password associated with the `user`.
#' 
#' @examples
#' \dontrun{
#' # Create a credentials file for access to
#' # remote databases (where the tables to be
#' # validated reside); place in the user's
#' # home directory
#' create_creds_file(
#'   file = "~/.pb_credentials",
#'   dbname = "********",
#'   host = "**************",
#'   port = ***,
#'   user = "********",
#'   password = "****************")
#' }
#' 
#' @export
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
    password = as.character(password)
  )
  
  # Save the credential values as a file
  saveRDS(credentials, file = file)
}
