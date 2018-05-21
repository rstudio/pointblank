#' Create a file with DB access credentials
#' @description Creates a file containing access
#' credentials for a database.
#' @param file a file path for the credentials file
#' to be stored on disk.
#' @param dbname the database name.
#' @param host the \code{host} name.
#' @param port the port number.
#' @param user the username for the database
#' @param password the password associated with the
#' \code{user}.
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
    password = as.character(password))
  
  # Save the credential values as a file
  saveRDS(credentials, file = file)
}
