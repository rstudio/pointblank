#' Bind environment variable names for database access
#' @description Associates environment variables
#' as credentials for a database. Used to
#' generate a list of environment variable
#' names, which is used as an input value for
#' the \code{db_creds_env_vars} argument of the
#' \code{focus_on()} function.
#' @param dbname the name of the environment
#' variable storing the database name.
#' @param host the name of the environment
#' variable storing the \code{host} name.
#' @param port the name of the environment
#' variable storing the port number.
#' @param user the name of the environment
#' variable storing a username for the
#' database.
#' @param password the name of the environment
#' variable storing the password associated
#' with the \code{user}.
#' @export
db_creds_env_vars <- function(dbname,
                              host,
                              port,
                              user,
                              password) {
  
  # Ensure that the environment variables
  # exist (i.e., are not empty strings)
  if(
    any(
      c(Sys.getenv(dbname),
        Sys.getenv(host),
        Sys.getenv(port),
        Sys.getenv(user),
        Sys.getenv(password)) == "")) {
    stop("All environment variables must exist and not have empty strings.")
  }
  
  # Collect all credential values into a
  # named vector
  env_vars <- list(
    dbname = as.character(dbname),
    host = as.character(host),
    port = as.character(port),
    user = as.character(user),
    password = as.character(password))
  
  env_vars
}
