#' Create a file with email access credentials
#' @description Creates a file with access
#' credentials for the purpose of automatically
#' emailing notification messages.
#' @param file a file path for the credentials file
#' to be stored on disk.
#' @param sender the sender name.
#' @param host the \code{host} name.
#' @param port the port number.
#' @param user the username for the email account.
#' @param password the password associated with the
#' \code{user}'s email address.
#' @param use_ssl an option as to whether to use
#' SSL; supply a \code{TRUE} or \code{FALSE}
#' value (\code{TRUE} is the default value).
#' @param authenticate an option as to whether to
#' authenticate; supply a \code{TRUE} or \code{FALSE}
#' value (\code{TRUE} is the default value).
#' @examples
#' \dontrun{
#' # Create a credentials file for automatic
#' # email notifications; place in the user's
#' # home directory
#' create_email_creds_file(
#'   file = "~/.pb_notify",
#'   sender = "point@blank.org",
#'   host = "smtp.blank.org",
#'   port = 465,
#'   user = "point@blank.org",
#'   password = "************")
#' }
#' @export
create_email_creds_file <- function(file,
                                    sender,
                                    host,
                                    port,
                                    user,
                                    password,
                                    use_ssl = TRUE,
                                    authenticate = TRUE) {
  
  # Ensure that `use_ssl` is either TRUE or FALSE
  if (!(use_ssl %in% c(TRUE, FALSE))) {
    stop("The value supplied to `use_ssl` must be TRUE or FALSE.")
  }
  
  # Ensure that `authenticate` is either TRUE or FALSE
  if (!(authenticate %in% c(TRUE, FALSE))) {
    stop("The value supplied to `authenticate` must be TRUE or FALSE.")
  }
  
  # Collect all credential values into a
  # named vector
  credentials <- c(
    sender = as.character(sender),
    host = as.character(host),
    port = as.character(port),
    user = as.character(user),
    password = as.character(password),
    use_ssl = as.character(use_ssl),
    authenticate = as.character(authenticate))
  
  # Save the credential values as a file
  saveRDS(credentials, file = file)
}
