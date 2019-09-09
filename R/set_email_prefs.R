#' Set email credentials and enable email reporting
#'
#' Grants email credentials to a pointblank agent object and provides an
#' opportunity to set email reporting options. This function is to be used in a
#' pointblank pipeline any time before an [interrogate()] call.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param notify_active An option to enable notification emails whenever any of
#'   the validation steps in the `agent` object have triggered a `notify`
#'   status.
#' @param email_recipients An optional vector of email addresses to which
#'   notification emails should be sent.
#' @param creds_file An optional path to an email credentials file. Such a file
#'   can be generated using the [create_email_creds_file()] function.
#'   
#' @examples 
#' \dontrun{
#' # Generate an email credentials
#' # file using the function
#' # `create_email_creds_file()`
#' create_email_creds_file(
#'   file = "~/.pb_email",
#'   sender = "point@blank.org",
#'   host = "smtp.blank.org",
#'   port = 465,
#'   user = "point@blank.org",
#'   password = "************") 
#' 
#' # Create a simple data frame
#' # with a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7))
#' 
#' # Create a pointblank `agent`,
#' # set up the email notification
#' # preferences, and conduct a
#' # simple validation; because
#' # `notify_count` (in the step
#' # where `col_vals_lt()` is called)
#' # has a value of `1` the
#' # `email_recipients` will be
#' # notified when there are one or
#' # more non-passing validations (in
#' # this case, non-passing rows)
#' agent <-
#'   create_agent() %>%
#'   set_email_prefs(
#'     notify_active = TRUE,
#'     email_recipients = 
#'       c("a@b.net", "c@d.com"),
#'     creds_file = "~/.pb_email") %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_lt(
#'     column = a,
#'     value = 6,
#'     notify_count = 1) %>%
#'   interrogate()
#' }
#' 
#' @return Either a \pkg{pointblank} agent object or a table object, depending
#'   on what was passed to `x`.
#' @export
set_email_prefs <- function(agent,
                            notify_active = FALSE,
                            email_recipients = NULL,
                            creds_file = NULL) {
  
  if (notify_active %in% c(TRUE, FALSE)) {
    agent$email_notifications_active <- notify_active
  }

  if (!is.null(email_recipients)) {
    agent$email_notification_recipients <- email_recipients
  }
  
  if (!is.null(creds_file)) {
    agent$email_creds_file_path <- creds_file
  }
  
  agent
}
