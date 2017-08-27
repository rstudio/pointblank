#' Create an agent object
#' @description Creates an agent object.
#' @param name optional name for the agent that
#' will eventually carry out the interrogation
#' process. If no value provided, a name will be
#' generated based on the current system time.
#' @param email_creds_file_path an optional path
#' to an email credentials file.
#' @param notification_recipient_emails an optional
#' vector of email addresses to which notification
#' emails should be sent.
#' @param notification_emails_active an option to
#' enable notification emails when tests trigger a
#' \code{notify} status.
#' @examples 
#' # Create an `agent` object in order to begin
#' # defining validation steps
#' agent <- create_agent()
#' 
#' \dontrun{
#' # Should notifications be required through
#' # email, we first create an email credentials
#' # file, with `create_email_creds_file()`, and
#' # then reference that file with `create_agent()`
#' create_email_creds_file(
#'   file = "~/.pb_notify",
#'   sender = "point@blank.org",
#'   host = "smtp.blank.org",
#'   port = 465,
#'   user = "point@blank.org",
#'   password = "************") 
#' 
#' agent_notify <-
#'   create_agent(
#'   email_creds_file_path = "~/.pb_notify",
#'   notification_recipient_emails = 
#'     c("a@b.net", "c@d.com", "e@f.org"),
#'   notification_emails_active = TRUE)
#' }
#' 
#' # Then, as with any `ptblank_agent` object,
#' # we can focus on different table, add
#' # validation steps, and then eventually use
#' # `interrogate()` to perform the validations
#' agent <-
#'   agent %>%
#'   focus_on(
#'     file_name = 
#'       system.file(
#'         "extdata", "small_table.csv",
#'         package = "pointblank"),
#'     col_types = "TDicidlc") %>%
#'   col_exists(column = a) %>%
#'   interrogate()
#'  
#' # A basic summary can be produced
#' # using `get_interrogation_summary()`
#' get_interrogation_summary(agent)[, 1:7]
#' #> # A tibble: 1 x 7
#' #>      tbl_name    db_type assertion_type column value regex all_passed
#' #>         <chr>      <chr>          <chr>  <chr> <dbl> <chr>      <lgl>
#' #> 1 small_table local_file     col_exists      a    NA  <NA>       TRUE
#' @return an agent object.
#' @importFrom dplyr filter
#' @importFrom tibble tibble as_tibble
#' @export create_agent

create_agent <- function(name = NULL,
                         email_creds_file_path = NULL,
                         notification_recipient_emails = NULL,
                         notification_emails_active = FALSE) {
  
  # Generate an agent name if none provided
  if (is.null(name)) {
    name <- paste0("agent_", gsub(" ", "_", Sys.time() %>% as.character()))
    brief <- "Create agent with auto-assigned name"
  } else {
    brief <- "Create agent with an assigned name"
  }
  
  # Create the agent list object
  agent <-
    list(
      validation_name = as.character(NA)[-1],
      validation_time = as.POSIXct(NA)[-1],
      focal_tbl_name = as.character(NA)[-1],
      focal_file_name = as.character(NA)[-1],
      focal_db_type = as.character(NA)[-1],
      focal_col_names = as.character(NA)[-1],
      focal_col_types = as.character(NA)[-1],
      focal_db_cred_file_path = as.character(NA)[-1],
      focal_init_sql = as.character(NA)[-1],
      email_creds_file_path = as.character(NA)[-1],
      notification_recipients = as.character(NA)[-1],
      notification_emails_active = FALSE,
      logical_plan =
        tibble::tibble(
          component_name = as.character("create_agent"),
          parameters = as.character(NA),
          brief = brief),
      validation_set =
        tibble::tibble(
          tbl_name = as.character(NA),
          db_type = as.character(NA),
          assertion_type = as.character(NA),
          column = as.character(NA),
          value = as.numeric(NA),
          regex = as.character(NA),
          all_passed = as.logical(NA),
          n = as.integer(NA),
          n_passed = as.integer(NA),
          n_failed = as.integer(NA),
          f_passed = as.numeric(NA),
          f_failed = as.numeric(NA),
          warn_count = as.numeric(NA),
          notify_count = as.numeric(NA),
          warn_fraction = as.numeric(NA),
          notify_fraction = as.numeric(NA),
          warn = as.logical(NA),
          notify = as.logical(NA),
          row_sample = as.numeric(NA),
          init_sql = as.character(NA),
          db_cred_file_path = as.character(NA),
          file_path = as.character(NA),
          col_types = as.character(NA),
          time_processed = as.POSIXct(NA),
          proc_duration_s = as.numeric(NA)),
      sets = list(),
      preconditions = list())
  
  # Add the agent name to the object
  agent$validation_name <- name
  
  if (!is.null(email_creds_file_path)) {
    agent$email_creds_file_path <- email_creds_file_path
  }
  
  if (!is.null(notification_recipient_emails)) {
    agent$notification_recipients <- notification_recipient_emails
  }
  
  if (notification_emails_active %in% c(TRUE, FALSE)) {
    agent$notification_emails_active <- notification_emails_active
  }
  
  agent$validation_set <-
    agent$validation_set %>%
    dplyr::filter(n == 1)
  
  # Assign the class attribute value `ptblank_agent` to
  # the `agent object`
  attr(agent, "class") <- "ptblank_agent"
  
  agent
}
