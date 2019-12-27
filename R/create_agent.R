#' Create a pointblank agent object
#'
#' Creates an agent object.
#'
#' @param validation_name An optional name for the validation pipeline that the
#'   agent will eventually carry out during the interrogation process. If no
#'   value is provided, a name will be generated based on the current system
#'   time.
#'   
#' @examples 
#' # Create a simple data frame
#' # with a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7))
#' 
#' # Create a pointblank `agent` object
#' agent <- create_agent()
#'
#' # Then, as with any `ptblank_agent`
#' # object, we can focus on a table,
#' # add validation steps, and then
#' # eventually use `interrogate()`
#' # to perform the validations;
#' # here, in a single validation
#' # step, we expect that values in
#' # column `a` are always greater
#' # than 4
#' agent <-
#'   agent %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_gt(
#'     column = a,
#'     value = 4) %>%
#'   interrogate()
#'  
#' # A summary can be produced using
#' # `get_interrogation_summary()`; we
#' # we will just obtain the first
#' # 7 columns of its output
#' (agent %>%
#'   get_interrogation_summary())[, 1:7]
#'   
#' @return A \pkg{pointblank} agent object.
#' @export
create_agent <- function(validation_name = NULL) {
  
  # Generate an agent name if none provided
  if (is.null(validation_name)) {
    validation_name <- paste0("agent_", gsub(" ", "_", Sys.time() %>% as.character()))
    brief <- "Create agent with auto-assigned validation name"
  } else {
    brief <- "Create agent with an assigned validation name"
  }
  
  # Create the agent list object
  agent <-
    list(
      validation_name = character(0),
      validation_time = as.POSIXct(NA)[-1],
      focal_tbl_name = character(0),
      focal_file_name = character(0),
      focal_db_type = character(0),
      focal_col_names = character(0),
      focal_col_types = character(0),
      focal_db_cred_file_path = character(0),
      focal_db_env_vars = list(),
      focal_init_sql = character(0),
      email = 
        list(
          email_creds_file_path = character(0),
          email_notification_recipients = character(0),
          email_notifications_active = FALSE
        ),
      slack = 
        list(
          slack_webhook_url = character(0),
          slack_channel = character(0),
          slack_username = character(0),
          slack_author_name = character(0),
          slack_title = character(0),
          slack_report_url = character(0),
          slack_footer_thumb_url = character(0),
          slack_footer_text = character(0),
          slack_notifications_active = FALSE
        ),
      logical_plan =
        dplyr::tibble(
          component_name = as.character("create_agent"),
          parameters = as.character(NA),
          brief = brief
        ),
      validation_set =
        dplyr::tibble(
          tbl_name = character(0),
          db_type = character(0),
          assertion_type = character(0),
          column = character(0),
          value = numeric(0),
          set = list(NULL),
          regex = character(0),
          all_passed = logical(0),
          n = integer(0),
          n_passed = integer(0),
          n_failed = integer(0),
          f_passed = numeric(0),
          f_failed = numeric(0),
          warn_count = numeric(0),
          stop_count = numeric(0),
          notify_count = numeric(0),
          warn_fraction = numeric(0),
          stop_fraction = numeric(0),
          notify_fraction = numeric(0),
          warn = logical(0),
          notify = logical(0),
          row_sample = numeric(0),
          init_sql = character(0),
          db_cred_file_path = character(0),
          file_path = character(0),
          col_types = character(0),
          time_processed = as.POSIXct(NA)[-1],
          proc_duration_s = numeric(0)
        ),
      preconditions = list()
    )
  
  # Add the agent name to the object
  agent$validation_name <- validation_name
  
  # Assign the class attribute value `ptblank_agent` to
  # the `agent object`
  attr(agent, "class") <- "ptblank_agent"
  
  agent
}
