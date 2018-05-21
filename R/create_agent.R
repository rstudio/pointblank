#' Create a pointblank agent object
#' @description Creates an agent object.
#' @param validation_name an optional name
#' for the validation pipeline that the
#' agent will eventually carry out during
#' the interrogation process. If no
#' value is provided, a name will be
#' generated based on the current system
#' time.
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
#' #> # A tibble: 1 x 7
#' #>   tbl_name  db_type assertion_type column value regex all_passed
#' #>      <chr>    <chr>          <chr>  <chr> <dbl> <chr>      <lgl>
#' #> 1       df local_df    col_vals_gt      a     4  <NA>       TRUE
#' @return an agent object.
#' @importFrom dplyr filter
#' @importFrom tibble tibble as_tibble
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
      validation_name = as.character(NA)[-1],
      validation_time = as.POSIXct(NA)[-1],
      focal_tbl_name = as.character(NA)[-1],
      focal_file_name = as.character(NA)[-1],
      focal_db_type = as.character(NA)[-1],
      focal_col_names = as.character(NA)[-1],
      focal_col_types = as.character(NA)[-1],
      focal_db_cred_file_path = as.character(NA)[-1],
      focal_db_env_vars = list(),
      focal_init_sql = as.character(NA)[-1],
      email_creds_file_path = as.character(NA)[-1],
      email_notification_recipients = as.character(NA)[-1],
      email_notifications_active = FALSE,
      slack_webhook_url = as.character(NA)[-1],
      slack_channel = as.character(NA)[-1],
      slack_username = as.character(NA)[-1],
      slack_author_name = as.character(NA)[-1],
      slack_title = as.character(NA)[-1],
      slack_report_url = as.character(NA)[-1],
      slack_footer_thumb_url = as.character(NA)[-1],
      slack_footer_text = as.character(NA)[-1],
      slack_notifications_active = FALSE,
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
  agent$validation_name <- validation_name
  
  agent$validation_set <-
    agent$validation_set %>%
    dplyr::filter(n == 1)
  
  # Assign the class attribute value `ptblank_agent` to
  # the `agent object`
  attr(agent, "class") <- "ptblank_agent"
  
  agent
}
