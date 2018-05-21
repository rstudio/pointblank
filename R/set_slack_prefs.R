#' Set Slack credentials and enable Slack notifications
#' @description Grants Slack credentials
#' to a pointblank agent object and
#' provides an opportunity to set Slack
#' reporting options. This function is to
#' be used in a pointblank pipeline any
#' time before an \code{interrogate()} call.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @param notify_active an option to
#' enable Slack notifications whenever any
#' of the validation steps in the
#' \code{agent} object have triggered a
#' \code{notify} status.
#' @param slack_webhook_url the URL that is
#' the endpoint for the API POST request.
#' @param slack_channel the slack channel
#' to which the notification message will
#' be posted.
#' @param slack_username the custom username
#' associated with the webhook integration. 
#' @param slack_author_name an optional
#' author name for the notification. If not
#' provided, then \code{pointblank} will be
#' used as fallback text.
#' @param slack_title the title text for
#' the notification. If not provided, the
#' name of the validation will be used.
#' @param slack_report_url an optional URL
#' for a validation report that is associated
#' with the notification message. The link
#' is embedded in the \code{slack_title} text.
#' @param slack_footer_thumb_url an optional
#' URL that is associated with the thumbnail
#' image in the notification footer.
#' @param slack_footer_text an optional
#' snippet of text that will be part of
#' the notification footer.
#' \code{notify} status.
#' @return an agent object.
#' @examples 
#' \dontrun{
#' # Create a simple data frame
#' # with a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7))
#' 
#' # Create a pointblank `agent`,
#' # set up the Slack notification
#' # preferences, and conduct a
#' # simple validation; because
#' # `notify_count` (in the step
#' # where `col_vals_lt()` is called)
#' # has a value of `1` the
#' # `slack_channel` will be
#' # notified when there are one or
#' # more non-passing validations (in
#' # this case, non-passing rows)
#' agent <-
#'   create_agent() %>%
#'   set_slack_prefs(
#'     notify_active = TRUE,
#'     slack_webhook_url = 
#'       "https://hooks.slack.com/services/XXXXX/XXXXX/XXXXX",
#'     slack_channel = 
#'       "#table-validation",
#'     slack_username = "table_validator",
#'     slack_report_url = 
#'       "https:://my.company.com/reports/df_validation") %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_lt(
#'     column = a,
#'     value = 6,
#'     notify_count = 1) %>%
#'   interrogate()
#' }
#' @export
set_slack_prefs <- function(agent,
                            notify_active = FALSE,
                            slack_webhook_url,
                            slack_channel,
                            slack_username,
                            slack_author_name = NULL,
                            slack_title = NULL,
                            slack_report_url = NULL,
                            slack_footer_thumb_url = NULL,
                            slack_footer_text = NULL) {
  
  if (notify_active %in% c(TRUE, FALSE)) {
    agent$slack_notifications_active <- notify_active
  }
  
  agent$slack_webhook_url <- slack_webhook_url
  agent$slack_channel <- slack_channel
  agent$slack_username <- slack_username
  
  if (is.null(slack_author_name)) {
    agent$slack_author_name <- "pointblank"
  } else {
    agent$slack_author_name <- slack_author_name
  }
  
  if (is.null(slack_title)) {
    agent$slack_title <- agent$validation_name
  } else {
    agent$slack_title <- slack_title
  }
  
  if (is.null(slack_report_url)) {
    agent$slack_report_url <- NA
  } else {
    agent$slack_report_url <- slack_report_url
  }
  
  if (is.null(slack_footer_thumb_url)) {
    agent$slack_footer_thumb_url <- "https://github.com/rich-iannone/pointblank"
  } else {
    agent$slack_footer_thumb_url <- slack_footer_thumb_url
  }
  
  if (is.null(slack_footer_text)) {
    agent$slack_footer_text <- "Validation performed by the pointblank R package"
  } else {
    agent$slack_footer_text <- slack_footer_text
  }
  
  agent
}
