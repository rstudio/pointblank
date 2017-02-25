#' Determine the course of action for a
#' given verification step.
#' @description Based on a recent judgment,
#' what actions are taken now?
#' @return a tibble with decision results.
#' @importFrom dplyr filter group_by summarize
#' @importFrom tibble tibble as_tibble
#' @export determine_action

determine_action <- function(false_count,
                             report_count,
                             warn_count,
                             notify_count) {
  
  if (false_count >= report_count) {
    report <- TRUE
  } else {
    report <- FALSE
  }
  
  if (false_count >= warn_count) {
    warn <- TRUE
  } else {
    warn <- FALSE
  }
  
  if (false_count >= notify_count) {
    notify <- TRUE
  } else {
    notify <- FALSE
  }
  
  action_df <-
    tibble::tibble(
      report = report,
      warn = warn,
      notify = notify)
  
  return(action_df)
}
