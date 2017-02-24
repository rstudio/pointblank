#' Determine the course of action for a
#' given verification step.
#' @description Based on a recent judgment,
#' what actions are taken now?
#' @return a tibble with decision results.
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @export determine_action

determine_action <- function(judgment,
                             report_count,
                             warn_count,
                             notify_count) {
  
  # Determine which rows do not pass the
  # verification step
  rows_bad <-
    judgment %>%
    dplyr::filter(pb_is_good_ == FALSE) %>%
    nrow()
  
  if (rows_bad >= report_count) {
    report <- TRUE
  } else {
    report <- FALSE
  }
  
  if (rows_bad >= warn_count) {
    warn <- TRUE
  } else {
    warn <- FALSE
  }
  
  if (rows_bad >= notify_count) {
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

