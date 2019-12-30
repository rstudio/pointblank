#' Get a simple summary of the interrogation
#'
#' Gets the essential information from an agent object after an interrogation is
#' complete.
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' 
#' @return A tibble.
#'   
#' @export
get_interrogation_summary <- function(agent) {

  if (did_agent_interrogate(agent)) {
    
    interrogation_summary <-
      agent$validation_set %>%
      dplyr::select(
        assertion_type, column, value, set, regex,
        preconditions, all_passed, n, f_passed, n_passed, f_passed,
        warn, notify, brief
      ) %>%
      dplyr::mutate(
        action = dplyr::case_when(
          .$warn == FALSE & .$notify == FALSE ~ as.character(NA),
          .$warn == TRUE & .$notify == FALSE ~ "warn",
          .$warn == FALSE & .$notify == TRUE ~ "notify",
          .$warn == TRUE & .$notify == TRUE ~ "notify")
      ) %>%
      dplyr::select(
        assertion_type, column, value, set, regex,
        preconditions, all_passed, n, f_passed, n_passed, f_passed,
        action, brief
      )
    
    return(interrogation_summary)
    
  } else {
    stop("An interrogation hasn't yet occurred.", call. = FALSE)
  }
}
