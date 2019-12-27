#' Get a simple summary of the interrogation
#'
#' Gets the essential information from an agent object after an interrogation is
#' complete.
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' 
#' @return Either a \pkg{pointblank} agent object or a table object, depending
#'   on what was passed to `x`.
#' @export
get_interrogation_summary <- function(agent) {
  
  if (did_agent_interrogate(agent)) {
    
    # Get validation set
    validation_set <- 
      agent$validation_set %>%
      dplyr::mutate(
        brief =
          agent$logical_plan %>%
          dplyr::filter(!(component_name %in% c("create_agent", "focus_on"))) %>%
          dplyr::pull(brief)
      )
    
    interrogation_summary <-
      validation_set %>%
      dplyr::select(1:10, f_passed, warn, notify, brief) %>%
      dplyr::mutate(
        action = dplyr::case_when(
          .$warn == FALSE & .$notify == FALSE ~ as.character(NA),
          .$warn == TRUE & .$notify == FALSE ~ "warn",
          .$warn == FALSE & .$notify == TRUE ~ "notify",
          .$warn == TRUE & .$notify == TRUE ~ "notify")) %>%
      dplyr::select(-warn, -notify) %>%
      dplyr::select(1:11, action, brief)
    
    return(interrogation_summary)
    
  } else {
    stop("An interrogation hasn't yet occurred.", call. = FALSE)
  }
}
