#' Get a simple summary of the interrogation
#' @description Gets the essential information
#' from an agent object after an interrogation
#' is complete.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @return an agent object.
#' @importFrom dplyr select mutate case_when
#' @export get_interrogation_summary

get_interrogation_summary <- function(agent) {
  
  # Create bindings for specific variables
  f_passed <- warn <- notify <- NULL
  
  if (did_agent_interrogate(agent)) {
    
    # Get validation set
    validation_set <- 
      agent$validation_set %>%
      mutate(
        brief =
          agent$logical_plan %>%
          filter(!(component_name %in% c("create_agent", "focus_on"))) %>%
          pull(brief))
    
    interrogation_summary <-
      validation_set %>%
      dplyr::select(1:9, f_passed, warn, notify, brief) %>%
      dplyr::mutate(
        action = dplyr::case_when(
           .$warn == FALSE & .$notify == FALSE ~ as.character(NA),
           .$warn == TRUE & .$notify == FALSE ~ "warn",
           .$warn == FALSE & .$notify == TRUE ~ "notify",
           .$warn == TRUE & .$notify == TRUE ~ "notify")) %>%
      dplyr::select(-warn, -notify)
    
    return(interrogation_summary)
    
  } else {
    stop("An interrogation hasn't yet occurred.")
  }
}
