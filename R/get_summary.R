#' Get a simple summary of the interrogation
#' @description Gets the essential information
#' from an agent object after an interrogation
#' is complete.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @return an agent object.
#' @importFrom dplyr select mutate case_when
#' @export get_summary

get_summary <- function(agent) {
  
  # Create bindings for specific variables
  f_passed <- warn <- notify <- NULL
  
  if (all(agent$validation_set$all_passed %in% c(TRUE, FALSE))) {
    
    validation_summary <-
      agent$validation_set %>%
      dplyr::select(1:9, f_passed, warn, notify) %>%
      dplyr::mutate(
        action = dplyr::case_when(
           .$warn == FALSE & .$notify == FALSE ~ as.character(NA),
           .$warn == TRUE & .$notify == FALSE ~ "warn",
           .$warn == FALSE & .$notify == TRUE ~ "notify",
           .$warn == TRUE & .$notify == TRUE ~ "notify")) %>%
      dplyr::select(-warn, -notify)
    
    return(validation_summary)
  } else {
    stop("An interrogation hasn't yet occurred.")
  }
}
