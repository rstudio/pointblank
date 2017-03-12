#' Create a simple summary of the
#' interrogation
#' @description Gets the essential information
#' from an agent object after an interrogation
#' is complete.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @return an agent object.
#' @export get_summary

get_summary <- function(agent) {
  
  if (all(agent$validation_set$all_passed %in% c(TRUE, FALSE))) {
    return(agent$validation_set[, c(1:11)])
  } else {
    stop("An interrogation hasn't yet occurred.")
  }
}
