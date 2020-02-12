#' Print the agent information to the console
#'
#' This function will allow the agent to print a useful report.
#' 
#' @param x An agent object of class `ptblank_agent`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_agent <- function(x, ...) {
  
  # nocov start 
  
  print(get_agent_report(x))
  
  # nocov end 
}
