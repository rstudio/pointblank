#' Print the agent information to the console
#'
#' This function will allow the agent to provide a summary report.
#' 
#' @param x An agent object of class `ptblank_agent`.
#' 
#' @keywords internal
#' @export
print.ptblank_agent <- function(x, ...) {
  
  # nocov start 
  
  args <- list(...)
  args <- NULL
  
  # Get the console width
  console_width <- getOption("width")

  # Generate the complete statement for printing
  if (is_agent_empty(x)) {
    
    print_stmt <- paste0("pointblank agent // <", x$name, ">")
    
  } else {
    
    print_stmt <-
      paste0(
        "pointblank agent // <", x$name, ">", "\n", "\n",
        "number of validation steps: ", number_of_validation_steps(x), "\n"
      )
    
    if (has_agent_intel(x)) {
      
      passing_steps <-
        length(which(x$validation_set$all_passed == TRUE))
      
      failing_steps <-
        length(which(x$validation_set$all_passed == FALSE))
      
      print_stmt <-
        paste0(
          print_stmt, "\n",
          "interrogation (",
          interrogation_time(x), ") resulted in:", "\n",
          "  - ", 
          ifelse(passing_steps > 0, as.character(passing_steps), "no"),
          " passing validation",
          ifelse(passing_steps == 1, "", "s"),
          "\n",
          "  - ", 
          ifelse(failing_steps > 0, as.character(failing_steps), "no"),
          " failing validation",
          ifelse(failing_steps == 1, "", "s"),
          "   ",
          "more info: `get_interrogation_summary()`\n"
        )
    }
  }
  
  cat(print_stmt)
  
  # nocov end 
}
