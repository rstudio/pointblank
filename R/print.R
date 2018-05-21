#' Print the agent to the terminal
#' @description This function will allow the
#' agent to provide a summary report.
#' @param x agent an agent object of class
#' \code{ptblank_agent}.
#' @keywords internal
#' @importFrom dplyr mutate select group_by summarize n ungroup transmute pull
#' @export
print.ptblank_agent <- function(x, ...) {
  
  args <- list(...)
  args <- NULL
  
  # Create bindings for specific variables
  tbl_name <- db_type <- tbl_name_type <- tbl_name_type_n <- NULL
  
  # Get the console width
  console_width <- getOption("width")
  
  # Get the tables focussed on
  tables_of_focus <-
    x$validation_set %>%
    dplyr::mutate(tbl_name_type = paste0(tbl_name, "/", db_type)) %>%
    dplyr::select(tbl_name_type) %>%
    dplyr::group_by(tbl_name_type) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(tbl_name_type_n = paste0(tbl_name_type, " (", n, ")")) %>%
    dplyr::pull(tbl_name_type_n)
  
  if (length(tables_of_focus) == 0 & 
      length(x$focal_tbl_name) != 0) {
    tables_of_focus <- paste0(x$focal_tbl_name, "/", x$focal_db_type, " (1)") 
  }
  
  # Generate the complete statement for printing
  if (is_agent_empty(x)) {
    
    print_stmt <-
      paste0(
        "pointblank agent // <", x$validation_name, ">")
    
  } else {
    
    print_stmt <-
      paste0(
        "pointblank agent // <", x$validation_name, ">", "\n", "\n",
        "tables of focus: ",
        paste(tables_of_focus, collapse = ", "),
        ".", "\n",
        "number of validation steps: ", number_of_validation_steps(x), "\n")
    
    if (did_agent_interrogate(x)) {
      
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
          "more info: `get_interrogation_summary()`")
    }
  }
  
  cat(print_stmt)
}
