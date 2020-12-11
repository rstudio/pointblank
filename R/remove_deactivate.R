#' Remove one or more of an *agent*'s validation steps
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default) then step removal
#'   won't occur by index.
#'
#' @return A `ptblank_agent` object. 
#'
#' @export
remove_steps <- function(agent,
                         i = NULL) {
  
  if (!is.null(i)) {
    agent$validation_set <- 
      agent$validation_set %>%
      dplyr::slice(-{{ i }})
  }
  
  # Renumber steps
  agent$validation_set$i <- 
    as.integer(seq(from = 1, to = nrow(agent$validation_set), by = 1))
  
  # Remove any data extracts
  agent$extracts <- NULL
  
  agent
}

#' Deactivate one or more of an *agent*'s validation steps
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default) then step
#'   deactivation won't occur by index.
#'
#' @return A `ptblank_agent` object.
#'
#' @export
deactivate_steps <- function(agent,
                             i = NULL) {
  
  if (!is.null(i)) {
    agent$validation_set <- 
      agent$validation_set %>%
      dplyr::mutate(active = ifelse(i %in% {{ i }}, list(FALSE), active))
  }
  
  # Remove any data extracts
  agent$extracts <- NULL
  
  agent
}

#' Activate one or more of an *agent*'s validation steps
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default) then step
#'   activation won't occur by index.
#'
#' @return A `ptblank_agent` object.
#'
#' @export
activate_steps <- function(agent,
                           i = NULL) {
  
  if (!is.null(i)) {
    agent$validation_set <- 
      agent$validation_set %>%
      dplyr::mutate(active = ifelse(i %in% {{ i }}, list(TRUE), active))
  }
  
  # Remove any data extracts
  agent$extracts <- NULL
  
  agent
}
