#' Remove one or more of an *agent*'s validation steps
#'
#' Validation steps can be removed from an *agent* object through use of the
#' `remove_steps()` function. This is useful, for instance, when getting an
#' agent from disk (via the [x_read_disk()] function) and omitting one or more
#' steps from the *agent*'s validation plan. Please note that when removing
#' validation steps all stored data extracts will be removed from the *agent*.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default) then step removal
#'   won't occur by index.
#'
#' @return A `ptblank_agent` object.
#' 
#' @seealso Instead of removal, the [deactivate_steps()] function will simply
#'   change the `active` status of one or more validation steps to `FALSE` (and
#'   [activate_steps()] will do the opposite).
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

#' Activate one or more of an *agent*'s validation steps
#'
#' If certain validation steps need to be activated after the creation of the
#' validation plan for an *agent*, use the `activate_steps()` function. This is
#' equivalent to using the `active = TRUE` for the selected validation steps
#' (`active` is an argument in all validation functions). This will replace any
#' function that may have been defined for the `active` argument during creation
#' of the targeted validation steps.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition.
#'
#' @return A `ptblank_agent` object.
#' 
#' @seealso For the opposite behavior, use the [deactivate_steps()] function.
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

#' Deactivate one or more of an *agent*'s validation steps
#'
#' Should the deactivation of one or more validation steps be necessary after
#' creation of the validation plan for an *agent*, the `deactivate_steps()`
#' function will be helpful for that. This has the same effect as using the
#' `active = FALSE` option (`active` is an argument in all validation functions)
#' for the selected validation steps. Please note that this directly edit the
#' validation step, wiping out any function that may have been defined for
#' whether the step should be active or not.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition.
#'
#' @return A `ptblank_agent` object.
#'
#' @seealso For the opposite behavior, use the [activate_steps()] function.
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
