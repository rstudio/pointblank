#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Activate one or more of an *agent*'s validation steps
#'
#' @description 
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
#' @examples 
#' # Create an agent that has the
#' # `small_table` object as the
#' # target table, add a few inactive
#' # validation steps, and then use
#' # `interrogate()`
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>%
#'   col_exists(
#'     vars(date),
#'     active = FALSE
#'   ) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}",
#'     active = FALSE
#'   ) %>%
#'   interrogate()
#' 
#' # In the above, the data is
#' # not actually interrogated
#' # because the `active` setting
#' # was `FALSE` in all steps; we
#' # can selectively change this
#' # with `activate_steps()`
#' agent_2 <-
#'   agent_1 %>%
#'   activate_steps(i = 1) %>%
#'   interrogate()
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-5
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
#' @description
#' Should the deactivation of one or more validation steps be necessary after
#' creation of the validation plan for an *agent*, the `deactivate_steps()`
#' function will be helpful for that. This has the same effect as using the
#' `active = FALSE` option (`active` is an argument in all validation functions)
#' for the selected validation steps. Please note that this directly edits the
#' validation step, wiping out any function that may have been defined for
#' whether the step should be active or not.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition.
#'
#' @return A `ptblank_agent` object.
#' 
#' @examples 
#' # Create an agent that has the
#' # `small_table` object as the
#' # target table, add a few
#' # validation steps, and then use
#' # `interrogate()`
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>%
#'   col_exists(vars(date)) %>%
#'   col_vals_regex(
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]"
#'   ) %>%
#'   interrogate()
#'   
#' # The second validation step is
#' # now being reconsidered and may
#' # be either phased out or improved
#' # upon; in the interim period it
#' # was decided that the step should
#' # be deactivated for now
#' agent_2 <-
#'   agent_1 %>%
#'   deactivate_steps(i = 2) %>%
#'   interrogate()
#'
#' @family Object Ops
#' @section Function ID:
#' 9-6
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

#' Remove one or more of an *agent*'s validation steps
#'
#' @description
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
#' @examples 
#' # Create an agent that has the
#' # `small_table` object as the
#' # target table, add a few
#' # validation steps, and then use
#' # `interrogate()`
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>%
#'   col_exists(vars(date)) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]"
#'   ) %>%
#'   interrogate()
#'   
#' # The second validation step has
#' # been determined to be unneeded and
#' # is to be removed; this can be done
#' # by used `remove_steps()` with the
#' # agent object
#' agent_2 <-
#'   agent_1 %>%
#'   remove_steps(i = 2) %>%
#'   interrogate()
#'
#' @return A `ptblank_agent` object.
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-7
#' 
#' @seealso Instead of removal, the [deactivate_steps()] function will simply
#'   change the `active` status of one or more validation steps to `FALSE` (and
#'   [activate_steps()] will do the opposite).
#'
#' @export
remove_steps <- function(agent,
                         i = NULL) {
  
  # TODO: Allow for removal of multiple steps (e.g., `i = 1:3`)
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
