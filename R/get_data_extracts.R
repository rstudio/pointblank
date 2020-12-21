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


#' Collect data extracts from a validation step
#'
#' @description
#' In an agent-based workflow, after interrogation with [interrogate()] we can
#' get the row data that didn't pass row-based validation steps with the
#' `get_data_extracts()` function. The amount of data available in a particular
#' extract depends on both the fraction of test units that didn't pass a
#' validation step and the level of sampling or explicit collection from that
#' set of units.
#'
#' The availability of data extracts for each row-based validation step is
#' depends on whether `extract_failed` is set to `TRUE` within the
#' [interrogate()] call (it is by default). The amount of *fail* rows extracted
#' depends on the collection parameters in [interrogate()], and the default
#' behavior is to collect up to the first 5000 *fail* rows.
#'
#' Row-based validation steps are based on the validation functions of the form
#' `col_vals_*()` and also include [conjointly()] and [rows_distinct()]. Only
#' those types of validation steps can provide data extracts.
#'
#' @param agent An agent object of class `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were carried
#'   out and any sample rows from non-passing validations could potentially be
#'   available in the object.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default), all data extract
#'   tables will be provided in a list object.
#' 
#' @return A list of tables if `i` is not provided, or, a standalone table if
#'   `i` is given.
#' 
#' @examples
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- 
#'   dplyr::tibble(a = c(5, 7, 8, 5))
#' 
#' # Create 2 simple validation steps
#' # that test whether values within
#' # column `a`
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_between(vars(a), 4, 6) %>%
#'   col_vals_lte(vars(a), 7) %>%
#'   interrogate(
#'     extract_failed = TRUE,
#'     get_first_n = 10
#'   )
#' 
#' # Get row sample data for those rows
#' # in `tbl` that did not pass the first
#' # validation step (`col_vals_between`)
#' agent %>% get_data_extracts(i = 1)
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-2
#' 
#' @export
get_data_extracts <- function(agent,
                              i = NULL) {

  # Stop function if the agent hasn't
  # yet performed an interrogation
  if (!inherits(agent, "has_intel")) {
    stop("The `agent` has not yet performed an interrogation.", call. = FALSE)
  }
  
  # Get the number of validation steps
  validation_steps <- unique(agent$validation_set$i)
  
  if (is.null(i)) {
    return(agent$extracts)
  }
  
  # Stop function if the `i`th step does not exist in `agent`
  if (!(i %in% seq(validation_steps))) {
    stop("The provided step number does not exist.", call. = FALSE)
  }
  
  # Get the names of the extracts
  extract_names <- names(agent$extracts)
  
  # Stop function if the `i`th step does not have an extract available
  if (!(as.character(i) %in% extract_names)) {
    stop("The provided step number does not have an associated extract.",
         call. = FALSE)
  }
  
  # Get the data extract
  agent$extracts[[as.character(i)]]
}
