#' Collect data extracts from a validation step
#'
#' Get data that didn't pass a validation step. The amount of data available in
#' a particular extract depends on both the fraction of validation units that
#' didn't pass a validation step and the level of sampling or explicit
#' collection from that set of units (this is defined within the [interrogate()]
#' call).
#'
#' @param agent An agent object of class `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were carried
#'   out and any sample rows from non-passing validations could potentially be
#'   available in the object.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition.
#' 
#' @return A list of tibbles if `i` is not provided, or, a tibble if `i` is
#'   given.
#' 
#' @examples
#' library(dplyr)
#' 
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- tibble(a = c(5, 7, 8, 5))
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
#' @family Interrogate and Get Info
#' @section Function ID:
#' 3-4
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
