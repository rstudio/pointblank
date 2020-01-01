#' Get non-passing sample rows from a validation step
#'
#' Get row data that didn't pass a validation step. The amount of row data
#' available depends on both the fraction of rows that didn't pass a validation
#' step and the level of sampling or explicit collection from that set of rows
#' (this is defined within the [interrogate()] call).
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
#' \dontrun{
#' # Set a seed
#' set.seed(23)
#' 
#' # Create a simple data frame with a
#' # column of numerical values
#' df <-
#'   data.frame(
#'     a = rnorm(100, 5, 2)
#'   )
#' 
#' # Create 2 simple validation steps
#' # that test whether values within
#' # column `a`
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_between(
#'     columns = vars(a),
#'     left = 4, right = 6
#'   ) %>%
#'   col_vals_lte(columns = vars(a), value = 10) %>%
#'   interrogate(
#'     extract_failed = TRUE,
#'     get_first_n = 10
#'   )
#' 
#' # Get row sample data for those rows
#' # in `df` that did not pass the first
#' # validation step (`col_vals_between`)
#' agent %>% get_row_extracts(step = 1)
#' }
#' 
#' @export
get_row_extracts <- function(agent,
                             i = NULL) {

  # Stop function if the agent hasn't
  # yet performed an interrogation
  if (agent$time %>% length() == 0) {
    return(NA)
  }
  
  # Get the number of validation steps
  validation_steps <- nrow(agent$validation_set)
  
  if (is.null(i)) {
    return(agent$extracts)
  }
  
  # Stop function if the `i`th step does not exist in `agent`
  if (!(i %in% seq_len(validation_steps))) {
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
