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
#' @param step The validation step number, which is assigned to each validation
#'   step in the order of definition. To determine which validation steps
#'   produced sample row data, one can use the [get_row_sample_info()] function.
#'   The data frame output provides the step number and the number of rows in
#'   the sample.
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
#'     a = rnorm(
#'       n = 100,
#'       mean = 5,
#'       sd = 2))
#' 
#' # Create 2 simple validation steps
#' # that test whether values within
#' # column `a`
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_between(
#'     column = a,
#'     left = 4,
#'     right = 6) %>%
#'   col_vals_lte(
#'     column = a,
#'     value = 10) %>%
#'   interrogate(
#'     get_problem_rows = TRUE,
#'     get_first_n = 10)
#'   
#' # Find out which validation steps
#' # contain sample row data
#' get_row_sample_info(agent)
#' 
#' # Get row sample data for those rows
#' # in `df` that did not pass the first
#' # validation step (`col_vals_between`);
#' # the leading column `pb_step_` is
#' # applied to provide context on the
#' # validation step for which these rows
#' # failed to pass 
#' agent %>%
#'   get_row_sample_data(step = 1)
#' }
#' 
#' @export
get_row_sample_data <- function(agent,
                                step) {
  
  # Stop function if the agent hasn't
  # yet performed an interrogation
  if (agent$validation_time %>% length() == 0) {
    return(NA)
  }
  
  # Get the number of validation steps
  validation_steps <- nrow(agent$validation_set)
  
  # Stop function if the step number
  # does not exist in `agent`
  if (!(step %in% 1:validation_steps)) {
    stop("The provided step number does not exist in this `agent` object.")
  }
  
  # Extract a tibble of non-passing table
  # rows associated with the step number provided
  if (!is.null(agent$row_samples) && step %in% agent$row_samples$pb_step_) {
    
    return(
      agent$row_samples %>%
        dplyr::filter(pb_step_ == step)
      )
  } else {
    return(NA)
  }
}
