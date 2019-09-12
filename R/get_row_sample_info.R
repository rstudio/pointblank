#' Get information on sample rows from non-passing validations
#'
#' Get information on which validation steps produced sample rows from
#' non-passing validations.
#'
#' @param agent An agent object of class `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were carried
#'   out and any sample rows from non-passing validations could potentially be
#'   available in the object.
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
#' }
#' 
#' @export
get_row_sample_info <- function(agent) {
  
  # Return NA if the agent hasn't
  # yet performed an interrogation
  if (did_agent_interrogate(agent) == FALSE |
      is.na(did_agent_interrogate(agent))) {
    return(NA)
  }
  
  # Get the number of validation steps
  validation_steps <- nrow(agent$validation_set)
  
  # Get the validation set and integrate
  # the available briefs to it
  validation_set <- 
    agent$validation_set %>%
    dplyr::mutate(
      brief =
        agent$logical_plan %>%
        dplyr::filter(!(component_name %in% c("create_agent", "focus_on"))) %>%
        dplyr::pull(brief)
    )
  
  # Create a tibble that has the
  # validation step number, the assertion
  # type, the total number of rows that
  # were found to fail the validation step,
  # and the number of non-passing rows available
  # in the object
  row_sample_info <-
    seq_len(validation_steps) %>%
    purrr::map_df(.f = function(x) {
      
      if (!is.null(agent$row_samples) && x %in% agent$row_samples$pb_step_) {
        n_sampled <- agent$row_samples %>%
          dplyr::filter(pb_step_ == x) %>%
          nrow()
      } else {
        n_sampled <- 0
      }
      
      dplyr::tibble(
        step = x,
        tbl = validation_set$tbl_name[x],
        type = validation_set$assertion_type[x],
        n_fail = validation_set$n_failed[x],
        n_sampled = n_sampled,
        brief = validation_set$brief[x]
      )
      
    })
  
  # Return the output table if there are
  # rows, otherwise return NA
  if (nrow(row_sample_info) == 0) {
    return(NA)
  } else if (nrow(row_sample_info > 0)) {
    return(row_sample_info)
  }
}
