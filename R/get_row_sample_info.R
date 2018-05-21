#' Get information on sample rows from non-passing validations
#' @description Get information on which validation
#' steps produced sample rows from non-passing
#' validations.
#' @param agent an agent object of class
#' \code{ptblank_agent}. It should have had
#' \code{interrogate()} called on it, such
#' that the validation steps were carried out
#' and any sample rows from non-passing
#' validations could potentially be available
#' in the object.
#' @examples 
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
#' #>   step   assertion_type n_failed rows_in_sample
#' #> 1    1 col_vals_between       65             10
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom dplyr filter pull
#' @export
get_row_sample_info <- function(agent) {
  
  # Create bindings for specific variables
  component_name <- brief <- NULL
  
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
    mutate(
      brief =
        agent$logical_plan %>%
        dplyr::filter(!(component_name %in% c("create_agent", "focus_on"))) %>%
        dplyr::pull(brief))
  
  # Create a tibble that has the
  # validation step number, the assertion
  # type, the total number of rows that
  # were found to fail the validation step,
  # and the number of non-passing rows available
  # in the object
  row_sample_info <-
    1:validation_steps %>%
    purrr::map_df(.f = function(x) {
      
      if (inherits(validation_set$row_sample[[x]][[1]], "tbl_df")) {
        tibble::tibble(
          step = x,
          tbl = validation_set$tbl_name[x],
          type = validation_set$assertion_type[x],
          n_fail = validation_set$n_failed[x],
          n_sampled = nrow(validation_set$row_sample[[x]][[1]]),
          brief = validation_set$brief[x])
      }})
  
  # Return the output table if there are
  # rows, otherwise return NA
  if (nrow(row_sample_info) == 0) {
    return(NA)
  } else if (nrow(row_sample_info > 0)) {
    return(row_sample_info %>% as.data.frame(stringsAsFactors = FALSE))
  }
}
