#' Did all of the validations fully *pass*?
#'
#' Given an agent's validation plan that had undergone interrogation via
#' `interrogate()`, did every single validation step result in zero *fail*
#' levels? Using the `all_passed()` function will let us know whether that's
#' `TRUE` or not.
#'
#' @param agent An agent object of class `ptblank_agent`.
#'
#' @return A logical value.
#' 
#' @examples
#' # Create a simple data frame with
#' # a column of numerical values
#' df <- data.frame(a = c(5, 7, 8, 5))
#' 
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_gt(vars(a), 4) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @export
all_passed <- function(agent) {
  
  if (!has_agent_intel(agent)) {
    stop("The agent hasn't performed an interrogation.", call. = FALSE)
  }
  
  if (all(agent$validation_set$all_passed == TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
