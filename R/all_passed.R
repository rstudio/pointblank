#' Did all of the validations pass?
#'
#' Given a completed validation pass, this function will return `TRUE` or
#' `FALSE` on whether all of the validations passed without any fails.
#'
#' @param agent An agent object of class `ptblank_agent`.
#'
#' @return A logical value.
#' @export
all_passed <- function(agent) {
  
  if (all(agent$validation_set$all_passed == TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
