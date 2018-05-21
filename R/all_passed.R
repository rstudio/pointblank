#' Did all of the validations pass?
#' @description Given a completed validation
#' pass, this function will return \code{TRUE}
#' or \code{FALSE} on whether all of the
#' validations passed without any fails. 
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @return an agent object.
#' @export
all_passed <- function(agent) {
  
  if (all(agent$validation_set$all_passed == TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
