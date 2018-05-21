#' Is the object a pointblank agent?
#' @description Determines whether the object is
#' actually a pointbank agent object (i.e., of
#' type \code{ptblank_agent}).
#' @param object the object to test for whether
#' it is of class \code{ptblank_agent}.
#' @return an logical value.
#' @export
is_ptblank_agent <- function(object) {
  
  # Check whether the object has class `ptblank_agent`
  ifelse(inherits(object, "ptblank_agent"), TRUE, FALSE)
}
