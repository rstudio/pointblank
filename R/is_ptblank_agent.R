#' Is the object a pointblank agent?
#'
#' Determines whether the object is actually a pointbank agent object (i.e., of
#' type `ptblank_agent`).
#'
#' @param object The object to test for whether it is of class `ptblank_agent`.
#'   
#' @return A logical value.
#' @export
is_ptblank_agent <- function(object) {
  
  # Check whether the object has class `ptblank_agent`
  ifelse(inherits(object, "ptblank_agent"), TRUE, FALSE)
}
