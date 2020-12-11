#' Remove one or more of an *agent*'s validation steps
#'
#' @export
remove_steps <- function(x,
                         i = NULL) {
  
  if (!is.null(i)) {
    x$validation_set <- 
      x$validation_set %>%
      dplyr::slice(-{{ i }})
  }
  
  # Renumber steps
  x$validation_set$i <- 
    as.integer(seq(from = 1, to = nrow(x$validation_set), by = 1))
  
  # Remove any data extracts
  x$extracts <- NULL
  
  x
}

#' Deactivate one or more of an *agent*'s validation steps
#'
#' @export
deactivate_steps <- function(x,
                             i = NULL) {
  
  
}

