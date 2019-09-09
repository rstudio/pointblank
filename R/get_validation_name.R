#' Get the pointblank validation name
#'
#' Gets the name of a pointblank validation from an agent object. This name is
#' optionally assigned during the [create_agent()] call using the
#' `validation_name` argument.
#'
#' @param agent An agent object of class `ptblank_agent`.
#'
#' @return A character object with the agent name.
#' @export
get_validation_name <- function(agent) {
  
  agent$validation_name
}
