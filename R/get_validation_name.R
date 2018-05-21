#' Get the pointblank validation name
#' @description Gets the name of a pointblank
#' validation from an agent object. This name
#' is optionally assigned during the
#' \code{create_agent()} call using the
#' \code{validation_name} argument.
#' @param agent an agent object of
#' class \code{ptblank_agent}.
#' @return a character object with the agent
#' name.
#' @export
get_validation_name <- function(agent) {
  
  agent$validation_name
}
