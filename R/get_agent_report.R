#' Get a simple summary of the interrogation
#'
#' Gets the essential information from an agent object after an interrogation is
#' complete.
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' 
#' @return A tibble.
#'   
#' @export
get_agent_report <- function(agent) {
  
  validation_set <- agent$validation_set
  
  columns <- 
    validation_set$column %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) paste(x, collapse = ", ")
    )
  
  value <- 
    validation_set$value %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = as.character
    )
  
  set <- 
    validation_set$set %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        if (is.null(x)) {
          NA_character_
        } else {
          paste(x, collapse = ", ")
        }
      } 
    )
  
  has_preconds <-
    validation_set$preconditions %>%
    vapply(
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      FUN = function(x) if (is.null(x)) FALSE else TRUE
    )

  if (!has_agent_intel(agent)) {
    has_extract <- rep(NA, nrow(validation_set))
  } else {
    has_extract <- as.character(validation_set[["i"]]) %in% names(agent$extracts)
  }

  state <-
    validation_set %>%
    dplyr::select(warn, notify) %>%
    dplyr::mutate(state = dplyr::case_when(
      warn == FALSE & notify == FALSE ~ "OK",
      warn == TRUE  & notify == FALSE ~ "WARN",
      warn == FALSE & notify == TRUE  ~ "NOTIFY",
      warn == TRUE  & notify == TRUE  ~ "NOTIFY"
    )) %>%
    dplyr::pull(state)
  
  dplyr::tibble(
    i = validation_set$i,
    j = validation_set$j,
    type = validation_set$assertion_type,
    columns = columns,
    value = validation_set$value,
    set = set,
    regex = validation_set$regex,
    preconds = has_preconds,
    units = validation_set$n,
    n_pass = validation_set$n_passed,
    f_pass = validation_set$f_passed,
    state = state,
    extract = has_extract
  )
}
