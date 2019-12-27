#' Do the columns contain character/string data?
#'
#' Verification step where a table column is expected to consist of string data.
#'
#' @inheritParams col_vals_gt
#' @param column The name of a single table column, multiple columns in the same
#'   table, or, a helper function such as [all_cols()].
#'   
#' @examples
#' # Create a simple data frame
#' # with a column containing data
#' # classed as `character`
#' df <-
#'   data.frame(
#'     a = c("one", "two"),
#'     stringsAsFactors = FALSE
#'   )
#' 
#' # Validate that column `a`
#' # in the data frame is classed
#' # as `character`
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_is_character(column = a) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @return Either a \pkg{pointblank} agent object or a table object, depending
#'   on what was passed to `x`.
#' @import rlang
#' @export
col_is_character <- function(x,
                             column,
                             brief = NULL,
                             warn_count = NULL,
                             stop_count = NULL,
                             notify_count = NULL,
                             warn_fraction = NULL,
                             stop_fraction = NULL,
                             notify_fraction = NULL) {
  
  # Get the column name
  column <- 
    rlang::enquo(column) %>%
    rlang::expr_text() %>%
    stringr::str_replace_all("~", "") %>%
    stringr::str_replace_all("\"", "'")
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "col_is_character",
          column = column,
          value = value,
          preconditions = NULL,
          warn_count = warn_count,
          stop_count = stop_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          stop_fraction = stop_fraction,
          notify_fraction = notify_fraction
        )
    )
  }
  
  agent <- x
  
  preconditions <- NULL
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_is_character",
        column = column
      )
  }
  
  # If "*" is provided for `column`, select all
  # table columns for this verification
  if (column[1] == "all_cols()") {
    column <- get_all_cols(agent = agent)
  }
  
  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_is_character",
      column = column,
      preconditions = preconditions,
      brief = brief,
      warn_count = warn_count,
      stop_count = stop_count,
      notify_count = notify_count,
      warn_fraction = warn_fraction,
      stop_fraction = stop_fraction,
      notify_fraction = notify_fraction
    )
  
  # If no `brief` provided, set as NA
  if (is.null(brief)) {
    brief <- as.character(NA)
  }
  
  # Place the validation step in the logical plan
  agent$logical_plan <-
    dplyr::bind_rows(
      agent$logical_plan,
      dplyr::tibble(
        component_name = "col_is_character",
        parameters = as.character(NA),
        brief = brief
      )
    )
  
  agent
}
