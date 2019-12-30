#' Do the columns contain character/string data?
#'
#' Verification step where a table column is expected to consist of string data.
#'
#' @inheritParams col_vals_gt
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with
#' # a column of `character` values
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
#'   col_is_character(column = vars(a)) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
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
  
  # Capture the `column` expression
  column <- rlang::enquo(column)
  
  # Resolve the columns based on the expression
  column <- resolve_columns(x = x, var_expr = column, preconditions = NULL)
  
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
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (col in column) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_is_character",
        column = col,
        preconditions = preconditions,
        brief = brief,
        warn_count = warn_count,
        stop_count = stop_count,
        notify_count = notify_count,
        warn_fraction = warn_fraction,
        stop_fraction = stop_fraction,
        notify_fraction = notify_fraction
      )
  }

  agent
}
