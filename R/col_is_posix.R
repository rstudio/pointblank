#' Do the columns contain `POSIXct` dates?
#'
#' Verification step where a table column is expected to consist entirely of
#' `POSIXct` dates.
#'
#' @inheritParams col_vals_gt
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with a
#' # column of `POSIXct` values
#' df <-
#'   data.frame(
#'     a = as.POSIXct(
#'       strptime(
#'         "2011-03-27 01:30:00",
#'         "%Y-%m-%d %H:%M:%S")
#'       )
#'   )
#' 
#' # Validate that column `a` in the
#' # data frame is classed as `POSIXct`
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_is_posix(columns = vars(a)) %>%
#'   interrogate()
#' 
#' # Determine if this column
#' # validation has passed by
#' # using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_is_posix <- function(x,
                         columns,
                         brief = NULL,
                         actions = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_is_posix(
        columns = columns,
        brief = brief,
        actions = prime_actions(actions)
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_is_posix",
        column = columns
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_is_posix",
        column = column,
        preconditions = NULL,
        actions = actions,
        brief = brief
      )
  }

  agent
}
