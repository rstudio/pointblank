#' Do the columns contain R `factor` objects?
#'
#' Verification step where a table column is expected to consist entirely of R
#' `factor` objects.
#'
#' @inheritParams col_vals_gt
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'
#' @examples
#' # Create a simple data frame with
#' # a column of `factor` values
#' df <-
#'   data.frame(
#'     a = c("one", "two")
#'   )
#' 
#' # Validate that column `a`
#' # in the data frame is classed
#' # as `factor`
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_is_factor(columns = vars(a)) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_is_factor <- function(x,
                          columns,
                          brief = NULL,
                          actions = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_is_factor(
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
        assertion_type = "col_is_factor",
        column = columns
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_is_factor",
        column = column,
        preconditions = NULL,
        actions = actions,
        brief = brief
      )
  }

  agent
}
