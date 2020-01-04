#' Are numerical column data less than a specific value?
#'
#' Verification step where numeric values in a table column should be less than
#' a specified value.
#' 
#' @inheritParams col_vals_gt
#' @param value A numeric value used for this test. Any column values `< value`
#'   are considered passing.
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with
#' # a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 4, 3, 5, 1, 2)
#'   )
#' 
#' # Validate that values in
#' # column `a` are always less
#' # than 6
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_lt(columns = vars(a), value = 6) %>%
#'   interrogate()
#' 
#' # Determine if this column 
#' # validation has passed by using
#' # `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_vals_lt <- function(x,
                        columns,
                        value,
                        incl_na = FALSE,
                        preconditions = NULL,
                        brief = NULL,
                        actions = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_vals_lt(
        columns = columns,
        value = value,
        incl_na = incl_na,
        preconditions = preconditions,
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
        assertion_type = "col_vals_lt",
        preconditions = preconditions,
        column = columns,
        value = value)
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_lt",
        column = column,
        value = value,
        incl_na = incl_na,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }

  agent
}
