#' Are numerical column data equal to a specific value?
#'
#' Verification step where numeric values in a table column should be equal to a
#' specified value.
#'
#' @inheritParams col_vals_gt
#' @param value A numeric value used to test for equality.
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#' 
#' @examples
#' # Create a simple data frame with
#' # two columns of numerical values
#' df <-
#'   data.frame(
#'     a = c(1, 1, 1, 2, 2, 2),
#'     b = c(5, 5, 5, 3, 6, 3)
#'   )
#' 
#' # Validate that values in column
#' # `b` are equal to 5 when values
#' # in column `a` are equal to 1 
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_equal(vars(b), 5,
#'     preconditions = ~ tbl %>% dplyr::filter(a == 1)
#'   ) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_vals_equal <- function(x,
                           columns,
                           value,
                           na_pass = FALSE,
                           preconditions = NULL,
                           actions = NULL,
                           brief = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_vals_equal(
        columns = columns,
        value = value,
        na_pass = na_pass,
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
        assertion_type = "col_vals_equal",
        preconditions = preconditions,
        column = columns,
        value = value
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_equal",
        column = column,
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }

  agent
}
