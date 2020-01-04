#' Are column data part of a specific set of values?
#'
#' Verification step where numeric values in a table column should be part of a
#' set of values.
#'   
#' @inheritParams col_vals_gt
#' @param set A vector of numeric or string-based elements, where column values
#'   found within this `set` will be considered as passing.
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with
#' # 2 columns: one with numerical
#' # values and the other with strings
#' df <-
#'   data.frame(
#'     a = c(1, 2, 3, 4),
#'     b = c("one", "two", "three", "four"),
#'     stringsAsFactors = FALSE
#'   )
#' 
#' # Validate that all numerical values
#' # in column `a` belong to a numerical
#' # set, and, create an analogous 
#' # validation check for column `b` with
#' # a set of string values 
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_in_set(
#'     columns = vars(a),
#'     set = 1:4
#'   ) %>%
#'   col_vals_in_set(
#'     columns = vars(b),
#'     set = c("one", "two",
#'             "three", "four")
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
col_vals_in_set <- function(x,
                            columns,
                            set,
                            preconditions = NULL,
                            brief = NULL,
                            actions = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_vals_in_set(
        columns = columns,
        set = set,
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
        assertion_type = "col_vals_in_set",
        column = columns,
        set = set
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_in_set",
        column = column,
        set = set,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }

  agent
}
