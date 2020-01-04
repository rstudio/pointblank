#' Do one or more columns actually exist?
#'
#' Verification step that checks whether one or several specified columns exist
#' in the target table.
#'
#' @inheritParams col_vals_gt
#' @param columns One or more columns from the table in focus. This can be provided
#'   as a vector of column names using `c()` or bare column names enclosed in
#'   [vars()].
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with
#' # two columns of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' # Validate that columns `a` and `b`
#' # exist in the `df` table
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_exists(columns = vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine if these three validation
#' # steps passed by using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_exists <- function(x,
                       columns,
                       brief = NULL,
                       actions = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (is_a_table_object(x)) {

    warning("The `col_exists()` function is not compatible with a table object",
            call. = FALSE)
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_exists",
        column = columns
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_exists",
        column = column,
        preconditions = NULL,
        actions = actions,
        brief = brief
      )
  }

  agent
}
