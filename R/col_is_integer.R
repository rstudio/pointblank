#' Do the columns contain integer values?
#'
#' Verification step where a table column is expected to consist of
#' integer values.
#' 
#' @inheritParams col_vals_gt
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame
#' # with a column containing data
#' # classed as `integer`
#' df <-
#'   data.frame(
#'     a = as.integer(c(5, 9, 3))
#'   )
#' 
#' # Validate that column `a` in the
#' # data frame is an `integer`
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_is_integer(columns = vars(a)) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_is_integer <- function(x,
                           columns,
                           brief = NULL,
                           actions = NULL) {
  
  # Capture the `column` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "col_is_integer",
          column = columns,
          value = value,
          preconditions = NULL,
          actions = actions
        )
    )
  }
  
  agent <- x
  
  preconditions <- NULL
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_is_integer",
        column = columns
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_is_integer",
        column = column,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }
  
  agent
}
