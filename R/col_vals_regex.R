#' Do strings in column data match a regex pattern?
#'
#' Verification step where string-based column data should correspond to a regex
#' matching expression.
#'
#' @inheritParams col_vals_gt
#' @param regex A regex pattern to test for matching strings.
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#' 
#' @examples
#' # Create a simple data frame
#' # with a column containing strings
#' df <-
#'   data.frame(
#'     a = c("s_0131", "s_0231",
#'           "s_1389", "s_2300"),
#'     stringsAsFactors = FALSE
#'   )
#' 
#' # Validate that all string values in
#' # column `a` match a regex statement
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_regex(
#'     columns = vars(a),
#'     regex = "^s_[0-9]{4}$"
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
col_vals_regex <- function(x,
                           columns,
                           regex,
                           na_pass = FALSE,
                           preconditions = NULL,
                           brief = NULL,
                           actions = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_vals_regex(
        columns = columns,
        regex = regex,
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
        assertion_type = "col_vals_regex",
        column = columns,
        regex = regex
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_regex",
        column = column,
        regex = regex,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }

  agent
}
