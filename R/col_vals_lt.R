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
#'   col_vals_lt(column = vars(a), value = 6) %>%
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
                        column,
                        value,
                        preconditions = NULL,
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
  column <- resolve_columns(x = x, var_expr = column, preconditions)
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "col_vals_lt",
          column = column,
          value = value,
          preconditions = preconditions,
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
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_gt",
        preconditions = preconditions,
        column = column,
        value = value)
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (col in column) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_lt",
        column = col,
        value = value,
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
