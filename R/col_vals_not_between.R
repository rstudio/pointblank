#' Are numerical column data not between two specified values?
#'
#' Verification step where column data should not be between two values.
#'
#' @inheritParams col_vals_gt
#' @param left,right The lower and uppers bounds for the range. The validation
#'   Any values `>= left` and `<= right` will be considered as failing.
#' @inheritParams col_vals_between
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with
#' # a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5.6, 8.2, 6.3, 7.8, 3.4)
#'   )
#' 
#' # Validate that none of the values 
#' # in column `a` are between 9 and 10,
#' # or, between 0 and 2
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_not_between(
#'     column = vars(a),
#'     left = 9, right = 10
#'   ) %>%
#'   col_vals_not_between(
#'     column = vars(a),
#'     left = 0, right = 2
#'   ) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed by
#' # using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_vals_not_between <- function(x,
                                 column,
                                 left,
                                 right,
                                 inclusive = c(TRUE, TRUE),
                                 incl_na = TRUE,
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
  
  left <- stats::setNames(left, inclusive[1])
  right <- stats::setNames(right, inclusive[2])
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {

    return(
      x %>%
        evaluate_single(
          type = "col_vals_not_between",
          column = column,
          left = left,
          right = right,
          incl_na = incl_na,
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
        assertion_type = "col_vals_not_between",
        column = column,
        left = left,
        right = right
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (col in column) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_not_between",
        column = col,
        set = c(left, right),
        incl_na = incl_na,
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
