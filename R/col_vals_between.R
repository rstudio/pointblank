#' Are numerical column data between two specified values?
#'
#' Verification step where column data should be between two values.
#'
#' @inheritParams col_vals_gt
#' @param left The lower bound for the range. The validation includes this bound
#'   value in addition to values greater than `left`.
#' @param right The upper bound for the range. The validation includes this
#'   bound value in addition to values lower than `right`.
#' @param inclusive A two-element logical value that indicates whether the
#'   `left` and `right` bounds should be inclusive. By default, both bounds
#'   are inclusive.
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
#' # Validate that values in
#' # column `a` are all between
#' # 1 and 9
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_between(
#'     columns = vars(a),
#'     left = 1, right = 9
#'   ) %>%
#'   interrogate()
#' 
#' # Determine if this column
#' # validation has passed by using
#' # `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_vals_between <- function(x,
                             columns,
                             left,
                             right,
                             inclusive = c(TRUE, TRUE),
                             incl_na = FALSE,
                             preconditions = NULL,
                             brief = NULL,
                             warn_count = NULL,
                             stop_count = NULL,
                             notify_count = NULL,
                             warn_fraction = NULL,
                             stop_fraction = NULL,
                             notify_fraction = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  left <- stats::setNames(left, inclusive[1])
  right <- stats::setNames(right, inclusive[2])
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {

    return(
      x %>%
        evaluate_single(
          type = "col_vals_between",
          column = columns,
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
        assertion_type = "col_vals_between",
        column = columns,
        left = left,
        right = right
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_between",
        column = column,
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
