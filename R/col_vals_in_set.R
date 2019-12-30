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
#'     column = vars(a),
#'     set = 1:4
#'   ) %>%
#'   col_vals_in_set(
#'     column = vars(b),
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
                            column,
                            set,
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
          type = "col_vals_in_set",
          column = column,
          set = set,
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
        assertion_type = "col_vals_in_set",
        column = column,
        set = set
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (col in column) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_in_set",
        column = col,
        set = set,
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
