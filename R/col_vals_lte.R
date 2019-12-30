#' Are numerical column data less than or equal to a specific value?
#'
#' Verification step where numeric values in a table column should be less than
#' or equal to a specified value.
#' 
#' @inheritParams col_vals_gt
#' @param value A numeric value used for this test. Any column values `<= value`
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
#'     a = c(5, 4, 3, 5, 1, 2),
#'     b = c(3, 2, 4, 3, 5, 6)
#'   )
#' 
#' # Validate that the sum of
#' # values across columns `a`
#' # and `b` are always less
#' # than or equal to 10
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_lte(
#'     columns = vars(a_b),
#'     value = 10,
#'     preconditions = ~ {
#'       tbl %>% dplyr::mutate(a_b = a + b)
#'     }
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
col_vals_lte <- function(x,
                         columns,
                         value,
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
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "col_vals_lte",
          column = columns,
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
        column = columns,
        value = value
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_lte",
        column = column,
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
