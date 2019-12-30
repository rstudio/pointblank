#' Are column data `NULL`?
#'
#' Verification step where all values in a table column are expected to be
#' `NULL`.
#'
#' @inheritParams col_vals_gt
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' library(dplyr)
#' 
#' # Create a simple data frame with
#' # two columns of numerical values
#' df <-
#'   data.frame(
#'     a = c(1, 2, NA, NA),
#'     b = c(2, 2, 5, 5),
#'     stringsAsFactors = FALSE
#'   )
#' 
#' # Validate that all values in
#' # column `a` are NULL when
#' # values in column `b` are
#' # equal to 5
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_null(
#'     columns = vars(a),
#'     preconditions = ~ tbl %>% dplyr::filter(b >= 5)
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
col_vals_null <- function(x,
                          columns,
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
          type = "col_vals_null",
          column = columns,
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
        assertion_type = "col_vals_null",
        column = columns
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_null",
        column = column,
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
