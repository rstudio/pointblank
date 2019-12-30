#' Do one or more columns actually exist?
#'
#' Verification step that checks whether one or several specified columns exist
#' in the target table.
#'
#' @inheritParams col_vals_gt
#' @param cols One or more columns from the table in focus. This can be provided
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
#'   cols_exist(cols = vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine if these three validation
#' # steps passed by using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
cols_exist <- function(x,
                       cols,
                       brief = NULL,
                       warn_count = NULL,
                       stop_count = NULL,
                       notify_count = NULL,
                       warn_fraction = NULL,
                       stop_fraction = NULL,
                       notify_fraction = NULL) {
  
  # Capture the `column` expression
  cols <- rlang::enquo(cols)
  
  # Resolve the columns based on the expression
  cols <- resolve_columns(x = x, var_expr = cols, preconditions = NULL)
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "cols_exist",
          column = cols,
          value = value,
          preconditions = NULL,
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
  
  preconditions <- NULL
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "cols_exist",
        column = cols
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (col in cols) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "cols_exist",
        column = col,
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
