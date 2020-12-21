#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#

#' Do columns in the table contain anomalous values?
#'
#' The `col_anomaly_check()` validation function, the
#' `expect_col_anomaly_check()` expectation function, and the
#' `test_col_anomaly_check()` test function all check whether column values
#' contain anomalous values in a time series. Because this family of functions
#' only currently supports a time-series analysis for the detection of
#' anomalies, the `x_column` must contain date-time values and the `y_column`
#' must contain numeric values. The validation function can be used directly on
#' a data table or with an *agent* object (technically, a `ptblank_agent`
#' object) whereas the expectation and test functions can only be used with a
#' data table. The types of data tables that can be used include data frames,
#' tibbles, database tables (`tbl_dbi`), and Spark DataFrames (`tbl_spark`).
#' Each validation step or expectation will operate over the number of test
#' units that is equal to the number of rows in the table (after any
#' `preconditions` have been applied).
#'
#' @inheritParams col_vals_gt
#' @param x_column The column that contains the *x* values. This should be a
#'   date-time column because, currently, the `col_anomaly_check()` function
#'   only supports anomaly detection as part of a time-series analysis.
#' @param y_column The column that contains the *y* values, which must be
#'   numeric.
#'
#' @family validation functions
#'
#' @name col_anomaly_check
NULL

#' @rdname col_anomaly_check
#' @import rlang
#' @export
col_anomaly_check <- function(x,
                              x_column,
                              y_column,
                              preconditions = NULL,
                              actions = NULL,
                              step_id = NULL,
                              label = NULL,
                              brief = NULL,
                              active = TRUE) {
  
  x_column <- rlang::enquo(x_column)
  y_column <- rlang::enquo(y_column)
  
  # Resolve the columns based on the expression
  x_column <- resolve_columns(x = x, var_expr = x_column, preconditions)
  y_column <- resolve_columns(x = x, var_expr = y_column, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      col_anomaly_check(
        x_column = x_column,
        y_column = y_column,
        preconditions = preconditions,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_anomaly_check"
      )
  }

  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add a validation step
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_anomaly_check",
      column = x_column,
      values = y_column,
      preconditions = preconditions,
      actions = covert_actions(actions, agent),
      step_id = step_id,
      label = label,
      brief = brief,
      active = active
    )
}
