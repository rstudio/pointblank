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
  step_id <- normalize_step_id(step_id, columns, agent)
  
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
