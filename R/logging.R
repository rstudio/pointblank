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

# nocov start

#' Enable logging of failure conditions at the validation step level
#' 
#' @description 
#' The `log4r_step()` function can be used as an action in the [action_levels()]
#' function (as a list component for the `fns` list). Place a call to this
#' function in every failure condition that should produce a log (i.e., `warn`,
#' `stop`, `notify`). Only the failure condition with the highest severity for a
#' given validation step will produce a log entry (skipping failure conditions
#' with lower severity) so long as the call to `log4r_step()` is present.
#' 
#' @param x A reference to the x-list object prepared by the `agent`. This
#'   version of the x-list is the same as that generated via
#'   `get_agent_x_list(<agent>, i = <step>)` except this version is internally
#'   generated and hence only available in an internal evaluation context.
#' @param message The message to use for the log entry. When not provided, a
#'   default glue string is used for the messaging. This is dynamic since the
#'   internal `glue::glue()` call occurs in the same environment as `x`, the
#'   x-list that's constrained to the validation step. The default message, used
#'   when `message = NULL` is the glue string `"Step {x$i} exceeded the {level}
#'   failure threshold (f_failed = {x$f_failed}) ['{x$type}']"`. As can be seen,
#'   a custom message can be crafted that uses other elements of the x-list with
#'   the `{x$<component>}` construction.
#' @param append_to The file to which log entries at the warn level are
#'   appended. This can alternatively be one or more **log4r** appenders.
#' 
#' @return Nothing is returned however log files may be written in very specific
#'   conditions.
#' 
#' @examples 
#' # We can create an `action_levels`
#' # object that has a threshold for
#' # the `warn` state, and, an
#' # associated function that should
#' # be invoked whenever the `warn`
#' # state is entered. Here, the
#' # function call with `log4r_step()`
#' # will be invoked whenever there
#' # is one failing test unit. It's
#' # important to match things up here;
#' # notice that `warn_at` is given a
#' # threshold and the list of functions
#' # given to `fns` has a `warn` component
#' al <-
#'   action_levels(
#'     warn_at = 1,
#'     fns = list(
#'       warn = ~ log4r_step(
#'         x, append_to = "example_log"
#'       )
#'     )
#'   )
#' 
#' # Printing `al` will show us the
#' # settings for the
#' # `action_levels` object:
#' al
#' 
#' # Let's create an agent with
#' # `small_table` as the target
#' # table, apply the `action_levels`
#' # object created above as `al`,
#' # add two validation steps, and
#' # then `interrogate()` the data
#' agent <- 
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(d), 300) %>%
#'   col_vals_in_set(
#'     vars(f), c("low", "high")
#'   ) %>%
#'   interrogate()
#' 
#' # From the agent report, we can
#' # see that both steps have yielded
#' # warnings upon interrogation
#' # (i.e., filled yellow circles
#' # in the `W` column).
#' 
#' # We can see this more directly
#' # by inspecting the `warn`
#' # component of the agent's x-list:
#' get_agent_x_list(agent)$warn
#' 
#' # Upon entering the `warn` state
#' # in each validation step during
#' # interrogation, the `log4r_step()`
#' # function call was invoked! This
#' # will generate an `"example_log"`
#' # file in the working directory
#' # and log entries will be appended
#' # to the file
#' 
#' if (file.exists("example_log")) {
#'   file.remove("example_log")
#' }
#' 
#' @family Logging
#' @section Function ID:
#' 5-1
#' 
#' @export
log4r_step <- function(x,
                       message = NULL,
                       append_to = "pb_log_file") {
  
  if (!requireNamespace("log4r", quietly = TRUE)) {
    
    stop(
      "Using the `log4r_step()` function requires the log4r package:\n",
      "* It can be installed with `install.packages(\"log4r\")`.",
      call. = FALSE
    )
  }
  
  type <- x$this_type
  warn_val <- x$warn
  stop_val <- x$stop
  notify_val <- x$notify

  log4r_fn_present <-
    vapply(
      c("warn", "stop", "notify"),
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      FUN = function(y) {
        grepl(
          "log4r_step(x",
          paste(
            as.character(x$actions[[paste0("fns.", y)]]),
            collapse = ""
          ),
          fixed = TRUE
        )
      }
    )
  level <- toupper(type)
  
  level_val <-
    switch(
      level,
      "WARN" = 3,
      "STOP" = 4,
      "NOTIFY" = 5,
      3
    )

  # Skip logging at this level if a higher severity
  # condition is present for this validation step *and*
  # there is a `log4r_step()` function ready for
  # evaluation at those higher severities
  if (warn_val   && log4r_fn_present[1]) highest_level <- 3
  if (stop_val   && log4r_fn_present[2]) highest_level <- 4
  if (notify_val && log4r_fn_present[3]) highest_level <- 5
  
  if (highest_level > level_val) {
    return(invisible(NULL))
  }
  
  if (is.character(append_to)) {
    appenders <- log4r::file_appender(file = append_to[1])
  }

  logger <- log4r::logger(appenders = appenders)
  
  log4r::levellog(
    logger = logger,
    level = level_val,
    message = glue::glue(
      "Step {x$i} exceeded the {level} failure threshold \\
      (f_failed = {x$f_failed}) ['{x$type}']"
    )
  )
}

# nocov end
