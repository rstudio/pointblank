#' Enable logging of failure conditions at the validation step level
#' 
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
#' @export
log4r_step <- function(x,
                       message = NULL,
                       append_to = "pb_log_file") {
  
  # nocov start
  
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
  
  # nocov end
}
