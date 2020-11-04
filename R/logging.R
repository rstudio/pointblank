#' Enable logging of WARN conditions at the validation step level
#' 
#' The `log4r_step()` function can be used as an action in the [action_levels()]
#' function (as a list component for the `fns` list).
#' 
#' @param x A reference to the x-list object prepared by the agent. This version
#'   of the x-list is the same as that generated via
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
  
  type <- x$this_type
  warn_val <- x$warn
  stop_val <- x$stop
  notify_val <- x$notify
  
  warn_log <- 
    grepl(
      "log4r_step(x", paste(as.character(x$actions$fns.warn), collapse = ""),
      fixed = TRUE
    )
  stop_log <- 
    grepl(
      "log4r_step(x", paste(as.character(x$actions$fns.stop), collapse = ""),
      fixed = TRUE
    )
  notify_log <- 
    grepl(
      "log4r_step(x", paste(as.character(x$actions$fns.notify), collapse = ""),
      fixed = TRUE
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
  
  if (warn_val && warn_log) highest_level <- 3
  if (stop_val && stop_log) highest_level <- 4
  if (notify_val && notify_log) highest_level <- 5
  
  # TODO: skip logging at this level if a higher severity condition is present
  if (highest_level > level_val) {
    return(NULL)
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
