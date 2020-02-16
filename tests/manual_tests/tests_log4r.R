library(pointblank)
library(log4r)
library(glue)

# Create a log4r `logger` object
logger <- logger("WARN", appenders = file_appender("log_file"))

# Create a custom logging function to react to any `warn`
# state. The `.vars_list` list is available in the context
# where the function will be evaluated
log4r_warn <- function(x) {
    log4r::warn(logger, glue::glue(
      "Step {x$i} exceeded the `warn` threshold (f_failed = {x$f_failed}) ['{x$name}']"
    ))
}

agent <-
  create_agent(
    tbl = small_table,
    name = "small_table_tests",
    actions = action_levels(warn_at = 3, fns = list(warn = ~log4r_warn(x)))
  ) %>%
  col_vals_gt(vars(d), 1000) %>%
  col_vals_in_set(vars(f), c("low", "high")) %>%
  interrogate()

agent

agent %>% get_data_extracts(i = 1)


