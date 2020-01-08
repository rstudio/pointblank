library(pointblank)
library(tidyverse)
library(log4r)

# Create a log4r `logger` object
logger <- logger("WARN", appenders = file_appender("log_file"))

# Create a custom logging function to react to any `warn`
# state. The `.vars_list` list is available in the context
# where the function will be evaluated
log4r_warn <- function(vl) {
    log4r::warn(logger, glue::glue(
      "Step {vl$i} exceeded the `warn` threshold (f_failed = {vl$f_failed}) ['{vl$name}']"
    ))
}

agent <-
  create_agent(tbl = small_table, name = "small_table_tests") %>%
  col_vals_gt(
    vars(d), 1000,
    actions = action_levels(warn_at = 3, fns = list(warn = ~logger_warn(.vars_list))
    )
  ) %>%
  col_vals_in_set(
    vars(f), c("low", "high"),
    actions = action_levels(warn_at = 0.1, fns = list(warn = ~logger_warn(.vars_list))
    )
  ) %>%
  interrogate()

agent %>% get_agent_report()

agent %>% get_data_extracts(i = 1)


