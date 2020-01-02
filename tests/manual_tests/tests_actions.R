library(pointblank)
library(tidyverse)

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

# Create a `failures` vector, which can serve as a log
failures <- c()

# Create a custom logging (to a vector) function
# The `.vars_list` list is available in the context
# where the function will be evaluated; note that we
# need the double-back arrow to assign to `failures`
# (which is on the top-level)
add_fails <- function(vl) {
  failures <<- 
    glue::glue(
      "Step {vl$i} failed (f_failed = {vl$f_failed}) ['{vl$name}' at {vl$time}]") %>%
    append(failures, .)
}

agent <-
  create_agent(tbl = small_table, name = "small_table_tests") %>%
  col_vals_gt(
    vars(d), 1000,
    actions = action_levels(warn_at = 3, fns = list(warn = ~add_fails(vl = .vars_list))
    )
  ) %>%
  col_vals_in_set(
    vars(f), c("low", "high"),
    actions = action_levels(warn_at = 0.1, fns = list(warn = ~add_fails(vl = .vars_list))
    )
  ) %>%
  interrogate()

# We now have a custom set of logs in a vector
failures

agent %>% get_agent_report()

agent_intel <- agent %>% interrogate()

agent_intel

agent_report <- agent_intel %>% get_agent_report()

agent_report

extract_1 <- agent_intel %>% get_data_extracts(i = 1)

extract_1
