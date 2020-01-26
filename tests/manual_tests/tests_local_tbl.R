library(pointblank)
library(tidyverse)

agent_plan <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f)) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE)

agent_plan %>% get_agent_report()

agent_intel <- agent_plan %>% interrogate()

agent_intel

agent_report <- agent_intel %>% get_agent_report()

agent_report

extract_1 <- agent_intel %>% get_data_extracts(i = 1)

extract_1
