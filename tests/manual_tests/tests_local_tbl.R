library(pointblank)
library(tidyverse)

agent_plan <-
  create_agent(tbl = small_table) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f)) %>%
  col_vals_gt(vars(d), 100)

agent_plan %>% get_agent_report()

agent_intel <- agent_plan %>% interrogate()

agent_intel

agent_report <- agent_intel %>% get_agent_report()

agent_report

extract_1 <- agent_intel %>% get_data_extracts(i = 1)

extract_1
