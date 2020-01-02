library(pointblank)
library(tidyverse)

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

agent <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(
    vars(d), 1000,
    actions = action_levels(warn_at = 8)
  ) %>%
  interrogate()

agent %>% get_agent_report()

agent_intel <- agent %>% interrogate()

agent_intel

agent_report <- agent_intel %>% get_agent_report()

agent_report

extract_1 <- agent_intel %>% get_data_extracts(i = 1)

extract_1
