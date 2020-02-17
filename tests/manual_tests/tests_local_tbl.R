library(pointblank)
library(tidyverse)

al <- action_levels(warn_at = 0.1, stop_at = 0.2)

agent <-
  create_agent(tbl = small_table, actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE)

agent <- agent %>% interrogate()
agent

extracts <- agent %>% get_data_extracts()
extracts

x_list <- get_agent_x_list(agent)
x_list

email_blast_preview(agent)

get_sundered_data(agent)
