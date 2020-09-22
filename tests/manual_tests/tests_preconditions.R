library(pointblank)
library(tidyverse)

al <- action_levels(warn_at = 0.1, stop_at = 0.2)

agent <-
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "Tests of Preconditions",
    actions = al,
    lang = "it"
  ) %>%
  col_vals_gt(vars(g), 100, preconditions = ~ . %>% dplyr::mutate(g = a + 95)) %>%
  col_vals_lt(vars(c), vars(d), preconditions = ~ . %>% dplyr::mutate(d = d - 200)) %>%
  col_vals_in_set(vars(f), c("low", "mid", "high", "higher")) %>%
  col_vals_not_in_set(vars(f), LETTERS[1:8]) %>%
  col_vals_between(vars(c), 0, 10) %>%
  col_vals_not_between(vars(c), 100, 500) %>%
  col_vals_regex(vars(b), ".-...-...") %>%
  col_vals_not_null(vars(a)) %>%
  col_is_date(vars(date)) %>%
  rows_distinct(columns = vars(a, b, c, d, e, f)) %>%
  rows_distinct() %>%
  conjointly(
    ~ col_vals_in_set(., vars(f), c("low", "mid", "high", "higher")),
    ~ col_vals_not_in_set(., vars(f), LETTERS[1:8]),
    ~ col_vals_between(., vars(c), 0, 10)
  )

agent <- agent %>% interrogate()
agent

report_all <- get_agent_report(agent)
report_all

report_severe <- get_agent_report(agent, arrange_by = "severity")
report_severe

report_severe_trunc <- 
  get_agent_report(
    agent,
    arrange_by = "severity",
    keep = "fail_states"
  )
report_severe_trunc

extracts <- agent %>% get_data_extracts()
extracts

x_list <- get_agent_x_list(agent)
x_list

email_create(agent)
