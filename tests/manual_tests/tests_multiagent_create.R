library(pointblank)
library(tidyverse)

al <- action_levels(0.05, 0.10, 0.20)

agent_1 <-
  create_agent(
    read_fn = ~ small_table,
    label = "An example.",
    actions = al
  ) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  col_vals_not_between(vars(c), left = 10, right = 20, na_pass = TRUE) %>%
  rows_distinct(vars(d, e, f)) %>%
  col_is_integer(vars(a)) %>%
  interrogate()

agent_2 <- 
  create_agent(
    read_fn = ~ small_table,
    label = "An example.",
    actions = al
  ) %>%
  col_exists(vars(date, date_time)) %>%
  col_vals_regex(
    vars(b), "[0-9]-[a-z]{3}-[0-9]{3}",
    active = FALSE
  ) %>%
  rows_distinct() %>%
  interrogate()


agent_3 <- 
  create_agent(
    read_fn = ~ small_table,
    label = "An example.",
    actions = al
  ) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_lte(vars(c), 5) %>%
  col_vals_equal(
    vars(d), vars(d),
    na_pass = TRUE
  ) %>%
  col_vals_in_set(
    vars(f),
    set = c("low", "mid", "high")
  ) %>%
  col_vals_between(
    vars(c),
    left = vars(a), right = vars(d),
    na_pass = TRUE
  ) %>%
  interrogate()

agent_4 <-
  agent_3 %>%
  remove_steps(i = 1) %>%
  deactivate_steps(i = 1) %>%
  interrogate()

agent_5 <-
  create_agent(
    read_fn = ~ small_table,
    label = "An example.",
    actions = al
  ) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  interrogate()

multiagent <-
  create_multiagent(agent_1, agent_2, agent_3, agent_5)

# 4UP report
multiagent

get_multiagent_report(multiagent)

# 8UP report
create_multiagent(agent_1, agent_2, agent_3, agent_2, agent_3)

# 16UP report
create_multiagent(agent_1, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3)

# 16+ report
create_multiagent(
  agent_1, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3,
  agent_1, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3,
  agent_1, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3, agent_2, agent_3
)
