library(pointblank)

al <- action_levels(warn_at = 0.1, stop_at = 0.2)

agent <-
  create_agent(
    tbl = small_table,
    tbl_name = "pointblank::small_table",
    label = "A local table test.",
    actions = al
  ) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  col_vals_regex(vars(b), "[1-9]-[a-z]{3}-[0-9]{3}") %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  col_vals_not_between(vars(c), left = 10, right = 20, na_pass = TRUE) %>%
  rows_distinct(vars(d, e, f)) %>%
  col_is_integer(vars(a)) %>%
  interrogate()

agent

get_agent_report(agent, arrange_by = "severity")

get_agent_report(agent, arrange_by = "severity", keep = "fail_states")

get_agent_report(agent, title = "Validation Report: `small_table`")

extracts <- agent %>% get_data_extracts()

x_list <- get_agent_x_list(agent)

email <- email_create(agent)

sundered <- get_sundered_data(agent)
