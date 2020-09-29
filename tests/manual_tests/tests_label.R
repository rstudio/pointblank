library(pointblank)

al <- action_levels(warn_at = 0.1, stop_at = 0.2)

agent <-
  small_table %>%
  create_agent(actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(
    vars(b), vars(g), na_pass = TRUE,
    label = "b > g"
  ) %>%
  rows_distinct(
    vars(d, e, f),
    label = "Distinct rows across 'd', 'e', and 'f'"
  ) %>%
  col_is_integer(
    vars(a),
    label = "`a` must be an integer"
  ) %>%
  interrogate()

agent
