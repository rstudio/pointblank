library(pointblank)

agent <-
  create_agent(
    read_fn = ~ small_table,
    actions = action_levels(warn_at = 0.1, stop_at = 0.2)
  ) %>%
  col_vals_gt(
    vars(date_time), vars(date),
    na_pass = TRUE
  ) %>%
  col_vals_gt(
    vars(b), vars(g), na_pass = TRUE,
    label = "b > g"
  ) %>%
  col_is_character(
    vars(b, f),
    label = "Verifying character-type columns" 
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
