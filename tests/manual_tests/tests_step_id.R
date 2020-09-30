library(pointblank)

al <- action_levels(warn_at = 0.1, stop_at = 0.2)

# Automatically generated `step_id` values
agent <-
  small_table %>%
  create_agent(actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  col_exists(vars(a, b, c, d))

agent$validation_set$step_id
# [1] "0001" "0002" "0003" "0004" "0005" "0006"

# Mixture of automatically generated `step_id` values and
# user-provided values
agent <-
  small_table %>%
  create_agent(actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  col_exists(vars(a, b, c, d), step_id = "exists")

agent$validation_set$step_id
# [1] "0001" "0002" "exists.0001" "exists.0002" "exists.0003" "exists.0004"

# This validation plan will stop since duplicate `step_id` values
# are seen across validation steps
agent <-
  small_table %>%
  create_agent(actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE, step_id = "dupe") %>%
  col_exists(vars(a), step_id = "dupe")
