library(pointblank)
library(log4r)
library(glue)

# Here, we add a custom logging function to react to any `warn`
# state (this goes in `action_levels()` -> `fns` -> list(warn = ~ {.}).)

agent <-
  create_agent(
    tbl = small_table,
    read_fn = ~small_table,
    label = "small_table_tests",
    actions = action_levels(
      warn_at = 3,
      fns = list(
        warn = ~ {
          # Create a log4r `logger` object
          logger <- logger("WARN", appenders = file_appender("log_file"))
          
          # Call the `warn()` function; the `glue()` statement has access to any
          # parameter values available in the agent's x-list (use `x$<param>`
          # with confidence; use the `get_agent_x_list()` function to see
          # what is in there)
          log4r::warn(logger, glue::glue(
            "Step {x$i} exceeded the `warn` threshold (f_failed = {x$f_failed}) ['{x$name}']"
          ))
        }
      )
    )
  ) %>%
  col_vals_gt(vars(d), 1000) %>%
  col_vals_in_set(vars(f), c("low", "high"))

agent %>% yaml_write(filename = "test_log4r.yaml")

yaml_agent_string(filename = "test_log4r.yaml")

yaml_agent_show_exprs(filename = "test_log4r.yaml")

agent_read <- yaml_read_agent(filename = "test_log4r.yaml")

agent_intel <- yaml_agent_interrogate(filename = "test_log4r.yaml")
agent_intel
