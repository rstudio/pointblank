library(pointblank)

al <- action_levels(warn_at = 0.1, stop_at = 0.2)

agent <- 
  create_agent(tbl = small_table, read_fn = ~ small_table, actions = al) %>%
  col_vals_in_set(vars(f), set = c("low", "mid", "high")) %>%
  col_vals_between(vars(a), 2, 8) %>%
  col_vals_lt(vars(a), vars(d), na_pass = TRUE, preconditions = ~ . %>% dplyr::filter(a > 3)) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_null(vars(c)) %>%
  col_vals_regex(vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}") %>%
  col_is_character(vars(b)) %>%
  col_exists(vars(a, b)) %>%
  col_vals_expr(expr(a %% 1 == 0)) %>%
  rows_distinct(vars(a, b, c)) %>%
  rows_distinct() %>%
  col_schema_match(
    schema = col_schema(
      date_time = c("POSIXct", "POSIXt"),
      date = "Date",
      a = "integer",
      b = "character",
      c = "numeric",
      d = "numeric",
      e = "logical",
      f = "character"
    )
  ) %>%
  conjointly(
    ~ col_vals_lt(., vars(a), 8),
    ~ col_vals_gt(., vars(c), vars(a)),
    ~ col_vals_not_null(., vars(b))
  ) %>%
  col_vals_between(vars(c), left = 2.03, right = vars(d), na_pass = TRUE)

as_agent_yaml_list(agent)

agent_yaml_string(agent) %>% cat()

agent_yaml_write(agent, "test.yaml")

agent <- agent_yaml_read(file = "test.yaml")

# TODO: Adapt `agent_yaml_string` to show a yaml string from a file
# agent_yaml_string(file = "test.yaml") %>% yaml::as.yaml() %>% cat()

# TODO: Create `agent_yaml_show_exprs()` to show the expression pipeline
# in the console (similar to `dput()`'s console output)
agent_yaml_show_exprs(file = "test.yaml") #%>% cat()

agent_file <- agent_yaml_interrogate(file = "test.yaml")

agent_file
