test_that("explicit c()-expr make the yaml roundtrip", {
  
  agent_pre <- create_agent(~ small_table) %>% 
    col_vals_lt(
      columns = c(a, c),
      value = 8
    )
  
  agent_yaml <- tempfile()
  yaml_write(agent_pre, expanded = FALSE, filename = agent_yaml)
  # Writes to c()-expr
  expect_true(any(grepl("columns: c(a, c)", readLines(agent_yaml), fixed = TRUE)))
  
  agent_post <- yaml_read_agent(agent_yaml)
  # yaml_agent_string(agent_post, expanded = FALSE)
  
  expect_identical(
    as_agent_yaml_list(agent_pre, expanded = FALSE),
    as_agent_yaml_list(agent_post, expanded = FALSE)
  )
  expect_identical(
    agent_pre %>% interrogate() %>% get_agent_report(display_table = FALSE),
    agent_post %>% interrogate() %>% get_agent_report(display_table = FALSE)
  )
  
  # Defaults writing to c()-expr
  expect_message(
    create_agent(~ small_table) %>% col_exists(a) %>% yaml_agent_string(),
    "columns: c\\(a\\)"
  )
  
})
