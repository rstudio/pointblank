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

test_that("everything() default in `rows_*()` makes yaml roundtrip", {
  
  agent_distinct <- create_agent(~ small_table) %>% 
    rows_distinct()
  agent_complete <- create_agent(~ small_table) %>% 
    rows_complete()
  
  expect_message(yaml_agent_string(agent_distinct), "columns: everything\\(\\)")
  expect_message(yaml_agent_string(agent_complete), "columns: everything\\(\\)")
  
  agent_yaml <- tempfile()
  # everything() makes yaml round trip for `rows_distinct()`
  yaml_write(agent_distinct, expanded = FALSE, filename = agent_yaml)
  expect_identical(
    as_agent_yaml_list(agent_distinct, expanded = FALSE),
    as_agent_yaml_list(yaml_read_agent(agent_yaml), expanded = FALSE)
  )
  # everything() makes yaml round trip for `rows_complete()`
  yaml_write(agent_complete, expanded = FALSE, filename = agent_yaml)
  expect_identical(
    as_agent_yaml_list(agent_complete, expanded = FALSE),
    as_agent_yaml_list(yaml_read_agent(agent_yaml), expanded = FALSE)
  )

})

test_that("complex column selection expressions make the round trip", {
  
  # `expanded = FALSE` preserves complex expr and makes roundtrip
  agent_pre <- create_agent(~ small_table) |> 
    col_vals_lt(
      columns = where(is.numeric) & starts_with("c"),
      value = 8
    )
  
  agent_yaml <- tempfile()
  yaml_write(agent_pre, expanded = FALSE, filename = agent_yaml)
  agent_post <- yaml_read_agent(agent_yaml)
  
  expect_message(yaml_agent_string(agent_pre), 'where\\(is.numeric\\) & starts_with\\("c"\\)')
  expect_message(yaml_agent_string(agent_post), 'where\\(is.numeric\\) & starts_with\\("c"\\)')

  expect_identical(
    as_agent_yaml_list(agent_pre, expanded = FALSE),
    as_agent_yaml_list(agent_post, expanded = FALSE)
  )
  
  expect_identical({
    agent_pre %>% 
      interrogate() %>% 
      get_agent_report(display_table = FALSE)
  }, {
    agent_post %>% 
      interrogate() %>% 
      get_agent_report(display_table = FALSE)
  })
  
  
  # `expanded = TRUE` resolves immediately and works as expected
  agent_pre <- create_agent(~ small_table) |> 
    col_vals_lt(
      columns = where(is.numeric) & starts_with("c"),
      value = 8
    )
  
  agent_yaml <- tempfile()
  yaml_write(agent_pre, expanded = TRUE, filename = agent_yaml)
  agent_post <- yaml_read_agent(agent_yaml)
  
  expect_message(yaml_agent_string(agent_pre, expanded = TRUE), "c\\(c\\)")
  expect_message(yaml_agent_string(agent_post, expanded = TRUE), "c\\(c\\)")
  
  expect_identical(
    as_agent_yaml_list(agent_pre, expanded = TRUE),
    as_agent_yaml_list(agent_post, expanded = TRUE)
  )
  
  expect_identical({
    agent_pre %>% 
      interrogate() %>% 
      get_agent_report(display_table = FALSE)
  }, {
    agent_post %>% 
      interrogate() %>% 
      get_agent_report(display_table = FALSE)
  })
  
})
