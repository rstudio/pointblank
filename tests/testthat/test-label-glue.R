test_that("label supports glue syntax for {.segment} {.step} {.col}", {
  
  # Reprex from (#451) for {.segment}
  agent1 <- small_table |> 
    create_agent() |> 
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "The `col_vals_lt()` step for group '{.segment}'"
    ) |> 
    interrogate()
  expect_identical(
    gsub(".*(high|low|mid).*", "\\1", agent1$validation_set$label),
    c("high", "low", "mid")
  )
  
  # {.step}
  agent2 <- small_table |> 
    create_agent() |> 
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "{.step}"
    ) |> 
    interrogate()
  expect_true(all(agent2$validation_set$label == "col_vals_lt"))
  
  # {.col}
  agent3 <- small_table |> 
    create_agent() |> 
    col_vals_lt(
      columns = matches("^[acd]$"),
      value = 8,
      label = "{.col}"
    ) |> 
    interrogate()
  expect_identical(agent3$validation_set$label, c("a", "c", "d"))
  
  # Only those three internal values are available inside the glue mask
  agent4 <- create_agent(small_table) %>%
    col_vals_lt(
      c, 8,
      label = "{toString(sort(ls(all.names = TRUE)))}"
    )
  expect_identical(agent4$validation_set$label, c(".col, .segment, .step"))
  
})

test_that("glue scope doesn't expose internal variables", {
  
  # Ex: should not be able to access `columns` local variable in `col_vals_lt()`
  expect_error(create_agent(small_table) %>% col_vals_lt(c, 8, label = "{columns}"))
  # Ex: should not be able to access `i` local variable in `create_validation_step()`
  expect_error(create_agent(small_table) %>% col_vals_lt(c, 8, label = "{i}"))
  
  # Should be able to access global vars/fns
  expect_equal(
    create_agent(small_table) %>%
      col_vals_lt(c, 8, label = "{match(.col, letters)}") %>% 
      {.$validation_set$label},
    "3"
  )
  
})

test_that("glue env searches from the caller env of the validation function", {
  
  to_upper <- function(x) stop("Oh no!")
  
  expect_error(
    create_agent(small_table) %>%
      col_vals_lt(c, 8, label = "{to_upper(.col)}") %>% 
      {.$validation_set$label},
    "Oh no!"
  )
  
  expect_equal(
    local({
      to_upper <- function(x) toupper(x)
      create_agent(small_table) %>%
        col_vals_lt(c, 8, label = "{to_upper(.col)}") %>% 
        {.$validation_set$label}
    }),
    "C"
  )
  
})

test_that("materialized multi-length glue labels make the yaml roundtrip", {
  
  agent_pre <- create_agent(~ small_table) |> 
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "The `col_vals_lt()` step for group '{.segment}'"
    )
  
  yaml_agent_string(agent_pre, expanded = FALSE)
  
  agent_yaml <- tempfile()
  yaml_write(agent_pre, expanded = FALSE, filename = agent_yaml)
  
  agent_post <- yaml_read_agent(agent_yaml)
  yaml_agent_string(agent_post, expanded = FALSE)
  
  expect_identical(
    as_agent_yaml_list(agent_pre, expanded = FALSE),
    as_agent_yaml_list(agent_post, expanded = FALSE)
  )
  expect_identical(
    agent_pre %>% interrogate() %>% get_agent_report(display_table = FALSE),
    agent_post %>% interrogate() %>% get_agent_report(display_table = FALSE)
  )
  
})

