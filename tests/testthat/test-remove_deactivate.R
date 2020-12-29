test_that("activating validation steps can be done with `activate_steps()`", {
  
  # Create an agent with 4 validation steps (using 3 validation function calls)
  # where the 3rd step (based on `col_vals_regex()`) is not active
  agent <- 
    create_agent(
      read_fn = ~ small_table
    ) %>%
    col_exists(vars(date, date_time)) %>%
    col_vals_regex(
      vars(b), "[0-9]-[a-z]{3}-[0-9]{3}", active = FALSE
    ) %>%
    rows_distinct()

  # Expect the third value in `agent$validation_set$active` to be FALSE
  # whilst the others are TRUE
  expect_equal(
    unlist(agent$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  
  agent_n <- activate_steps(agent)
  expect_equal(
    unlist(agent_n$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  agent_1 <- activate_steps(agent, i = 1)
  expect_equal(
    unlist(agent_1$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  agent_2 <- activate_steps(agent, i = 2)
  expect_equal(
    unlist(agent_2$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  agent_3 <- activate_steps(agent, i = 3)
  expect_equal(
    unlist(agent_3$validation_set$active),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  agent_4 <- activate_steps(agent, i = 4)
  expect_equal(
    unlist(agent_4$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  agent_1_4 <- activate_steps(agent, i = 1:4)
  expect_equal(
    unlist(agent_1_4$validation_set$active),
    c(TRUE, TRUE, TRUE, TRUE)
  )
})
