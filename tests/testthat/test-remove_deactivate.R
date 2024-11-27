# Create an agent with 4 validation steps (using 3 validation function calls)
# where the 3rd step (based on `col_vals_regex()`) is not active
agent <-
  create_agent(tbl = ~ small_table) %>%
  col_exists(vars(date, date_time)) %>%
  col_vals_regex(
    vars(b), "[0-9]-[a-z]{3}-[0-9]{3}", active = FALSE
  ) %>%
  rows_distinct()

test_that("activating validation steps can be done with `activate_steps()`", {

  # Expect the third value in `agent$validation_set$active` to be FALSE
  # whilst the others are TRUE
  expect_equal(
    unlist(agent$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # Perform several expectations when cycling through values for `i`
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
  agent_1_6 <- activate_steps(agent, i = 1:6)
  expect_equal(
    unlist(agent_1_6$validation_set$active),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  agent_9 <- activate_steps(agent, i = 9)
  expect_equal(
    unlist(agent_9$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("deactivating steps is possible with `deactivate_steps()`", {

  # Expect the third value in `agent$validation_set$active` to be FALSE
  # whilst the others are TRUE
  expect_equal(
    unlist(agent$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # Perform several expectations when cycling through values for `i`
  agent_n <- deactivate_steps(agent)
  expect_equal(
    unlist(agent_n$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  agent_1 <- deactivate_steps(agent, i = 1)
  expect_equal(
    unlist(agent_1$validation_set$active),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  agent_2 <- deactivate_steps(agent, i = 2)
  expect_equal(
    unlist(agent_2$validation_set$active),
    c(TRUE, FALSE, FALSE, TRUE)
  )
  agent_3 <- deactivate_steps(agent, i = 3)
  expect_equal(
    unlist(agent_3$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  agent_4 <- deactivate_steps(agent, i = 4)
  expect_equal(
    unlist(agent_4$validation_set$active),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  agent_1_4 <- deactivate_steps(agent, i = 1:4)
  expect_equal(
    unlist(agent_1_4$validation_set$active),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  agent_1_6 <- deactivate_steps(agent, i = 1:6)
  expect_equal(
    unlist(agent_1_6$validation_set$active),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  agent_9 <- deactivate_steps(agent, i = 9)
  expect_equal(
    unlist(agent_9$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )

  # Test whether an activated step can be deactivated
  agent_3_cycle <-
    agent %>%
    activate_steps(i = 3) %>%
    deactivate_steps(i = 3)
  expect_equal(
    unlist(agent_3_cycle$validation_set$active),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("removing steps is possible with `remove_steps()`", {

  agent_n <- remove_steps(agent)
  expect_equal(nrow(agent_n$validation_set), 4)
  expect_equal(agent_n$validation_set$i, 1:4)

  agent_1 <- remove_steps(agent, i = 1)
  expect_equal(nrow(agent_1$validation_set), 3)
  expect_equal(agent_1$validation_set$i, 1:3)
  expect_equal(
    agent_1$validation_set$assertion_type,
    c("col_exists", "col_vals_regex", "rows_distinct")
  )

  agent_2 <- remove_steps(agent, i = 2)
  expect_equal(nrow(agent_2$validation_set), 3)
  expect_equal(agent_2$validation_set$i, 1:3)
  expect_equal(
    agent_2$validation_set$assertion_type,
    c("col_exists", "col_vals_regex", "rows_distinct")
  )

  agent_3 <- remove_steps(agent, i = 3)
  expect_equal(nrow(agent_3$validation_set), 3)
  expect_equal(agent_3$validation_set$i, 1:3)
  expect_equal(
    agent_3$validation_set$assertion_type,
    c("col_exists", "col_exists", "rows_distinct")
  )

  agent_4 <- remove_steps(agent, i = 4)
  expect_equal(nrow(agent_4$validation_set), 3)
  expect_equal(agent_4$validation_set$i, 1:3)
  expect_equal(
    agent_4$validation_set$assertion_type,
    c("col_exists", "col_exists", "col_vals_regex")
  )
})
