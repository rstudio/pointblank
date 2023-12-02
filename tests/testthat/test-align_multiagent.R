al <- action_levels(0.05, 0.10, 0.20)

agent_1 <-
  create_agent(
    tbl = ~ small_table,
    label = "An example.",
    actions = al
  ) %>%
  # col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  # col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  col_vals_not_between(vars(c), left = 10, right = 20, na_pass = TRUE) %>%
  rows_distinct(vars(d, e, f)) %>%
  col_is_integer(vars(a)) %>%
  interrogate()

test_that("rehashing agent created in current ver generates the same hash", {
  expect_identical(
    agent_1$validation_set$sha1,
    rehash_agent(agent_1)$validation_set$sha1
  )
  expect_identical(
    rehash_agent(agent_1)$validation_set$sha1,
    rehash_agent(agent_1)$validation_set$sha1
  )
})

agent_a <- agent_1
agent_b <- agent_1
agent_a$validation_set$sha1 <- sample(letters, nrow(agent_a$validation_set))
agent_b$validation_set$sha1 <- sample(LETTERS, nrow(agent_b$validation_set))

test_that("rehashing is identical given same validation set", {
  expect_false(identical(
    agent_a$validation_set$sha1,
    agent_b$validation_set$sha1
  ))
  expect_identical(
    rehash_agent(agent_a),
    rehash_agent(agent_b)
  )
})

test_that("multiagent uses rehashing for alignment", {
  multiagent <- create_multiagent(agent_a, agent_b)
  expect_equivalent(
    rehash_agent(agent_a),
    multiagent$agents[[1]]
  )
  expect_equivalent(
    rehash_agent(agent_b),
    multiagent$agents[[2]]
  )
  expect_identical(
    multiagent$agents[[1]]$validation_set$sha1,
    multiagent$agents[[2]]$validation_set$sha1
  )
  multiagent_report <- get_multiagent_report(multiagent, display_table = FALSE)
  expect_equal(nrow(multiagent_report), nrow(agent_1$validation_set))
})
