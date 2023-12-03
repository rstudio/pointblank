agent <- create_agent(small_table)
align <- function(...) {
  intel_agents <- suppressMessages(lapply(list(...), interrogate))
  multiagent <- do.call(create_multiagent, intel_agents)
  get_multiagent_report(multiagent, display_table = FALSE)
}

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

test_that("rehashing reconstructs hash deterministically", {
  expect_false(identical(agent_a, agent_b))
  expect_identical(
    rehash_agent(agent_a),
    rehash_agent(agent_b)
  )
  expect_identical(
    rehash_agent(agent_a),
    rehash_agent(rehash_agent(agent_a))
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

test_that("multiagent alignment is not sensitive to environment", {

  # Insensitive to `vars()` quosure environment
  agent1a <- local({
    agent %>% 
      col_vals_gt(c, value = vars(a))
  })
  agent1b <- agent %>% 
    col_vals_gt(c, value = vars(a))
  expect_false(identical(
    agent1a$validation_set$values,
    agent1b$validation_set$values
  ))
  expect_equal(nrow(align(agent1a, agent1b)), 1)
  
  # Insensitive to formula environment
  agent2a <- local({
    agent %>% 
      col_vals_not_null(c, preconditions = ~ . %>% identity())
  })
  agent2b <- agent %>% 
    col_vals_not_null(c, preconditions = ~ . %>% identity())
  expect_false(identical(
    agent2a$validation_set$preconditions,
    agent2b$validation_set$preconditions
  ))
  expect_equal(nrow(align(agent2a, agent2b)), 1)
  
  # Insensitive to function environment
  agent3a <- local({
    agent %>% 
      col_vals_not_null(c, preconditions = . %>% identity())
  })
  agent3b <- agent %>% 
    col_vals_not_null(c, preconditions = . %>% identity())
  expect_false(identical(
    agent3a$validation_set$preconditions,
    agent3b$validation_set$preconditions
  ))
  expect_equal(nrow(align(agent3a, agent3b)), 1)
  
  # But sensitive to function definition
  fn <- function(x) identity(x)
  agent4a <- agent %>% 
    col_vals_not_null(c, preconditions = fn)
  fn <- function(x) force(x)
  agent4b <- agent %>% 
    col_vals_not_null(c, preconditions = fn)
  expect_false(identical(
    agent4a$validation_set$preconditions,
    agent4b$validation_set$preconditions
  ))
  expect_false(identical(
    agent4a$validation_set$sha1,
    agent4b$validation_set$sha1
  ))
  expect_equal(nrow(align(agent4a, agent4b)), 2)

})

test_that("multiagent alignment of special data types are correct", {

  # `vars(c)` not equivalent to `"c"`
  agent1a <- agent %>%
    col_vals_equal(columns = c, value = "c")
  agent1b <- agent %>%
    col_vals_equal(columns = c, value = vars(c))
  expect_false(identical(
    agent1a$validation_set$sha1,
    agent1b$validation_set$sha1
  ))
  expect_equal(nrow(align(agent1a, agent1b)), 2)
  c <- "c"
  agent1c <- agent %>%
    col_vals_equal(columns = c, value = c)
  expect_equal(nrow(align(agent1a, agent1c)), 1)
  
  # Aligns identical special data types correctly
  ## Function object
  agent2a <- agent %>% 
    rows_distinct(preconditions = identity)
  agent2b <- agent %>% 
    rows_distinct(preconditions = identity)
  expect_equal(nrow(align(agent2a, agent2b)), 1)
  ## Ignores bytecode differences (`identical(force, identity)`)
  agent2c <- agent %>% 
    rows_distinct(preconditions = force)
  expect_equal(nrow(align(agent2a, agent2c)), 1)
  identity2 <- function(y) y
  agent2d <- agent %>% 
    rows_distinct(preconditions = identity2)
  expect_equal(nrow(align(agent2a, agent2d)), 2)
  
  ## Magrittr anonymous function
  agent3a <- agent %>% 
    rows_distinct(preconditions = . %>% identity())
  agent3b <- agent %>% 
    rows_distinct(preconditions = . %>% identity())
  expect_equal(nrow(align(agent3a, agent3b)), 1)
  agent3c <- agent %>% 
    rows_distinct(preconditions = . %>% force())
  expect_equal(nrow(align(agent3a, agent3c)), 2)
  
  ## Formula + magrittr anonymous function
  agent4a <- agent %>% 
    rows_distinct(preconditions = ~ . %>% identity())
  agent4b <- agent %>% 
    rows_distinct(preconditions = ~ . %>% identity())
  expect_equal(nrow(align(agent4a, agent4b)), 1)
  agent4c <- agent %>% 
    rows_distinct(preconditions = ~ . %>% force())
  expect_equal(nrow(align(agent4a, agent4c)), 2)
  
  ## `vars()` quosures
  agent5a <- agent %>% 
    col_vals_between(columns = c, left = vars(a), right = vars(d))
  agent5b <- agent %>% 
    col_vals_between(columns = c, left = vars(a), right = vars(d))
  expect_equal(nrow(align(agent5a, agent5b)), 1)
  agent5c <- agent %>% 
    col_vals_between(columns = c, left = vars(d), right = vars(a))
  expect_equal(nrow(align(agent5a, agent5c)), 2)
  
  ## segments (same expr)
  agent6a <- agent %>% 
    col_vals_lt(columns = c(a, c), value = vars(d), segments = vars(e, f))
  agent6b <- agent %>% 
    col_vals_lt(columns = c(a, c), value = vars(d), segments = vars(e, f))
  expect_equal(nrow(align(agent6a, agent6b)), 10)
  ## segments (diff expr)
  agent7a <- agent %>% 
    col_vals_lt(columns = c(a, c), value = vars(d), segments = vars(e))
  agent7b <- agent %>% 
    col_vals_lt(columns = c(a, c), value = vars(d), segments = e ~ c(TRUE, FALSE))
  expect_identical(
    agent7a$validation_set$sha1, agent7b$validation_set$sha1
  )
  expect_equal(nrow(align(agent7a, agent7b)), 4)
  
})
