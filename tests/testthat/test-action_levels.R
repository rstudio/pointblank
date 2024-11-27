test_that("The `action_levels()` helper function works as expected", {
  
  # Expect that if `action_levels()` is used as is,
  # all of the elements will be NULL
  al <- action_levels()
  
  expect_s3_class(al, "action_levels")
  expect_named(
    al,
    c(
      "warn_fraction", "warn_count", "stop_fraction", "stop_count", 
      "notify_fraction", "notify_count", "fns"
      )
  )
  expect_length(al, 7)
  expect_null(al[[1]])
  expect_null(al[[2]])
  expect_null(al[[3]])
  expect_null(al[[4]])
  expect_null(al[[5]])
  expect_null(al[[6]])
  expect_named(al[[7]], c("warn", "stop", "notify"))
  expect_type(al[[7]], "list")
  expect_null(al[[7]][[1]])
  expect_null(al[[7]][[2]])
  expect_null(al[[7]][[3]])
  expect_length(al[[7]], 3)
  
  # Create an `action_levels()` list with fractional values
  al <- action_levels(warn_at = 0.2, stop_at = 0.8, notify_at = 0.345)
  
  expect_s3_class(al, "action_levels")
  expect_length(al, 7)
  expect_named(
    al,
    c(
      "warn_fraction", "warn_count", "stop_fraction", "stop_count", 
      "notify_fraction", "notify_count", "fns"
    )
  )
  
  expect_equal(al$warn_fraction, 0.2)
  expect_null(al$warn_count)
  expect_equal(al$stop_fraction, 0.8)
  expect_null(al$stop_count)
  expect_equal(al$notify_fraction, 0.345)
  expect_null(al$notify_count)
  
  expect_length(al[[7]], 3)
  expect_named(al[[7]], c("warn", "stop", "notify"))
  expect_type(al[[7]], "list")
  expect_null(al[[7]][[1]])
  expect_null(al[[7]][[2]])
  expect_null(al[[7]][[3]])
  
  # Create an `action_levels()` list with count values
  al <- action_levels(warn_at = 20, stop_at = 80, notify_at = 34.6)
  
  al %>% expect_s3_class("action_levels")
  al %>%
    expect_named(
      c(
        "warn_fraction", "warn_count", "stop_fraction", "stop_count", 
        "notify_fraction", "notify_count", "fns")
    )
  al[[7]] %>% expect_named(c("warn", "stop", "notify"))
  
  al$warn_fraction %>% expect_null()
  al$warn_count %>% expect_equal(20)
  al$stop_fraction %>% expect_null()
  al$stop_count %>% expect_equal(80)
  al$notify_fraction %>% expect_null()
  al$notify_count %>% expect_equal(34)
  al[[7]] %>% expect_type("list")
  al[[7]][[1]] %>% expect_null()
  al[[7]][[2]] %>% expect_null()
  al[[7]][[3]] %>% expect_null()
  al %>% length() %>% expect_equal(7)
  al[[7]] %>% length() %>% expect_equal(3)
  
  # Expect an error if non-numeric values provided
  expect_error(action_levels(warn_at = "20"))
  
  # Expect an error if any value less than or
  # equal to zero is provided
  expect_error(action_levels(warn_at = 0))
  expect_error(action_levels(warn_at = -1.5))
  
  # Add functions to the `fns` arg
  al <- 
    action_levels(
      warn_at = 3,
      fns = list(warn = ~ my_great_function(vl = .vars_list))
    )
  
  al %>% expect_s3_class("action_levels")
  al %>% 
    names() %>% 
    expect_equal(
      c(
        "warn_fraction", "warn_count", "stop_fraction", "stop_count", 
        "notify_fraction", "notify_count", "fns")
    )
  al[[7]] %>% expect_named("warn")
  al[[7]][[1]] %>% expect_s3_class("formula")
  al[[7]][[1]] %>%
    as.character() %>%
    expect_equal(c("~", "my_great_function(vl = .vars_list)"))
  
  al$warn_fraction %>% expect_null()
  al$warn_count %>% expect_equal(3)
  al$stop_fraction %>% expect_null()
  al$stop_count %>% expect_null()
  al$notify_fraction %>% expect_null()
  al$notify_count %>% expect_null()
  al[[7]] %>% expect_type("list")
  al %>% expect_length(7)
  al[[7]] %>% expect_length(1)
  
  # Expect an error if not all components
  # of the `fns` list are formulas
  expect_error(action_levels(warn_at = 3, fns = list(warn = "text")))
  
  # Expect an error if not all components
  # of the `fns` list are named
  expect_error(
    action_levels(
      warn_at = 3,
      fns = list(
        warn = ~ my_great_function(vl = .vars_list),
        ~ another_function()
        )
      )
    )
  
  # Expect an error if any of the named components
  # of the `fns` list aren't one of `warn`, `stop`,
  # or `notify`
  expect_error(
    action_levels(
      warn_at = 3,
      fns = list(
        warn = ~ my_great_function(vl = .vars_list),
        notable =  ~ another_function()
      )
    )
  )
})

test_that("The appropriate actions occur when using `action_levels()`", {
  
  agent <-
    create_agent(tbl = small_table, label = "small_table_tests") %>%
    col_vals_gt(
      vars(d), 1000,
      actions = action_levels(warn_at = 3, fns = list(warn = ~"warning")
      )
    ) %>%
    col_vals_in_set(
      vars(f), c("low", "high"),
      actions = action_levels(warn_at = 0.1, fns = list(warn = ~"warning")
      )
    ) %>%
    interrogate()
  
  agent_report <- get_agent_report(agent, display_table = FALSE)
  agent_report$W %>% expect_equal(rep(TRUE, 2))
  
  agent <-
    create_agent(tbl = small_table, label = "small_table_tests") %>%
    col_vals_gt(
      vars(d), 1000,
      actions = action_levels(notify_at = 3, fns = list(notify = ~"notify")
      )
    ) %>%
    col_vals_in_set(
      vars(f), c("low", "high"),
      actions = action_levels(notify_at = 0.1, fns = list(notify = ~"notify")
      )
    ) %>%
    interrogate()
  
  agent_report <- get_agent_report(agent, display_table = FALSE)
  agent_report$N %>% expect_equal(rep(TRUE, 2))
  
  agent <-
    create_agent(tbl = small_table, label = "small_table_tests") %>%
    col_vals_gt(
      vars(d), 1000,
      actions = action_levels(stop_at = 3, fns = list(stop = ~"stop")
      )
    ) %>%
    col_vals_in_set(
      vars(f), c("low", "high"),
      actions = action_levels(stop_at = 0.1, fns = list(stop = ~"stop")
      )
    ) %>%
    interrogate()
  
  agent_report <- get_agent_report(agent, display_table = FALSE)
  agent_report$S %>% expect_equal(rep(TRUE, 2))
})
