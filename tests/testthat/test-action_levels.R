test_that("The `action_levels()` helper function works as expected", {

  # Expect that if `action_levels()` is used as is,
  # all of the elements will be NULL
  al <- action_levels()

  expect_s3_class(al, "action_levels")
  expect_named(
    al,
    c(
      "warn_fraction", "warn_count", "error_fraction", "error_count",
      "critical_fraction", "critical_count", "fns"
      )
  )
  expect_length(al, 7)
  expect_null(al[[1]])
  expect_null(al[[2]])
  expect_null(al[[3]])
  expect_null(al[[4]])
  expect_null(al[[5]])
  expect_null(al[[6]])
  expect_true(all(c("warn", "error", "critical") %in% names(al[[7]])))
  expect_type(al[[7]], "list")
  expect_null(al[[7]][[1]])
  expect_null(al[[7]][[2]])
  expect_null(al[[7]][[3]])
  expect_length(al[[7]], 5)

  # Create an `action_levels()` list with fractional values
  al <- action_levels(warn = 0.2, error = 0.8, critical = 0.345)

  expect_s3_class(al, "action_levels")
  expect_length(al, 7)
  expect_named(
    al,
    c(
      "warn_fraction", "warn_count", "error_fraction", "error_count",
      "critical_fraction", "critical_count", "fns"
    )
  )

  expect_equal(al$warn_fraction, 0.2)
  expect_null(al$warn_count)
  expect_equal(al$error_fraction, 0.8)
  expect_null(al$error_count)
  expect_equal(al$critical_fraction, 0.345)
  expect_null(al$critical_count)

  expect_length(al[[7]], 5)
  expect_true(all(c("warn", "error", "critical") %in% names(al[[7]])))
  expect_type(al[[7]], "list")
  expect_null(al[[7]][[1]])
  expect_null(al[[7]][[2]])
  expect_null(al[[7]][[3]])

  # Create an `action_levels()` list with count values
  al <- action_levels(warn = 20, error = 80, critical = 34.6)

  al %>% expect_s3_class("action_levels")
  al %>%
    expect_named(
      c(
        "warn_fraction", "warn_count", "error_fraction", "error_count",
        "critical_fraction", "critical_count", "fns")
    )
  expect_true(all(c("warn", "error", "critical") %in% names(al[[7]])))

  al$warn_fraction %>% expect_null()
  al$warn_count %>% expect_equal(20)
  al$error_fraction %>% expect_null()
  al$error_count %>% expect_equal(80)
  al$critical_fraction %>% expect_null()
  al$critical_count %>% expect_equal(34)
  al[[7]] %>% expect_type("list")
  al[[7]][[1]] %>% expect_null()
  al[[7]][[2]] %>% expect_null()
  al[[7]][[3]] %>% expect_null()
  al %>% length() %>% expect_equal(7)
  expect_length(al[[7]], 5)

  # Expect an error if non-numeric values provided
  expect_error(action_levels(warn = "20"))

  # Expect an error if any value less than or
  # equal to zero is provided
  expect_error(action_levels(warn = 0))
  expect_error(action_levels(warn = -1.5))

  # Add functions to the `fns` arg
  al <-
    action_levels(
      warn = 3,
      fns = action_fns(warn = ~ my_great_function(vl = .vars_list))
    )

  al %>% expect_s3_class("action_levels")
  al %>%
    names() %>%
    expect_equal(
      c(
        "warn_fraction", "warn_count", "error_fraction", "error_count",
        "critical_fraction", "critical_count", "fns")
    )
  expect_true(all(c("warn", "error", "critical") %in% names(al[[7]])))
  al[[7]][[1]] %>% expect_s3_class("formula")
  al[[7]][[1]] %>%
    as.character() %>%
    expect_equal(c("~", "my_great_function(vl = .vars_list)"))

  al$warn_fraction %>% expect_null()
  al$warn_count %>% expect_equal(3)
  al$error_fraction %>% expect_null()
  al$error_count %>% expect_null()
  al$critical_fraction %>% expect_null()
  al$critical_count %>% expect_null()
  al[[7]] %>% expect_type("list")
  al %>% expect_length(7)

  # Expect an error if not all components
  # of the `fns` list are formulas
  expect_error(action_levels(warn = 3, fns = action_fns(warn = "text")))

  # Expect an error if not all components
  # of the `fns` list are named
  expect_error(
    action_levels(
      warn = 3,
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
      warn = 3,
      fns = action_fns(
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
      actions = action_levels(warn = 3, fns = action_fns(warn = ~"warning")
      )
    ) %>%
    col_vals_in_set(
      vars(f), c("low", "high"),
      actions = action_levels(warn = 0.1, fns = action_fns(warn = ~"warning")
      )
    ) %>%
    interrogate()

  agent_report <- get_agent_report(agent, display_table = FALSE)
  agent_report$W %>% expect_equal(rep(TRUE, 2))

  agent <-
    create_agent(tbl = small_table, label = "small_table_tests") %>%
    col_vals_gt(
      vars(d), 1000,
      actions = action_levels(critical = 3, fns = action_fns(critical = ~"critical")
      )
    ) %>%
    col_vals_in_set(
      vars(f), c("low", "high"),
      actions = action_levels(critical = 0.1, fns = action_fns(critical = ~"critical")
      )
    ) %>%
    interrogate()

  agent_report <- get_agent_report(agent, display_table = FALSE)
  agent_report$C %>% expect_equal(rep(TRUE, 2))

  agent <-
    create_agent(tbl = small_table, label = "small_table_tests") %>%
    col_vals_gt(
      vars(d), 1000,
      actions = action_levels(error = 3, fns = action_fns(error = ~"error")
      )
    ) %>%
    col_vals_in_set(
      vars(f), c("low", "high"),
      actions = action_levels(error = 0.1, fns = action_fns(error = ~"error")
      )
    ) %>%
    interrogate()

  agent_report <- get_agent_report(agent, display_table = FALSE)
  agent_report$E %>% expect_equal(rep(TRUE, 2))
})

test_that("Deprecated argument names in `action_levels()` emit warnings", {

  # All three old arg names trigger the same deprecation warning (cli frequency-limited)
  # Test that the first call emits the deprecatedWarning class
  expect_warning(
    al_warn <- action_levels(warn_at = 0.1),
    class = "deprecatedWarning"
  )
  expect_equal(al_warn$warn_fraction, 0.1)

  # Subsequent calls in same session may not re-emit (frequency-limited) — test results only
  al_stop <- suppressWarnings(action_levels(stop_at = 0.2))
  expect_equal(al_stop$error_fraction, 0.2)

  al_notify <- suppressWarnings(action_levels(notify_at = 0.3))
  expect_equal(al_notify$critical_fraction, 0.3)

  al_warn_at <- suppressWarnings(warn_on_fail(warn_at = 0.1))
  expect_equal(al_warn_at$warn_fraction, 0.1)

  # stop_on_fail() uses a separate cli_warn (not frequency-limited the same way)
  expect_warning(
    al <- stop_on_fail(stop_at = 0.5),
    regexp = "deprecated"
  )
  expect_equal(al$error_fraction, 0.5)

  # No warning when using new names
  expect_no_warning(action_levels(warn = 0.1, error = 0.2, critical = 0.3))
  expect_no_warning(error_on_fail(error = 0.5))
  expect_no_warning(warn_on_fail(warn = 0.1))
})
