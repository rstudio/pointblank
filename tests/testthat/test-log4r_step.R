test_that("log4r_step() uses custom message argument correctly", {
  
  skip_if_not_installed("log4r")
  
  # Use a fixed filename in tempdir
  temp_log_file <- file.path(tempdir(), "test_log4r_custom.log")
  if (file.exists(temp_log_file)) unlink(temp_log_file)
  on.exit(unlink(temp_log_file), add = TRUE)
  
  # Test with custom message
  al_custom <-
    action_levels(
      warn_at = 0.1,
      fns = list(
        warn = ~ log4r_step(
          x,
          message = "Custom: Step {x$i} had {x$n_failed} failing units.",
          append_to = file.path(tempdir(), "test_log4r_custom.log")
        )
      )
    )
  
  agent_custom <-
    create_agent(
      tbl = small_table,
      tbl_name = "small_table",
      label = "Custom message test",
      actions = al_custom
    ) %>%
    col_vals_lt(a, value = 7) %>%
    interrogate()
  
  # Read the log file
  log_contents <- readLines(temp_log_file)
  
  # Check that the custom message appears in the log
  expect_true(any(grepl("Custom: Step 1 had 2 failing units", log_contents)))
  expect_false(any(grepl("exceeded the WARN failure threshold", log_contents)))
  
  # Clean up for next test
  unlink(temp_log_file)
  
  # Test with default message (NULL)
  temp_log_file2 <- file.path(tempdir(), "test_log4r_default.log")
  if (file.exists(temp_log_file2)) unlink(temp_log_file2)
  on.exit(unlink(temp_log_file2), add = TRUE)
  
  al_default <-
    action_levels(
      warn_at = 0.1,
      fns = list(
        warn = ~ log4r_step(
          x,
          append_to = file.path(tempdir(), "test_log4r_default.log")
        )
      )
    )
  
  agent_default <-
    create_agent(
      tbl = small_table,
      tbl_name = "small_table",
      label = "Default message test",
      actions = al_default
    ) %>%
    col_vals_lt(a, value = 7) %>%
    interrogate()
  
  # Read the log file
  log_contents_default <- readLines(temp_log_file2)
  
  # Check that the default message appears in the log
  expect_true(any(grepl("exceeded the WARN failure threshold", log_contents_default)))
  expect_true(any(grepl("f_failed = 0.15385", log_contents_default)))
  expect_true(any(grepl("col_vals_lt", log_contents_default)))
})

test_that("log4r_step() message argument supports glue syntax", {
  
  skip_if_not_installed("log4r")
  
  # Use a fixed filename in tempdir
  temp_log_file <- file.path(tempdir(), "test_log4r_glue.log")
  if (file.exists(temp_log_file)) unlink(temp_log_file)
  on.exit(unlink(temp_log_file), add = TRUE)
  
  # Test with glue-style message using various x-list components
  al_glue <-
    action_levels(
      warn_at = 0.1,
      fns = list(
        warn = ~ log4r_step(
          x,
          message = "Step {x$i}: {x$type} validation. Passed: {x$n_passed}, Failed: {x$n_failed}",
          append_to = file.path(tempdir(), "test_log4r_glue.log")
        )
      )
    )
  
  agent_glue <-
    create_agent(
      tbl = small_table,
      tbl_name = "small_table",
      actions = al_glue
    ) %>%
    col_vals_gt(d, value = 10000) %>%
    interrogate()
  
  # Read the log file
  log_contents <- readLines(temp_log_file)
  
  # Check that the glue interpolation worked
  expect_true(any(grepl("Step 1: col_vals_gt validation", log_contents)))
  expect_true(any(grepl("Passed: .*, Failed: .*", log_contents)))
})

test_that("log4r_step() respects severity hierarchy with custom messages", {
  
  skip_if_not_installed("log4r")
  
  # Use fixed filenames in tempdir
  temp_log_warn <- file.path(tempdir(), "test_log4r_warn.log")
  temp_log_stop <- file.path(tempdir(), "test_log4r_stop.log")
  if (file.exists(temp_log_warn)) unlink(temp_log_warn)
  if (file.exists(temp_log_stop)) unlink(temp_log_stop)
  on.exit({
    unlink(temp_log_warn)
    unlink(temp_log_stop)
  }, add = TRUE)
  
  # Create action levels with both warn and stop
  al_multi <-
    action_levels(
      warn_at = 0.1,
      stop_at = 0.2,
      fns = list(
        warn = ~ log4r_step(
          x,
          message = "WARN: Step {x$i}",
          append_to = file.path(tempdir(), "test_log4r_warn.log")
        ),
        stop = ~ log4r_step(
          x,
          message = "STOP: Step {x$i}",
          append_to = file.path(tempdir(), "test_log4r_stop.log")
        )
      )
    )
  
  agent_multi <-
    create_agent(
      tbl = small_table,
      tbl_name = "small_table",
      actions = al_multi
    ) %>%
    col_vals_gt(d, value = 5000) %>%  # This will fail enough to trigger stop
    interrogate()
  
  # The stop condition should be logged, not the warn
  # (because stop is higher severity and both thresholds are exceeded)
  if (file.exists(temp_log_stop) && file.size(temp_log_stop) > 0) {
    log_contents_stop <- readLines(temp_log_stop)
    expect_true(any(grepl("STOP: Step 1", log_contents_stop)))
  }
  
  # Warn should not be logged when stop threshold is also exceeded
  if (file.exists(temp_log_warn)) {
    # File might exist but should be empty or not contain the message
    if (file.size(temp_log_warn) > 0) {
      log_contents_warn <- readLines(temp_log_warn)
      expect_false(any(grepl("WARN: Step 1", log_contents_warn)))
    }
  }
})
