agent <- create_agent(small_table)
z <- rlang::missing_arg()
nonexistent_col <- "z"

test_that("tidyselect errors signaled at report, not during development of validation plan", {
  
  # No immediate error for all patterns
  expect_s3_class(a1 <- agent %>% col_vals_not_null(z), "ptblank_agent")
  expect_s3_class(a2 <- agent %>% col_vals_not_null("z"), "ptblank_agent")
  expect_s3_class(a3 <- agent %>% col_vals_not_null(all_of("z")), "ptblank_agent")
  expect_s3_class(a4 <- agent %>% col_vals_not_null(all_of(nonexistent_col)), "ptblank_agent")
  
  # Failure signaled via report
  expect_false(a1 %>% interrogate() %>% all_passed())
  expect_false(a2 %>% interrogate() %>% all_passed())
  expect_false(a3 %>% interrogate() %>% all_passed())
  expect_false(a4 %>% interrogate() %>% all_passed())
  
})

test_that("fail state correctly registered in the report for tidyselect errors", {
  
  # Adopted from test-get_agent_report.R -------------------------
  
  # The following agent will perform an interrogation that results
  # in all test units passing in the second validation step, but
  # the first experiences an evaluation error (since column
  # `z` doesn't exist in `small_table`)
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(all_of("z")) %>% # swapped for `vars("z")`
    col_vals_gt(vars(c), 1, na_pass = TRUE) %>%
    interrogate()
  
  # Expect that the interrogation *does not* have
  # a completely 'all passed' state (returning FALSE)
  agent %>% all_passed() %>% expect_false()
  
  # If narrowing the `all_passed()` evaluation to only
  # the second validation step, then we should expect TRUE
  agent %>% all_passed(i = 2) %>% expect_true()
  
  # If narrowing the `all_passed()` evaluation to only
  # the first validation step, then we should expect FALSE
  agent %>% all_passed(i = 1) %>% expect_false()
  
})

test_that("(tidy-)selecting 0 columns = fail at interrogation", {
  
  eval_errors <- function(x) interrogate(x)$validation_set$eval_error
  
  # Old behavior for vars()/NULL/<missing> preserved:
  ## 1) No immediate error for zero columns selected
  expect_s3_class(a5 <- agent %>% col_vals_not_null(vars()), "ptblank_agent")
  expect_s3_class(a6 <- agent %>% col_vals_not_null(NULL), "ptblank_agent")
  expect_s3_class(a7 <- agent %>% col_vals_not_null(), "ptblank_agent")
  ## 2) # Treated as inactive in the report
  expect_true(a5 %>% eval_errors())
  expect_true(a6 %>% eval_errors())
  expect_true(a7 %>% eval_errors())
  
  # Same behavior of 0-column selection replicated in tidyselect patterns
  expect_length(small_table %>% dplyr::select(any_of("z")), 0)
  expect_length(small_table %>% dplyr::select(c()), 0)
  expect_s3_class(a8 <- agent %>% col_vals_not_null(any_of("z")), "ptblank_agent")
  expect_s3_class(a9 <- agent %>% col_vals_not_null(c()), "ptblank_agent")
  expect_true(a8 %>% eval_errors())
  expect_true(a9 %>% eval_errors())
  
})

test_that("tidyselecting 0 columns for rows_* functions = fail at interrogation", {
  
  expect_no_error(a_rows_distinct <- agent %>% rows_distinct(starts_with("z")) %>% interrogate())
  expect_no_error(a_rows_complete <- agent %>% rows_complete(starts_with("z")) %>% interrogate())
  expect_true(a_rows_distinct$validation_set$eval_error)
  expect_true(a_rows_complete$validation_set$eval_error)
  
  expect_no_error(a_rows_distinct2 <- agent %>% rows_distinct(any_of("z")) %>% interrogate())
  expect_no_error(a_rows_complete2 <- agent %>% rows_complete(any_of("z")) %>% interrogate())
  expect_true(a_rows_distinct2$validation_set$eval_error)
  expect_identical(unlist(a_rows_distinct2$validation_set$column), NA_character_)
  expect_true(a_rows_complete2$validation_set$eval_error)
  expect_identical(unlist(a_rows_complete2$validation_set$column), NA_character_)
  
})

test_that("tidyselect errors *are* immediate for assertion/expectation/test", {
  
  # For validation steps used on table
  expect_error(small_table %>% col_vals_not_null(z))
  expect_error(small_table %>% col_vals_not_null("z"))
  expect_error(small_table %>% col_vals_not_null(all_of("z")))
  expect_error(small_table %>% col_vals_not_null(all_of(nonexistent_col)))
  
  # For expectations
  expect_error(small_table %>% expect_col_vals_not_null(z))
  expect_error(small_table %>% expect_col_vals_not_null("z"))
  expect_error(small_table %>% expect_col_vals_not_null(all_of("z")))
  expect_error(small_table %>% expect_col_vals_not_null(all_of(nonexistent_col)))
  
  # For tests
  expect_error(small_table %>% test_col_vals_not_null(z))
  expect_error(small_table %>% test_col_vals_not_null("z"))
  expect_error(small_table %>% test_col_vals_not_null(all_of("z")))
  expect_error(small_table %>% test_col_vals_not_null(all_of(nonexistent_col)))
  
})

test_that("tidyselect errors cannot be downgraded in assertion/expectation on table", {
  
  # This replicates old behavior
  expect_error({
    small_table %>% 
      col_vals_not_null(a) %>% 
      col_vals_not_null(z, actions = warn_on_fail()) %>% 
      col_vals_not_null(b)
  })
  
})

test_that("env scoping with bare symbol patterns", {
  
  # `z` is external vector of valid column
  z <- "a"
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(small_table %>% col_vals_not_null(z))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  # `z` is not character
  z <- mtcars
  # c() and vars() both error, but different reasons
  ## c() scopes z in env and determines its invalid
  expect_error({small_table %>% col_vals_not_null(c(z))}, "`z` must be numeric or character")
  ## vars() doesn't attempt to scope z in env at all
  expect_error({small_table %>% col_vals_not_null(vars(z))}, "Column `z` doesn't exist")
  
  # Cleanup
  z <- rlang::missing_arg()
  
})
