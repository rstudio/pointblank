context("Performing interrogations")

test_that("Interrogating for column types", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_character()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_character(column = "b") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_character")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(
    validation$validation_set$file_path, 
      system.file("extdata", "small_table.csv", package = "pointblank"))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_is_numeric()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_numeric(column = "a") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_numeric")
  expect_equivalent(validation$validation_set$column, "a")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 0)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0)
  expect_equivalent(validation$validation_set$f_failed, 1)
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(
    validation$validation_set$file_path, 
    system.file("extdata", "small_table.csv", package = "pointblank"))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use `col_is_posix()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_posix(column = "date_time") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_posix")
  expect_equivalent(validation$validation_set$column, "date_time")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(
    validation$validation_set$file_path, 
    system.file("extdata", "small_table.csv", package = "pointblank"))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
})
