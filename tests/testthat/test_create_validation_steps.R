context("Creation of validation steps")

test_that("Creating a `col_is_character()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_character()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_character(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_character")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

test_that("Creating a `col_is_date()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_date()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_date(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_date")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

test_that("Creating a `col_is_factor()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_factor()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_factor(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_factor")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

test_that("Creating a `col_is_integer()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_integer()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_integer(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_integer")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

test_that("Creating a `col_is_logical()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_logical()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_logical(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_logical")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

test_that("Creating a `col_is_numeric()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_numeric()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_numeric(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_numeric")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

test_that("Creating a `col_is_posix()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_posix()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_posix(column = "b")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_posix")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_null(validation$validation_set$set[[1]])
  expect_true(is.na(validation$validation_set$regex))
  expect_null(validation$validation_set$preconditions[[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_equivalent(validation$validation_set$report_count, 0)
  expect_equivalent(validation$validation_set$warn_count, 1)
  expect_equivalent(validation$validation_set$notify_count, 1)
  expect_true(is.na(validation$validation_set$report))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

