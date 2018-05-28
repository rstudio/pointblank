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
    col_is_character(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_character")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_character(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
    col_is_date(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_date")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_date(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
    col_is_factor(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_factor")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_factor(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
    col_is_integer(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_integer")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_integer(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
    col_is_logical(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_logical")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_logical(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
    col_is_numeric(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_numeric")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_numeric(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
    col_is_posix(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_posix")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_posix(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_between()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_between()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_between(
      column = b,
      left = 2,
      right = 10)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[1] %>% as.numeric(), 2)
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[2] %>% as.numeric(), 10)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_between(
      column = all_cols(),
      left = 2,
      right = 10)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_between()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_not_between()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_between(
      column = b,
      left = 2,
      right = 10)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_between")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[1] %>% as.numeric(), 2)
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[2] %>% as.numeric(), 10)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_between(
      column = all_cols(),
      left = 2,
      right = 10)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_equal()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_equal()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_equal(
      column = b,
      value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equivalent(validation$validation_set$column, "b")
  expect_equivalent(validation$validation_set$value, 5)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_equal(
      column = all_cols(),
      value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_equal()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_not_equal()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_equal(
      column = b,
      value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equivalent(validation$validation_set$column, "b")
  expect_equivalent(validation$validation_set$value, 5)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_equal(
      column = all_cols(),
      value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_gt()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_gt()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_gt(
      column = b,
      value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_gt")
  expect_equivalent(validation$validation_set$column, "b")
  expect_equivalent(validation$validation_set$value, 5)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_gt(
      column = all_cols(),
      value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_gte()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_gte()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_gte(
      column = b,
      value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_gte")
  expect_equivalent(validation$validation_set$column, "b")
  expect_equivalent(validation$validation_set$value, 5)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_gte(
      column = all_cols(),
      value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_lt()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_lt()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_lt(
      column = b,
      value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_lt")
  expect_equivalent(validation$validation_set$column, "b")
  expect_equivalent(validation$validation_set$value, 5)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_lt(
      column = all_cols(),
      value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_lte()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_lte()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_lte(
      column = b,
      value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_lte")
  expect_equivalent(validation$validation_set$column, "b")
  expect_equivalent(validation$validation_set$value, 5)
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_lte(
      column = all_cols(),
      value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_in_set()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_in_set()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_in_set(
      column = b,
      set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_in_set")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[1], "1-bcd-345")
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[2], "5-jdo-903")
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_in_set(
      column = all_cols(),
      set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_in_set()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_not_in_set()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_in_set(
      column = b,
      set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[1], "1-bcd-345")
  expect_equivalent(validation$set[[1, 1]] %>% strsplit(",") %>% unlist %>% .[2], "5-jdo-903")
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_in_set(
      column = all_cols(),
      set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_regex()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_regex()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_regex(
      column = b,
      regex = "[0-9]-.*")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_regex")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set$regex, "[0-9]-.*")
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_regex(
      column = all_cols(),
      regex = "[0-9]-.*")
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_null()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_null()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_null(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_null")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_null(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_null()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_vals_not_null()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_null(column = b)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
  
  # Validate all available columns using the
  # `all_cols()` helper function
  validation_all <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_vals_not_null(column = all_cols())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `rows_not_duplicated()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `rows_not_duplicated()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    rows_not_duplicated()
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$focal_db_type, "local_file")
  expect_equivalent(validation$focal_col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$db_type, "local_file")
  expect_equivalent(validation$validation_set$assertion_type, "rows_not_duplicated")
  expect_true(is.na(validation$validation_set$column))
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  expect_true(is.na(validation$validation_set$init_sql))
  expect_true(is.na(validation$validation_set$db_cred_file_path))
  expect_equivalent(validation$validation_set$file_path,
                    system.file("extdata", "small_table.csv",
                                package = "pointblank"))
})

