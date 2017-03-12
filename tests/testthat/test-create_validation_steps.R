context("Creation of validation steps")

test_that("Creating a `col_is_character()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  # Make a simple local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,
      1,   6,   "h2adb",  "2017-02-01",
      2,   7,   "h2spb",  "2017-02-02",
      3,   8,   "h2df",   "2017-02-03",
      4,   9,   "d3jwb",  "2017-02-04",
      5,  10,   "h2esf",  "2017-02-05"
    ) %>%
    mutate(d = as.Date(d))
  
  # Use `col_is_character()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_character(column = "c")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_character")
  expect_equivalent(validation$validation_set$column, "c")
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
  expect_true(is.na(validation$validation_set$passed))
})

test_that("Creating a `col_is_date()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  # Make a simple local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,
      1,   6,   "h2adb",  "2017-02-01",
      2,   7,   "h2spb",  "2017-02-02",
      3,   8,   "h2df",   "2017-02-03",
      4,   9,   "d3jwb",  "2017-02-04",
      5,  10,   "h2esf",  "2017-02-05"
    ) %>%
    mutate(d = as.Date(d))
  
  # Use `col_is_date()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_date(column = "d")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_date")
  expect_equivalent(validation$validation_set$column, "d")
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
  expect_true(is.na(validation$validation_set$passed))
})

test_that("Creating a `col_is_factor()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  # Make a simple local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,
      1,   6,   "h2adb",  "2017-02-01",
      2,   7,   "h2spb",  "2017-02-02",
      3,   8,   "h2df",   "2017-02-03",
      4,   9,   "d3jwb",  "2017-02-04",
      5,  10,   "h2esf",  "2017-02-05"
    ) %>%
    mutate(a = as.factor(a))
  
  # Use `col_is_factor()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_factor(column = "a")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_factor")
  expect_equivalent(validation$validation_set$column, "a")
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
  expect_true(is.na(validation$validation_set$passed))
})

test_that("Creating a `col_is_integer()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  # Make a simple local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,
      1,   6,   "h2adb",  "2017-02-01",
      2,   7,   "h2spb",  "2017-02-02",
      3,   8,   "h2df",   "2017-02-03",
      4,   9,   "d3jwb",  "2017-02-04",
      5,  10,   "h2esf",  "2017-02-05"
    ) %>%
    mutate(a = as.integer(a))
  
  # Use `col_is_integer()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_integer(column = "a")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_integer")
  expect_equivalent(validation$validation_set$column, "a")
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
  expect_true(is.na(validation$validation_set$passed))
})

test_that("Creating a `col_is_logical()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  # Make a simple local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,           ~e,
      1,   6,   "h2adb",  "2017-02-01", TRUE,
      2,   7,   "h2spb",  "2017-02-02", TRUE,
      3,   8,   "h2df",   "2017-02-03", FALSE,
      4,   9,   "d3jwb",  "2017-02-04", TRUE,
      5,  10,   "h2esf",  "2017-02-05", FALSE
    )
  
  # Use `col_is_logical()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_logical(column = "e")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d", "e"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_logical")
  expect_equivalent(validation$validation_set$column, "e")
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
  expect_true(is.na(validation$validation_set$passed))
})

test_that("Creating a `col_is_numeric()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  # Make a simple local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,           ~e,
      1,   6,   "h2adb",  "2017-02-01", TRUE,
      2,   7,   "h2spb",  "2017-02-02", TRUE,
      3,   8,   "h2df",   "2017-02-03", FALSE,
      4,   9,   "d3jwb",  "2017-02-04", TRUE,
      5,  10,   "h2esf",  "2017-02-05", FALSE
    )
  
  # Use `col_is_numeric()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_numeric(column = "a")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d", "e"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_numeric")
  expect_equivalent(validation$validation_set$column, "a")
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
  expect_true(is.na(validation$validation_set$passed))
})

test_that("Creating a `col_is_posix()` step is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Create a local table
  tbl_1 <-
    tribble(
      ~a,  ~b,  ~c,       ~d,           ~e,    ~f,          
      1,   6,   "h2adb",  "2017-02-01", TRUE,  "2017-02-01 1:30",
      2,   7,   "h2spb",  "2017-02-02", TRUE,  "2017-02-02 14:22",
      3,   8,   "h2df",   "2017-02-03", FALSE, "2017-02-03 2:56",
      4,   9,   "d3jwb",  "2017-02-04", TRUE,  "2017-02-04 21:38",
      5,  10,   "h2esf",  "2017-02-05", FALSE, "2017-02-05 5:54"
    ) %>%
    mutate(f = as.POSIXct(f, tz = "GMT"))
  
  # Use `col_is_posix()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on("tbl_1") %>%
    col_is_posix(column = "f")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$focal_tbl_name, "tbl_1")
  expect_equivalent(validation$focal_db_type, "local")
  expect_equivalent(validation$focal_col_names, c("a", "b", "c", "d", "e", "f"))
  expect_true(is.na(validation$focal_db_cred_file_path))
  expect_true(is.na(validation$focal_init_sql))
  expect_equivalent(validation$validation_set$tbl_name, "tbl_1")
  expect_equivalent(validation$validation_set$db_type, "local")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_posix")
  expect_equivalent(validation$validation_set$column, "f")
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
  expect_true(is.na(validation$validation_set$passed))
})