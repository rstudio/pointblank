context("Creation of validation steps")

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

test_that("Creating a `col_is_character()` step is possible", {
  
  # Use `col_is_character()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_date()` step is possible", {
  
  # Use `col_is_date()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_date(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name,  "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_date(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_factor()` step is possible", {
  
  # Use `col_is_factor()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_factor(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_factor(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_integer()` step is possible", {
  
  # Use `col_is_integer()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_integer(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_integer(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_logical()` step is possible", {
  
  # Use `col_is_logical()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_logical(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_logical(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_numeric()` step is possible", {
  
  # Use `col_is_numeric()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_posix()` step is possible", {
  
  # Use `col_is_posix()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_posix(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_posix(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_between()` step is possible", {
  
  # Use `col_vals_between()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(columns = vars(b), left = 2, right = 10)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set[["set"]] %>% unlist(), c(2, 10))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_between(columns = everything(), left = 2, right = 10)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_between()` step is possible", {
  
  # Use `col_vals_not_between()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(columns = vars(b), left = 2, right = 10)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_between")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set[["set"]] %>% unlist(), c(2, 10))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(columns = everything(), left = 2, right = 10)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_equal()` step is possible", {
  
  # Use `col_vals_equal()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_equal()` step is possible", {
  
  # Use `col_vals_not_equal()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_gt()` step is possible", {
  
  # Use `col_vals_gt()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` select function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_gte()` step is possible", {
  
  # Use `col_vals_gte()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gte(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_gte(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_lt()` step is possible", {
  
  # Use `col_vals_lt()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_lt(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_lt(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_lte()` step is possible", {
  
  # Use `col_vals_lte()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_lte(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_lte(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_in_set()` step is possible", {
  
  # Use `col_vals_in_set()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = vars(b), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_in_set")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set[["set"]] %>% unlist(), c("1-bcd-345", "5-jdo-903"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = everything(), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_in_set()` step is possible", {
  
  # Use `col_vals_not_in_set()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = vars(b), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set[["set"]] %>% unlist(), c("1-bcd-345", "5-jdo-903"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = everything(), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_regex()` step is possible", {
  
  # Use `col_vals_regex()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(columns = vars(b), regex = "[0-9]-.*")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(columns = everything(), regex = "[0-9]-.*")
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_null()` step is possible", {
  
  # Use `col_vals_null()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_null(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_null()` step is possible", {
  
  # Use `col_vals_not_null()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
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
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equivalent(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equivalent(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `rows_not_duplicated()` step is possible", {
  
  # Use `rows_not_duplicated()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated()
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_is(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equivalent(validation$validation_set$assertion_type, "rows_not_duplicated")
  expect_true(is.na(validation$validation_set$column %>% .[[1]] %>% .[[1]]))
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
})
