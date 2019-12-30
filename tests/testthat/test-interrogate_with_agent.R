context("Performing interrogations with an agent")

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

test_that("Interrogating with an agent yields the correct results", {
  
  # Use `col_is_character()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_character(column = b) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
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
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_is_numeric()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(column = a) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
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
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use `col_is_posix()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_posix(column = date_time) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
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
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use `col_is_date()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_date(column = date) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_date")
  expect_equivalent(validation$validation_set$column, "date")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use `col_is_integer()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_integer(column = a) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_integer")
  expect_equivalent(validation$validation_set$column, "a")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use `col_is_logical()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_logical(column = e) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_is_logical")
  expect_equivalent(validation$validation_set$column, "e")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
})


test_that("Interrogating for valid row values", {
  
  # Use `col_vals_between()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      column = d,
      left = 0,
      right = 5000) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "d")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 12)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.92308)
  expect_equivalent(validation$validation_set$f_failed, 0.07692)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      column = d,
      left = 0,
      right = 5000,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    )%>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "d")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Use `col_vals_not_between()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(
      column = d,
      left = 500,
      right = 1000) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_between")
  expect_equivalent(validation$validation_set$column, "d")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 9)
  expect_equivalent(validation$validation_set$n_failed, 4)
  expect_equivalent(validation$validation_set$f_passed, 0.69231)
  expect_equivalent(validation$validation_set$f_failed, 0.30769)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_not_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(
      column = d,
      left = 500,
      right = 1000,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_between")
  expect_equivalent(validation$validation_set$column, "d")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 4)
  expect_equivalent(validation$validation_set$f_passed, 0.63636)
  expect_equivalent(validation$validation_set$f_failed, 0.36364)
  
  
  # Use `col_vals_equal()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(
      column = d,
      value = 283.94) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equivalent(validation$validation_set$column, "d")
  expect_equivalent(validation$validation_set$value, 283.94)
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 12)
  expect_equivalent(validation$validation_set$f_passed, 0.07692)
  expect_equivalent(validation$validation_set$f_failed, 0.92308)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(
      column = d,
      value = 283.94,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equivalent(validation$validation_set$column, "d")
  expect_equivalent(validation$validation_set$value, 283.94)
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 10)
  expect_equivalent(validation$validation_set$f_passed, 0.09091)
  expect_equivalent(validation$validation_set$f_failed, 0.90909)
  
  # Use `col_vals_not_equal()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(
      column = d,
      value = 283.94) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equivalent(validation$validation_set$column, "d")
  expect_equivalent(validation$validation_set$value, 283.94)
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 12)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.92308)
  expect_equivalent(validation$validation_set$f_failed, 0.07692)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(
      column = d,
      value = 283.94,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equivalent(validation$validation_set$column, "d")
  expect_equivalent(validation$validation_set$value, 283.94)
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 10)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.90909)
  expect_equivalent(validation$validation_set$f_failed, 0.09091)
  
  # Use `rows_not_duplicated()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated() %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "rows_not_duplicated")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated(
      preconditions = ~ tbl %>% dplyr::filter(date != "2016-01-20")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "rows_not_duplicated")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Use `col_vals_equal()` function to create
  # a validation step for selected columns, then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated(cols = vars(date_time, a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "rows_not_duplicated")
  expect_equivalent(validation$validation_set$column %>% unlist(), "date_time, a")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  
  # Use `col_vals_in_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(
      column = f,
      set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_in_set")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_not_in_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(
      column = f,
      set = c("lower", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Make another pass with the `col_vals_not_in_set()`
  # function to create a validation step, then, perform an
  # interrogation that fails every row
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(
      column = f,
      set = c("lower", "middle", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_not_null()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(column = c) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_not_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(
      column = c,
      preconditions = ~ tbl %>%
        dplyr::filter(date > "2016-01-06" & date < "2016-01-30")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 8)
  expect_equivalent(validation$validation_set$n_passed, 8)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Use `col_vals_null()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(column = c) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_null")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 2)
  expect_equivalent(validation$validation_set$n_failed, 11)
  expect_equivalent(validation$validation_set$f_passed, 0.15385)
  expect_equivalent(validation$validation_set$f_failed, 0.84615)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(
      column = c,
      preconditions = ~ tbl %>%
        dplyr::filter(date == '2016-01-06' | date == '2016-01-30')
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_null")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.na(validation$validation_set$value))
  expect_true(is.na(validation$validation_set$regex))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 2)
  expect_equivalent(validation$validation_set$n_passed, 2)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Use `col_vals_regex()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(
      column = b,
      regex = "[0-9]-[a-z]{3}-[0-9]{3}") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_regex")
  expect_equivalent(validation$validation_set$column, "b")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set$regex, "[0-9]-[a-z]{3}-[0-9]{3}")
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_true(is.na(validation$validation_set$warn_count))
  expect_true(is.na(validation$validation_set$notify_count))
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use `col_vals_regex()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(
      column = f,
      regex = "[a-z]{3}",
      preconditions = ~ tbl %>% dplyr::filter(f != "high")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$focal_tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_regex")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.na(validation$validation_set$value))
  expect_equivalent(validation$validation_set$regex, "[a-z]{3}")
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 7)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
})
