context("Performing interrogations with an agent")

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

test_that("Interrogating with an agent yields the correct results", {
  
  # Use the `col_exists()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_exists(columns = vars(b)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_exists")
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
  
  # Use the `col_exists()` function to create
  # a validation step, then, `interrogate()`
  # (expecting a failed interrogation)
  validation <-
    create_agent(tbl = small_table) %>%
    col_exists(columns = vars(g)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_exists")
  expect_equivalent(validation$validation_set$column, "g")
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
  
  # Use the `col_is_character()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = vars(b)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_is_numeric()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(columns = vars(a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_is_posix()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_posix(columns = vars(date_time)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_is_date()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_date(columns = vars(date)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_is_integer()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_integer(columns = vars(a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_is_logical()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_logical(columns = vars(e)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_between()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(d),
      left = 0, right = 5000
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(d),
      left = 0, right = 5000,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_not_between()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(
      columns = vars(d),
      left = 500, right = 1000
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_not_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(
      columns = vars(d),
      left = 500, right = 1000,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_equal()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(columns = vars(d), value = 283.94) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(
      columns = vars(d),
      value = 283.94,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_not_equal()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = vars(d), value = 283.94) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(
      columns = vars(d),
      value = 283.94,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `rows_not_duplicated()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated() %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated(
      preconditions = ~ tbl %>% dplyr::filter(date != "2016-01-20")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_equal()` function to create
  # a validation step for selected columns, then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_not_duplicated(columns = vars(date_time, a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_in_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = vars(f), set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_not_in_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = vars(f), set = c("lower", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
    col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_not_null()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(columns = vars(c)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_not_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(
      columns = vars(c),
      preconditions = ~ tbl %>%
        dplyr::filter(date > "2016-01-06" & date < "2016-01-30")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_null()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(columns = vars(c)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(
      columns = vars(c),
      preconditions = ~ tbl %>%
        dplyr::filter(date == '2016-01-06' | date == '2016-01-30')
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_regex()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(
      columns = vars(b),
      regex = "[0-9]-[a-z]{3}-[0-9]{3}"
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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
  
  # Use the `col_vals_regex()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(
      columns = vars(f),
      regex = "[a-z]{3}",
      preconditions = ~ tbl %>% dplyr::filter(f != "high")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
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

test_that("Interrogating with an agent incorporates the `incl_na` option", {
  
  # Use the `col_vals_equal()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_equal(
      columns = vars(a),
      value = 1.5,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()

  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_equal(
      columns = vars(a),
      value = 1.5,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_equal()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_not_equal(
      columns = vars(a),
      value = 2.0,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_not_equal(
      columns = vars(a),
      value = 2.0,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_gt()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gt(
      columns = vars(a),
      value = 0.5,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gt(
      columns = vars(a),
      value = 0.5,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_gte()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gte(
      columns = vars(a),
      value = 1.0,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gte(
      columns = vars(a),
      value = 1.0,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_lt()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lt(
      columns = vars(a),
      value = 3.0,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lt(
      columns = vars(a),
      value = 3.0,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_lte()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lte(
      columns = vars(a),
      value = 2.5,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lte(
      columns = vars(a),
      value = 2.5,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_between()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_between(
      columns = vars(a),
      left = 0, right = 3.0,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_between(
      columns = vars(a),
      left = 0, right = 3.0,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_between()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_not_between(
      columns = vars(a),
      left = 3.0, right = 4.5,
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_not_between(
      columns = vars(a),
      left = 3.0, right = 4.5,
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_regex()` function to perform
  # a validation step with NAs, switching the
  # value of the `incl_na` option
  dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
    create_agent() %>%
    col_vals_regex(
      columns = vars(a),
      regex = "[0-9]-[a-z]{3}-[0-9]{3}",
      incl_na = FALSE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
    create_agent() %>%
    col_vals_regex(
      columns = vars(a),
      regex = "[0-9]-[a-z]{3}-[0-9]{3}",
      incl_na = TRUE,
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
})

test_that("The validations with sets can include NA values", {
  
  # Use the `col_vals_in_set()` function to perform
  # a validation step with NAs
  dplyr::tibble(a = c("one", "two", "three", NA)) %>%
    create_agent() %>%
    col_vals_in_set(
      columns = vars(a),
      set = c("one", "two", "three", "four", "five"),
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c("one", "two", "three", NA)) %>%
    create_agent() %>%
    col_vals_in_set(
      columns = vars(a),
      set = c("one", "two", "three", "four", "five", NA),
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_in_set()` function to perform
  # a validation step with NAs
  dplyr::tibble(a = c("one", "two", "three", NA)) %>%
    create_agent() %>%
    col_vals_not_in_set(
      columns = vars(a),
      set = c("four", "five", "six", NA),
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c("one", "two", "three", NA)) %>%
    create_agent() %>%
    col_vals_not_in_set(
      columns = vars(a),
      set = c("four", "five", "six"),
      warn_count = 1
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
})
