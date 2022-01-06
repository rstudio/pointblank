skip_on_cran()

library(lubridate)

increasing_tbl <-
  dplyr::tibble(
    a = c(5, 6, 7, 8, 9, 12),
    b = c(1, 2, NA,3, 4, 5),
    c = c(1, 2, 2, 4, 5, 7),
    d = c(1, 2, 1.9999, 4, 5, 7)
  )

decreasing_tbl <-
  dplyr::tibble(
    a = c(12, 9, 8, 7, 6, 5),
    b = c(6, 5, NA,4, 3, 1),
    c = c(7, 5, 5, 4, 2, 1),
    d = c(7, 5, 4, 4.0001, 3, 1)
  )

test_that("Interrogating with an agent yields the correct results", {
  
  # Use the `col_schema_match()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_schema_match(schema = col_schema(.tbl = small_table)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_schema_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_is(validation$validation_set[["values"]][[1]], "col_schema")
  expect_is(validation$validation_set[["values"]][[1]], "r_type")
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_schema_match()` function with a `col_schema` object
  # to create a validation step, then, `interrogate()`
  schema <- 
    col_schema(
      date_time = c("POSIXct", "POSIXt"), date = "Date", 
      a = "integer", b = "character", c = "numeric",
      d = "numeric", e = "logical", f = "character"
    )
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_schema_match(schema = schema) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_schema_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_is(validation$validation_set[["values"]][[1]], "col_schema")
  expect_is(validation$validation_set[["values"]][[1]], "r_type")
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Expect an error if a `col_schema` object isn't provided to `schema`
  expect_error(
    create_agent(tbl = small_table) %>%
      col_schema_match(
        schema = list(
          date_time = c("POSIXct", "POSIXt"), date = "Date", 
          a = "integer", b = "character", c = "numeric",
          d = "numeric",  e = "logical", f = "character"
        )
      )
  )
  
  
  # Use the `row_count_match()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(tbl_compare = small_table) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "row_count_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_true(inherits(validation$validation_set[["values"]][[1]], "tbl_df"))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `tbl_match()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    tbl_match(tbl_compare = small_table) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "tbl_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_true(inherits(validation$validation_set[["values"]][[1]], "tbl_df"))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 0)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0)
  expect_equivalent(validation$validation_set$f_failed, 1)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 0)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0)
  expect_equivalent(validation$validation_set$f_failed, 1)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 1)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 12)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.92308)
  expect_equivalent(validation$validation_set$f_failed, 0.07692)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(d),
      left = 0, right = 5000,
      preconditions = ~ . %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "d")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_between()` function to create
  # a validation step, then, `interrogate()`; using
  # column names for `left` and `right`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(c),
      left = vars(a), right = vars(d)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 6)
  expect_equivalent(validation$validation_set$f_passed, 0.53846)
  expect_equivalent(validation$validation_set$f_failed, 0.46154)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_gt()` function to create
  # a validation step (comparison with a character value)
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(f),
      left = "i", right = "p") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 6)
  expect_equivalent(validation$validation_set$f_passed, 0.53846)
  expect_equivalent(validation$validation_set$f_failed, 0.46154)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_between()` function to create
  # a validation step, then, `interrogate()`; using
  # a column name for `right`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(c),
      left = 0, right = vars(d)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_between")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
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
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 9)
  expect_equivalent(validation$validation_set$n_failed, 4)
  expect_equivalent(validation$validation_set$f_passed, 0.69231)
  expect_equivalent(validation$validation_set$f_failed, 0.30769)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_not_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(
      columns = vars(d),
      left = 500, right = 1000,
      preconditions = ~ . %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_between")
  expect_equivalent(validation$validation_set$column, "d")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 4)
  expect_equivalent(validation$validation_set$f_passed, 0.63636)
  expect_equivalent(validation$validation_set$f_failed, 0.36364)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
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
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 12)
  expect_equivalent(validation$validation_set$f_passed, 0.07692)
  expect_equivalent(validation$validation_set$f_failed, 0.92308)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(
      columns = vars(d),
      value = 283.94,
      preconditions = ~ . %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equivalent(validation$validation_set$column, "d")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 10)
  expect_equivalent(validation$validation_set$f_passed, 0.09091)
  expect_equivalent(validation$validation_set$f_failed, 0.90909)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_equal()` function to create
  # a validation step (equality for a character value)
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(
      columns = vars(f),
      value = "high"
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equivalent(validation$validation_set$column, "f")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "high")
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 6)
  expect_equivalent(validation$validation_set$n_failed, 7)
  expect_equivalent(validation$validation_set$f_passed, 0.46154)
  expect_equivalent(validation$validation_set$f_failed, 0.53846)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
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
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 12)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.92308)
  expect_equivalent(validation$validation_set$f_failed, 0.07692)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_not_equal()` function to create
  # a validation step, then, `interrogate()`; using
  # a column name for `value`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = vars(a), value = vars(d)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equivalent(validation$validation_set$column, "a")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_not_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(
      columns = vars(d),
      value = 283.94,
      preconditions = ~ . %>% dplyr::filter(date > "2016-01-04")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equivalent(validation$validation_set$column, "d")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 10)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.90909)
  expect_equivalent(validation$validation_set$f_failed, 0.09091)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_gt()` function to create
  # a validation step, then, `interrogate()`; using
  # a column name for `value`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(date_time), value = vars(date)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_gt")
  expect_equivalent(validation$validation_set$column, "date_time")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_gt()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`; using a column name for `value`
  expect_warning(regexp = NA,
    validation <-
      create_agent(tbl = small_table) %>%
      col_vals_gt(
        columns = vars(date_time), value = vars(date),
        preconditions = ~ . %>% dplyr::mutate(date = lubridate::as_datetime(date))) %>%
      interrogate()
  )
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_gt")
  expect_equivalent(validation$validation_set$column, "date_time")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_gt()` function to create
  # a validation step (comparison with a Date value)
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(date_time), value = "2016-01-05") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_gt")
  expect_equivalent(validation$validation_set$column, "date_time")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "2016-01-05")
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_gt()` function to create
  # a validation step (comparison with a character value)
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(f), value = "l") %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_gt")
  expect_equivalent(validation$validation_set$column, "f")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "l")
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 6)
  expect_equivalent(validation$validation_set$f_passed, 0.53846)
  expect_equivalent(validation$validation_set$f_failed, 0.46154)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `rows_distinct()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct() %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "rows_distinct")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `rows_distinct()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct(
      preconditions = ~ . %>% dplyr::filter(date != "2016-01-20")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "rows_distinct")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 11)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `rows_distinct()` function to create
  # a validation step for selected columns, then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct(columns = vars(date_time, a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "rows_distinct")
  expect_equivalent(validation$validation_set$column %>% unlist(), "date_time, a")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
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
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
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
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_make_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 4)
  expect_equivalent(validation$validation_set$n_passed, 4)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Make another pass with `col_vals_make_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "mid")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 3)
  expect_equivalent(validation$validation_set$n_passed, 2)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.66667)
  expect_equivalent(validation$validation_set$f_failed, 0.33333)
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Make another pass with `col_vals_make_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "middle")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 3)
  expect_equivalent(validation$validation_set$n_passed, 1)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.33333)
  expect_equivalent(validation$validation_set$f_failed, 0.66667)
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_make_subset()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 3)
  expect_equivalent(validation$validation_set$n_passed, 3)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Make another pass with the `col_vals_make_subset()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "mid")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 2)
  expect_equivalent(validation$validation_set$n_passed, 2)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Make another pass with the `col_vals_make_subset()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "mid", "high", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equivalent(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 4)
  expect_equivalent(validation$validation_set$n_passed, 3)
  expect_equivalent(validation$validation_set$n_failed, 1)
  expect_equivalent(validation$validation_set$f_passed, 0.75)
  expect_equivalent(validation$validation_set$f_failed, 0.25)
  
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 11)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.84615)
  expect_equivalent(validation$validation_set$f_failed, 0.15385)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_not_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(
      columns = vars(c),
      preconditions = ~ . %>%
        dplyr::filter(date > "2016-01-06" & date < "2016-01-30")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 8)
  expect_equivalent(validation$validation_set$n_passed, 8)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
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
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 2)
  expect_equivalent(validation$validation_set$n_failed, 11)
  expect_equivalent(validation$validation_set$f_passed, 0.15385)
  expect_equivalent(validation$validation_set$f_failed, 0.84615)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(
      columns = vars(c),
      preconditions = ~ . %>%
        dplyr::filter(date == '2016-01-06' | date == '2016-01-30')
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_null")
  expect_equivalent(validation$validation_set$column, "c")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 2)
  expect_equivalent(validation$validation_set$n_passed, 2)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  

  # Use the `col_vals_increasing()` function to create
  # several validation steps, then, `interrogate()`
  validation <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(vars(a)) %>%
    col_vals_increasing(vars(b)) %>%
    col_vals_increasing(vars(b), na_pass = TRUE) %>%
    col_vals_increasing(vars(c)) %>%
    col_vals_increasing(vars(c), allow_stationary = TRUE) %>%
    col_vals_increasing(vars(d), allow_stationary = TRUE) %>%
    col_vals_increasing(vars(d), decreasing_tol = 0.001) %>%
    col_vals_increasing(vars(d), decreasing_tol = 0.0001) %>%
    col_vals_increasing(vars(d), decreasing_tol = 0.00001) %>%
    col_vals_increasing(vars(d), allow_stationary = TRUE, decreasing_tol = 0.001) %>%
    col_vals_increasing(vars(d), allow_stationary = TRUE, decreasing_tol = 0.00001) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "increasing_tbl")
  expect_equal(
    validation$validation_set$assertion_type,
    rep("col_vals_increasing", 11)
  )
  expect_equal(
    unlist(validation$validation_set$column),
    c("a", "b", "b", "c", "c", "d", "d", "d", "d", "d", "d")
  )
  expect_equal(
    validation$validation_set[["values"]] %>% sapply(`[[`, 1),
    c(0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1)
  )
  expect_equal(
    validation$validation_set[["values"]] %>% sapply(`[[`, 2),
    c(0, 0, 0, 0, 0, 0, 0.001, 1e-04, 1e-05, 0.001, 1e-05)
  )
  expect_equal(
    validation$validation_set$all_passed,
    c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_equal(validation$validation_set$n, rep(6, 11))
  expect_equal(
    validation$validation_set$n_passed,
    c(6, 5, 6, 5, 6, 5, 6, 6, 5, 6, 5)
  )
  expect_equal(
    validation$validation_set$n_failed,
    c(0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1)
  )
  expect_equal(
    validation$validation_set$f_passed,
    c(1, 0.83333, 1, 0.83333, 1, 0.83333, 1, 1, 0.83333, 1, 0.83333)
  )
  expect_equal(
    validation$validation_set$f_failed,
    c(0, 0.16667, 0, 0.16667, 0, 0.16667, 0, 0, 0.16667, 0, 0.16667)
  )
  expect_equal(nrow(validation$validation_set), 11)
  
  
  # Use the `col_vals_decreasing()` function to create
  # several validation steps, then, `interrogate()`
  validation <-
    create_agent(tbl = decreasing_tbl) %>%
    col_vals_decreasing(vars(a)) %>%
    col_vals_decreasing(vars(b)) %>%
    col_vals_decreasing(vars(b), na_pass = TRUE) %>%
    col_vals_decreasing(vars(c)) %>%
    col_vals_decreasing(vars(c), allow_stationary = TRUE) %>%
    col_vals_decreasing(vars(d), allow_stationary = TRUE) %>%
    col_vals_decreasing(vars(d), increasing_tol = 0.001) %>%
    col_vals_decreasing(vars(d), increasing_tol = 0.0001) %>%
    col_vals_decreasing(vars(d), increasing_tol = 0.00001) %>%
    col_vals_decreasing(vars(d), allow_stationary = TRUE, increasing_tol = 0.001) %>%
    col_vals_decreasing(vars(d), allow_stationary = TRUE, increasing_tol = 0.00001) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "decreasing_tbl")
  expect_equal(
    validation$validation_set$assertion_type,
    rep("col_vals_decreasing", 11)
  )
  expect_equal(
    unlist(validation$validation_set$column),
    c("a", "b", "b", "c", "c", "d", "d", "d", "d", "d", "d")
  )
  expect_equal(
    validation$validation_set[["values"]] %>% sapply(`[[`, 1),
    c(0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1)
  )
  expect_equal(
    validation$validation_set[["values"]] %>% sapply(`[[`, 2),
    c(0, 0, 0, 0, 0, 0, 0.001, 1e-04, 1e-05, 0.001, 1e-05)
  )
  expect_equal(
    validation$validation_set$all_passed,
    c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_equal(validation$validation_set$n, rep(6, 11))
  expect_equal(
    validation$validation_set$n_passed,
    c(6, 5, 6, 5, 6, 5, 6, 6, 5, 6, 5)
  )
  expect_equal(
    validation$validation_set$n_failed,
    c(0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1)
  )
  expect_equal(
    validation$validation_set$f_passed,
    c(1, 0.83333, 1, 0.83333, 1, 0.83333, 1, 1, 0.83333, 1, 0.83333)
  )
  expect_equal(
    validation$validation_set$f_failed,
    c(0, 0.16667, 0, 0.16667, 0, 0.16667, 0, 0, 0.16667, 0, 0.16667)
  )
  expect_equal(nrow(validation$validation_set), 11)
  
  
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
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "[0-9]-[a-z]{3}-[0-9]{3}")
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 13)
  expect_equivalent(validation$validation_set$n_passed, 13)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_regex()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(
      columns = vars(f),
      regex = "[a-z]{3}",
      preconditions = ~ . %>% dplyr::filter(f != "high")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "small_table")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_regex")
  expect_equivalent(validation$validation_set$column, "f")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "[a-z]{3}")
  expect_true(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 7)
  expect_equivalent(validation$validation_set$n_passed, 7)
  expect_equivalent(validation$validation_set$n_failed, 0)
  expect_equivalent(validation$validation_set$f_passed, 1)
  expect_equivalent(validation$validation_set$f_failed, 0)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_within_spec()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = specifications) %>%
    col_vals_within_spec(
      columns = vars(isbn_numbers),
      spec = "isbn"
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "specifications")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_within_spec")
  expect_equivalent(validation$validation_set$column, "isbn_numbers")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "isbn")
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 8)
  expect_equivalent(validation$validation_set$n_passed, 5)
  expect_equivalent(validation$validation_set$n_failed, 3)
  expect_equivalent(validation$validation_set$f_passed, 0.625)
  expect_equivalent(validation$validation_set$f_failed, 0.375)
  expect_equivalent(nrow(validation$validation_set), 1)
  
  
  # Use the `col_vals_within_spec()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = specifications) %>%
    col_vals_within_spec(
      columns = vars(isbn_numbers),
      spec = "isbn",
      preconditions = ~ . %>% tidyr::drop_na()
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equivalent(validation$tbl_name, "specifications")
  expect_equivalent(validation$validation_set$assertion_type, "col_vals_within_spec")
  expect_equivalent(validation$validation_set$column, "isbn_numbers")
  expect_equivalent(validation$validation_set[["values"]] %>% unlist(), "isbn")
  expect_false(validation$validation_set$all_passed)
  expect_equivalent(validation$validation_set$n, 7)
  expect_equivalent(validation$validation_set$n_passed, 5)
  expect_equivalent(validation$validation_set$n_failed, 2)
  expect_equivalent(validation$validation_set$f_passed, 0.71429)
  expect_equivalent(validation$validation_set$f_failed, 0.28571)
  expect_equivalent(nrow(validation$validation_set), 1)
})

test_that("Interrogating with an agent incorporates the `na_pass` option", {
  
  # Use the `col_vals_equal()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_equal(
      columns = vars(a),
      value = 1.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()

  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_equal(
      columns = vars(a),
      value = 1.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_equal()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_not_equal(
      columns = vars(a),
      value = 2.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
    create_agent() %>%
    col_vals_not_equal(
      columns = vars(a),
      value = 2.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_gt()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gt(
      columns = vars(a),
      value = 0.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gt(
      columns = vars(a),
      value = 0.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_gte()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gte(
      columns = vars(a),
      value = 1.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_gte(
      columns = vars(a),
      value = 1.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_lt()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lt(
      columns = vars(a),
      value = 3.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lt(
      columns = vars(a),
      value = 3.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_lte()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lte(
      columns = vars(a),
      value = 2.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_lte(
      columns = vars(a),
      value = 2.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_between()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_between(
      columns = vars(a),
      left = 0, right = 3.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_between(
      columns = vars(a),
      left = 0, right = 3.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_between()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_not_between(
      columns = vars(a),
      left = 3.0, right = 4.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
    create_agent() %>%
    col_vals_not_between(
      columns = vars(a),
      left = 3.0, right = 4.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_regex()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
    create_agent() %>%
    col_vals_regex(
      columns = vars(a),
      regex = "[0-9]-[a-z]{3}-[0-9]{3}",
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
    create_agent() %>%
    col_vals_regex(
      columns = vars(a),
      regex = "[0-9]-[a-z]{3}-[0-9]{3}",
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  create_agent(tbl = ~ specifications[1:6, ]) %>%
    col_vals_within_spec(columns = vars(isbn_numbers), spec = "isbn13", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(vin_numbers), spec = "VIN", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(zip_codes), spec = "zip", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(credit_card_numbers), spec = "credit_card", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(iban_austria), spec = "iban[AT]", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(swift_numbers), spec = "swift-bic", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(phone_numbers), spec = "phone", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(email_addresses), spec = "email", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(urls), spec = "url", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(ipv4_addresses), spec = "ipv4", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(ipv6_addresses), spec = "ipv6", na_pass = TRUE) %>%
    col_vals_within_spec(columns = vars(mac_addresses), spec = "mac", na_pass = TRUE) %>%
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
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c("one", "two", "three", NA)) %>%
    create_agent() %>%
    col_vals_in_set(
      columns = vars(a),
      set = c("one", "two", "three", "four", "five", NA),
      actions = action_levels(warn_at = 1)
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
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  dplyr::tibble(a = c("one", "two", "three", NA)) %>%
    create_agent() %>%
    col_vals_not_in_set(
      columns = vars(a),
      set = c("four", "five", "six"),
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
})

test_that("Select validation steps can be `active` or not", {
  
  # Perform validation with default of `active = TRUE` in
  # each validation step
  validation_all_active <-
    create_agent(tbl = small_table) %>%
    col_exists(columns = vars(b)) %>%
    col_is_character(columns = vars(b)) %>%
    col_is_numeric(columns = vars(a)) %>%
    col_is_posix(columns = vars(date_time)) %>%
    col_is_date(columns = vars(date)) %>%
    col_is_integer(columns = vars(a)) %>%
    col_is_logical(columns = vars(e)) %>%
    col_vals_between(columns = vars(d), left = 0, right = 5000) %>%
    col_vals_equal(columns = vars(d), value = 283.94) %>%
    col_vals_gt(columns = vars(date_time), value = vars(date)) %>%
    col_vals_gte(columns = vars(date_time), value = vars(date)) %>%
    col_vals_lt(columns = vars(date_time), value = vars(date)) %>%
    col_vals_lte(columns = vars(date_time), value = vars(date)) %>%
    col_vals_in_set(columns = vars(f), set = c("low", "mid", "high")) %>%
    col_vals_not_between(columns = vars(d), left = 500, right = 1000) %>%
    col_vals_not_equal(columns = vars(d), value = 283.94) %>%
    col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher")) %>%
    col_vals_not_null(columns = vars(c)) %>%
    col_vals_null(columns = vars(b)) %>%
    col_vals_regex(columns = vars(f), regex = "[a-z]{3}") %>%
    rows_distinct() %>%
    conjointly(
      ~ col_vals_gt(., columns = vars(a), value = 1),
      ~ col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
      ~ col_vals_not_null(., columns = vars(d))
    ) %>%
    serially(
      ~ test_col_vals_gt(., columns = vars(a), value = 0),
      ~ test_col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
      ~ col_vals_not_null(., columns = vars(d))
    ) %>%
    specially(
      fn = function(x) {
        as.integer(x$date) <= as.integer(x$date_time)
      }
    ) %>%
    interrogate()
  
  # Expect the `active` parameter in each validation step
  # to be set as `TRUE`
  expect_true(all(validation_all_active$validation_set$eval_active))
  
  # Expect validation results to be available in all of the columns
  # where those values are reported
  expect_true(all(!is.na(validation_all_active$validation_set$eval_error)))
  expect_true(all(!is.na(validation_all_active$validation_set$eval_warning)))
  expect_true(all(!is.na(validation_all_active$validation_set$all_passed)))
  expect_true(all(!is.na(validation_all_active$validation_set$n)))
  expect_true(all(!is.na(validation_all_active$validation_set$n_passed)))
  expect_true(all(!is.na(validation_all_active$validation_set$n_failed)))
  expect_true(all(!is.na(validation_all_active$validation_set$f_passed)))
  expect_true(all(!is.na(validation_all_active$validation_set$f_failed)))
  expect_true(all(!is.na(validation_all_active$validation_set$time_processed)))
  expect_true(all(!is.na(validation_all_active$validation_set$proc_duration_s)))
  
  # Perform validation with `active = FALSE` for all of the
  # validation steps
  validation_all_not_active <-
    create_agent(tbl = small_table) %>%
    col_exists(columns = vars(b), active = FALSE) %>%
    col_is_character(columns = vars(b), active = FALSE) %>%
    col_is_numeric(columns = vars(a), active = FALSE) %>%
    col_is_posix(columns = vars(date_time), active = FALSE) %>%
    col_is_date(columns = vars(date), active = FALSE) %>%
    col_is_integer(columns = vars(a), active = FALSE) %>%
    col_is_logical(columns = vars(e), active = FALSE) %>%
    col_vals_between(columns = vars(d), left = 0, right = 5000, active = FALSE) %>%
    col_vals_equal(columns = vars(d), value = 283.94, active = FALSE) %>%
    col_vals_gt(columns = vars(date_time), value = vars(date), active = FALSE) %>%
    col_vals_gte(columns = vars(date_time), value = vars(date), active = FALSE) %>%
    col_vals_lt(columns = vars(date_time), value = vars(date), active = FALSE) %>%
    col_vals_lte(columns = vars(date_time), value = vars(date), active = FALSE) %>%
    col_vals_in_set(columns = vars(f), set = c("low", "mid", "high"), active = FALSE) %>%
    col_vals_not_between(columns = vars(d), left = 500, right = 1000, active = FALSE) %>%
    col_vals_not_equal(columns = vars(d), value = 283.94, active = FALSE) %>%
    col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher"), active = FALSE) %>%
    col_vals_not_null(columns = vars(c), active = FALSE) %>%
    col_vals_null(columns = vars(b), active = FALSE) %>%
    col_vals_regex(columns = vars(f), regex = "[a-z]{3}", active = FALSE) %>%
    rows_distinct(active = FALSE) %>%
    conjointly(
      ~ col_vals_gt(., columns = vars(a), value = 1),
      ~ col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
      ~ col_vals_not_null(., columns = vars(d)),
      active = FALSE
    ) %>%
    serially(
      ~ test_col_vals_gt(., columns = vars(a), value = 0),
      ~ test_col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
      ~ col_vals_not_null(., columns = vars(d)),
      active = FALSE
    ) %>%
    specially(
      fn = function(x) {
        as.integer(x$date) <= as.integer(x$date_time)
      },
      active = FALSE
    ) %>%
    interrogate()
  
  # Expect the `active` parameter in each validation step
  # to be set as `FALSE`
  expect_true(!all(validation_all_not_active$validation_set$eval_active))
  
  # Expect no validation results to be available in any of the columns
  # where those values are normally reported (this is because no interrogations
  # had occurred at any of the validation steps)
  expect_true(!all(!is.na(validation_all_not_active$validation_set$eval_error)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$eval_warning)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$all_passed)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$n)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$n_passed)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$n_failed)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$f_passed)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$f_failed)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$time_processed)))
  expect_true(!all(!is.na(validation_all_not_active$validation_set$proc_duration_s)))
  
  # Perform validation directly on data with `active = TRUE`
  # for all of the validation steps; set action levels to
  # warn when there is a single unit failing in each step
  al <- action_levels(warn_at = 1)
  
  expect_warning(
    small_table %>%
      col_is_character(columns = vars(b), actions = al) %>%
      col_is_numeric(columns = vars(a), actions = al) %>%
      col_is_posix(columns = vars(date_time), actions = al) %>%
      col_is_date(columns = vars(date), actions = al) %>%
      col_is_integer(columns = vars(a), actions = al) %>%
      col_is_logical(columns = vars(e), actions = al) %>%
      col_vals_between(columns = vars(d), left = 0, right = 5000, actions = al) %>%
      col_vals_equal(columns = vars(d), value = 283.94, actions = al) %>%
      col_vals_gt(columns = vars(date_time), value = vars(date), actions = al) %>%
      col_vals_gte(columns = vars(date_time), value = vars(date), actions = al) %>%
      col_vals_lt(columns = vars(date_time), value = vars(date), actions = al) %>%
      col_vals_lte(columns = vars(date_time), value = vars(date), actions = al) %>%
      col_vals_in_set(columns = vars(f), set = c("low", "mid", "high"), actions = al) %>%
      col_vals_not_between(columns = vars(d), left = 500, right = 1000, actions = al) %>%
      col_vals_not_equal(columns = vars(d), value = 283.94, actions = al) %>%
      col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher"), actions = al) %>%
      col_vals_not_null(columns = vars(c), actions = al) %>%
      col_vals_null(columns = vars(b), actions = al) %>%
      col_vals_regex(columns = vars(f), regex = "[a-z]{3}", actions = al) %>%
      rows_distinct(actions = al) %>%
      conjointly(
        ~ col_vals_gt(., columns = vars(a), value = 1),
        ~ col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
        ~ col_vals_not_null(., columns = vars(d)),
        actions = al
      ) %>%
      serially(
        ~ test_col_vals_gt(., columns = vars(a), value = 0),
        ~ test_col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
        ~ col_vals_not_null(., columns = vars(d)),
        actions = al
      ) %>%
      specially(
        fn = function(x) {
          as.integer(x$date) <= as.integer(x$date_time)
        },
        actions = al
      )
  )
  
  # Perform validation directly on data with `active = FALSE`
  # for all of the validation steps; using the same action levels
  # to potentially warn when there is a single unit failing in each step
  # (however this won't happen because `active = FALSE` means that an
  # interrogation isn't even performed)
  expect_warning(regexp = NA,
    small_table %>%
      col_is_character(columns = vars(b), actions = al, active = FALSE) %>%
      col_is_numeric(columns = vars(a), actions = al, active = FALSE) %>%
      col_is_posix(columns = vars(date_time), actions = al, active = FALSE) %>%
      col_is_date(columns = vars(date), actions = al, active = FALSE) %>%
      col_is_integer(columns = vars(a), actions = al, active = FALSE) %>%
      col_is_logical(columns = vars(e), actions = al, active = FALSE) %>%
      col_vals_between(columns = vars(d), left = 0, right = 5000, actions = al, active = FALSE) %>%
      col_vals_equal(columns = vars(d), value = 283.94, actions = al, active = FALSE) %>%
      col_vals_gt(columns = vars(date_time), value = vars(date), actions = al, active = FALSE) %>%
      col_vals_gte(columns = vars(date_time), value = vars(date), actions = al, active = FALSE) %>%
      col_vals_lt(columns = vars(date_time), value = vars(date), actions = al, active = FALSE) %>%
      col_vals_lte(columns = vars(date_time), value = vars(date), actions = al, active = FALSE) %>%
      col_vals_in_set(columns = vars(f), set = c("low", "mid", "high"), actions = al, active = FALSE) %>%
      col_vals_not_between(columns = vars(d), left = 500, right = 1000, actions = al, active = FALSE) %>%
      col_vals_not_equal(columns = vars(d), value = 283.94, actions = al, active = FALSE) %>%
      col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher"), actions = al, active = FALSE) %>%
      col_vals_not_null(columns = vars(c), actions = al, active = FALSE) %>%
      col_vals_null(columns = vars(b), actions = al, active = FALSE) %>%
      col_vals_regex(columns = vars(f), regex = "[a-z]{3}", actions = al, active = FALSE) %>%
      rows_distinct(actions = al, active = FALSE) %>%
      conjointly(
        ~ col_vals_gt(., columns = vars(a), value = 1),
        ~ col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
        ~ col_vals_not_null(., columns = vars(d)),
        actions = al, active = FALSE
      ) %>%
      serially(
        ~ test_col_vals_gt(., columns = vars(a), value = 0),
        ~ test_col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE),
        ~ col_vals_not_null(., columns = vars(d)),
        actions = al, active = FALSE
      ) %>%
      specially(
        fn = function(x) {
          as.integer(x$date) <= as.integer(x$date_time)
        },
        actions = al
      )
  )
})

test_that("Some validation steps become inactive based on select expressions", {

  agent <- create_agent(tbl = small_table, label = "::QUIET::")
  
  check_eval_active_false <- function(agent) {
    
    suppressMessages(
      agent %>%
        interrogate() %>% 
        .$validation_set %>%
        dplyr::pull(eval_active) %>% 
        expect_false()
    )
  }
  
  eval_select <- function(select_expr) {
    rlang::eval_bare(rlang::f_rhs(select_expr))
  }
  
  select_exprs <-
    c(
      ~ starts_with("z"),
      ~ ends_with("z"),
      ~ contains("z"),
      ~ matches("z")
    )
  
  for (i in seq_along(select_exprs)) {
    
    agent %>% col_vals_lt(eval_select(select_exprs[[i]]), value = 5) %>%
      check_eval_active_false()
    agent %>% col_vals_lte(eval_select(select_exprs[[i]]), value = 5) %>%
      check_eval_active_false()
    agent %>% col_vals_equal(eval_select(select_exprs[[i]]), value = 5) %>%
      check_eval_active_false()
    agent %>% col_vals_not_equal(eval_select(select_exprs[[i]]), value = 5) %>%
      check_eval_active_false()
    agent %>% col_vals_gte(eval_select(select_exprs[[i]]), value = 5) %>%
      check_eval_active_false()
    agent %>% col_vals_gt(eval_select(select_exprs[[i]]), value = 5) %>%
      check_eval_active_false()
    agent %>% col_vals_between(eval_select(select_exprs[[i]]), 2, 5) %>%
      check_eval_active_false()
    agent %>% col_vals_not_between(eval_select(select_exprs[[i]]), 2, 5) %>%
      check_eval_active_false()
    agent %>% col_vals_in_set(eval_select(select_exprs[[i]]), c(2, 5)) %>%
      check_eval_active_false()
    agent %>% col_vals_not_in_set(eval_select(select_exprs[[i]]), c(2, 5)) %>%
      check_eval_active_false()
    agent %>% col_vals_make_set(eval_select(select_exprs[[i]]), c(2, 5)) %>%
      check_eval_active_false()
    agent %>% col_vals_make_subset(eval_select(select_exprs[[i]]), c(2, 5)) %>%
      check_eval_active_false()
    agent %>% col_vals_null(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_vals_not_null(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_vals_increasing(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_vals_decreasing(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_vals_regex(eval_select(select_exprs[[i]]), regex = "abc") %>%
      check_eval_active_false()
    agent %>% col_vals_within_spec(eval_select(select_exprs[[i]]), spec = "email") %>%
      check_eval_active_false()
    agent %>% col_is_character(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_is_numeric(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_is_integer(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_is_logical(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_is_date(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_is_posix(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
    agent %>% col_is_factor(eval_select(select_exprs[[i]])) %>%
      check_eval_active_false()
  }
})
