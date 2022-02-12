skip_on_cran()

tbl <- 
  small_table %>%
  dplyr::mutate(g = as.factor(f))

tbl_complete_yes <-
  dplyr::tibble(
    a = c(5, 7, 6, 5, 8, 7),
    b = c(3, 4, 6, 8, 9, 11),
    c = c(2, 6, 8, 2, 3, 8)
  )

tbl_complete_no <-
  dplyr::tibble(
    a = c(5, 7, 6, 5, 8, 7),
    b = c(3, 4, NA, 8, 9, 11),
    c = c(2, 6, 8, NA, 3, 8)
  )

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

schema_incorrect <- 
  col_schema(
    date_time = "POSIXct", date = "Date", 
    a = "integer", b = "character", c = "numeric", d = "numeric", 
    e = "logical", f = "character", g = "factor"
  )

schema_correct <- 
  schema <- 
  col_schema(
    date_time = c("POSIXct", "POSIXt"), date = "Date", 
    a = "integer", b = "character", c = "numeric", d = "numeric", 
    e = "logical", f = "character", g = "factor"
  )

test_that("Interrogating simply returns the expected results", {
  
  #
  # col_vals_lt()
  #
  
  # Use the `col_vals_lt()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_lt(
      columns = vars(a),
      value = 10,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_lt(
        columns = vars(a),
        value = 7,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_lt(
        columns = vars(a),
        value = 7,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_lte()
  #
  
  # Use the `col_vals_lte()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_lte(
      columns = vars(a),
      value = 8,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_lte(
        columns = vars(a),
        value = 7,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_lte(
        columns = vars(a),
        value = 7,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_equal()
  #
  
  # Use the `col_vals_equal()` function to perform
  # a simple validation step
  suppressWarnings(
    tbl_result <- 
      tbl %>%
      col_vals_equal(
        columns = vars(d),
        value = 283.94,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_equal(
        columns = vars(d), value = 283.94,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_equal(
        columns = vars(d),
        value = 283.94,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_not_equal()
  #
  
  # Use the `col_vals_not_equal()` function to perform
  # a simple validation step
  suppressWarnings(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(
        columns = vars(d),
        value = 283.94,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(
        columns = vars(d),
        value = 283.94,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(
        columns = vars(d),
        value = 283.94,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_gte()
  #
  
  # Use the `col_vals_gte()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_gte(
      columns = vars(a),
      value = 1,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_gte(
        columns = vars(a),
        value = 2,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_gte(
        columns = vars(a),
        value = 3,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_gt()
  #
  
  # Use the `col_vals_gt()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_gt(
      columns = vars(a),
      value = 0,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_gt(
        columns = vars(a),
        value = 1,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_gt(
        columns = vars(a),
        value = 2,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_between()
  #
  
  # Use the `col_vals_between()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_between(
      columns = vars(d),
      left = 0, right = 10000,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_between(
        columns = vars(d),
        left = 0, right = 1000,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_between(
        columns = vars(d),
        left = 0, right = 1000,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_not_between()
  #
  
  # Use the `col_vals_not_between()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_between(
      columns = vars(d),
      left = 15000, right = 20000,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_between(
        columns = vars(d),
        left = 9000, right = 11000,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_between(
        columns = vars(d),
        left = 9000, right = 11000,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_not_between()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_not_between(
      columns = vars(d),
      left = 3000, right = 10000,
      preconditions = ~ . %>% dplyr::filter(date > "2016-01-20"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_between(
        columns = vars(d),
        left = 0, right = 3000,
        preconditions = ~ . %>% dplyr::filter(date > "2016-01-20"),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_between(
        columns = vars(d),
        left = 0, right = 3000,
        preconditions = ~ . %>% dplyr::filter(date > "2016-01-20"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_in_set()
  #
  
  # Use the `col_vals_in_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_in_set(
      columns = vars(f),
      set = c("low", "mid", "high"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_in_set(
        columns = vars(f),
        set = c("low", "mid", "higher"),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_in_set(
        columns = vars(f),
        set = c("low", "mid", "higher"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_not_in_set()
  #
  
  # Use the `col_vals_not_in_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_in_set(
      columns = vars(f),
      set = c("lower", "higher"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_in_set(
        columns = vars(f),
        set = c("lower", "mid", "higher"),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_in_set(
        columns = vars(f),
        set = c("lower", "mid", "higher"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))

  #
  # col_vals_make_set()
  #
  
  # Use the `col_vals_make_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_make_set(
      columns = vars(f),
      set = c("low", "mid", "high"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_make_set(
        columns = vars(f),
        set = c("low", "mid", "higher", "highest"),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_in_set(
        columns = vars(f),
        set = c("low", "mid", "higher", "highest"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Perform another simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_in_set(
        columns = vars(f),
        set = c("low", "mid"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_make_subset()
  #
  
  # Use the `col_vals_make_subset()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_make_subset(
      columns = vars(f),
      set = c("low", "mid", "high"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Use the `col_vals_make_subset()` function to perform
  # another simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_make_subset(
      columns = vars(f),
      set = c("low", "mid"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_make_subset(
        columns = vars(f),
        set = c("low", "mid", "highest"),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_make_subset(
        columns = vars(f),
        set = c("lower", "mid", "higher", "highest"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Perform another simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_make_subset(
        columns = vars(f),
        set = c("low", "mid", "highly"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_increasing()
  #
  
  # Use the `col_vals_increasing()` function to perform
  # a simple validation step
  tbl_result <- 
    increasing_tbl %>%
    col_vals_increasing(
      columns = vars(a),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `increasing_tbl`
  expect_equivalent(increasing_tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      increasing_tbl %>%
      col_vals_increasing(
        columns = vars(b),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `increasing_tbl`
  expect_equivalent(increasing_tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      increasing_tbl %>%
      col_vals_increasing(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_decreasing()
  #
  
  # Use the `col_vals_decreasing()` function to perform
  # a simple validation step
  tbl_result <- 
    decreasing_tbl %>%
    col_vals_decreasing(
      columns = vars(a),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `decreasing_tbl`
  expect_equivalent(decreasing_tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      decreasing_tbl %>%
      col_vals_decreasing(
        columns = vars(b),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `decreasing_tbl`
  expect_equivalent(decreasing_tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      decreasing_tbl %>%
      col_vals_decreasing(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_null()
  #
  
  # Use the `col_vals_null()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_null(
      columns = vars(c),
      preconditions = ~ . %>% dplyr::filter(date == "2016-01-06"),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_null(
        columns = vars(c),
        preconditions = ~ . %>% dplyr::filter(date != "2016-01-06"),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_null(
        columns = vars(c),
        preconditions = ~ . %>% dplyr::filter(date != "2016-01-06"),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_not_null()
  #
  
  # Use the `col_vals_not_null()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_null(
      columns = vars(a),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(
        columns = vars(c),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(
        columns = vars(c),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_regex()
  #
  
  # Use the `col_vals_regex()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_regex(
      columns = vars(b),
      regex = "[0-9]-[a-z]*?-[0-9]*?", 
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_regex(
        columns = vars(b),
        regex = "[0-7]-[a-z]*?-[0-9]*?",
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_regex(
        columns = vars(b),
        regex = "[0-7]-[a-z]*?-[0-9]*?",
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_within_spec()
  #
  
  # Use the `col_vals_within_spec()` function to perform
  # a simple validation step
  tbl_result <- 
    specifications[1:5, ] %>%
    col_vals_within_spec(
      columns = vars(isbn_numbers),
      spec = "isbn",
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `specifications[1:5, ]`
  expect_equivalent(specifications[1:5, ], tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      specifications[1:6, ] %>%
      col_vals_within_spec(
        columns = vars(isbn_numbers),
        spec = "isbn",
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `specifications[1:6, ]`
  expect_equivalent(specifications[1:6, ], tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      specifications %>%
      col_vals_within_spec(
        columns = vars(isbn_numbers),
        spec = "isbn",
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_vals_expr()
  #
  
  # Use the `col_vals_expr()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_expr(
      expr = ~ a %% 1 == 0,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_expr(
        expr = ~ a %% 2 == 0,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_expr(
        expr = ~ a %% 2 == 0,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # rows_distinct()
  #
  
  # Use the `rows_distinct()` function to perform
  # a simple validation step
  tbl_result <- tbl[1:2, ] %>% rows_distinct()
  
  # Expect that `tbl_result` is equivalent to `tbl[1:2, ]`
  expect_equivalent(tbl[1:2, ], tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% rows_distinct(actions = warn_on_fail())
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% rows_distinct(actions = stop_on_fail())
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # rows_complete()
  #
  
  # Use the `rows_complete()` function to perform
  # a simple validation step
  tbl_result <- tbl_complete_yes %>% rows_complete()
  
  # Expect that `tbl_result` is equivalent to `tbl_complete_yes`
  expect_equivalent(tbl_complete_yes, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl_complete_no %>% rows_complete(actions = warn_on_fail())
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl_complete_no, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl_complete_no %>% rows_complete(actions = stop_on_fail())
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_character()
  #
  
  # Use the `col_is_character()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_character(columns = vars(b))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_is_character(
        columns = vars(a),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_is_character(
        columns = vars(a),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_numeric()
  #
  
  # Use the `col_is_numeric()` function to perform
  # a simple validation step
  suppressWarnings(
    tbl_result <-
      tbl %>%
      col_is_numeric(
        columns = vars(a),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_is_numeric(
        columns = vars(b),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_is_numeric(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_integer()
  #
  
  # Use the `col_is_integer()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_is_integer(
      columns = vars(a),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_is_integer(
        columns = vars(b),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_is_integer(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_posix()
  #
  
  # Use the `col_is_posix()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_is_posix(
      columns = vars(date_time),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_is_posix(
        columns = vars(b),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_is_posix(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_date()
  #
  
  # Use the `col_is_date()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_is_date(
      columns = vars(date),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_is_date(
        columns = vars(b),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_is_date(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_logical()
  #
  
  # Use the `col_is_logical()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_is_logical(columns = vars(e), actions = warn_on_fail())
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_is_logical(columns = vars(b), actions = warn_on_fail())
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_is_logical(
        columns = vars(b),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_is_factor()
  #
  
  # Use the `col_is_factor()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_factor(columns = vars(g))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>% 
      col_is_factor(
        columns = vars(a),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>% 
      col_is_factor(
        columns = vars(a),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # col_exists()
  #
  
  # Using the `col_exists()` function to perform
  # a simple validation step results in a warning
  expect_warning(
    tbl %>% col_exists(columns = vars(h), actions = warn_on_fail())
  )
  expect_warning(
    tbl %>% col_exists(columns = "h", actions = warn_on_fail())
  )
  expect_warning(
    tbl %>% col_exists(columns = vars(a, h), actions = warn_on_fail())
  )
  expect_warning(
    tbl %>% col_exists(columns = c("a", "h"), actions = warn_on_fail())
  )
  
  # Using the `col_exists()` function to perform
  # a simple validation step results in an error
  expect_error(
    tbl %>% col_exists(columns = vars(h), actions = stop_on_fail())
  )
  expect_error(
    tbl %>% col_exists(columns = "h", actions = stop_on_fail())
  )
  expect_error(
    tbl %>% col_exists(columns = vars(a, h), actions = stop_on_fail())
  )
  expect_error(
    tbl %>% col_exists(columns = c("a", "h"), actions = stop_on_fail())
  )
  
  # Expect no warning or error if all column names are correct
  expect_warning(regexp = NA,
    tbl %>% col_exists(columns = colnames(tbl), actions = warn_on_fail())
  )
  expect_error(regexp = NA,
    tbl %>% col_exists(columns = colnames(tbl), actions = stop_on_fail())
  )
  
  #
  # col_schema_match()
  #
  
  # Use the `col_schema_match()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl[1:2, ] %>%
    col_schema_match(
      schema = col_schema(.tbl = tbl),
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `tbl[1:2, ]`
  expect_equivalent(tbl[1:2, ], tbl_result)
  
  # Use the `col_schema_match()` function to perform
  # another simple validation step
  tbl_result <- tbl[1:2, ] %>% col_schema_match(schema = schema_correct)
  
  # Expect that `tbl_result` is equivalent to `tbl[1:2, ]`
  expect_equivalent(tbl[1:2, ], tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_schema_match(
        schema = schema_incorrect,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% 
      col_schema_match(
        schema = schema_incorrect,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # row_count_match()
  #
  
  # Use the `row_count_match()` function to perform
  # a simple validation step
  tbl_result <- 
    small_table %>%
    row_count_match(
      count = pointblank::small_table,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `small_table`
  expect_equivalent(small_table, tbl_result)
  
  # Use the `row_count_match()` function to perform
  # another simple validation step
  tbl_result <- 
    small_table %>%
    row_count_match(count = ~ pointblank::small_table)
  
  # Expect that `tbl_result` is equivalent to `small_table`
  expect_equivalent(small_table, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl_complete_yes %>%
      row_count_match(
        count = pointblank::small_table,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl_complete_yes`
  expect_equivalent(tbl_complete_yes, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl_complete_yes %>%
      row_count_match(
        tbl_compare = pointblank::small_table,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # tbl_match()
  #
  
  # Use the `tbl_match()` function to perform
  # a simple validation step
  tbl_result <- 
    small_table %>%
    tbl_match(
      tbl_compare = pointblank::small_table,
      actions = warn_on_fail()
    )
  
  # Expect that `tbl_result` is equivalent to `small_table`
  expect_equivalent(small_table, tbl_result)
  
  # Use the `tbl_match()` function to perform
  # another simple validation step
  tbl_result <- 
    small_table %>%
    tbl_match(tbl_compare = ~ pointblank::small_table)
  
  # Expect that `tbl_result` is equivalent to `small_table`
  expect_equivalent(small_table, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl_complete_yes %>%
      tbl_match(
        tbl_compare = pointblank::small_table,
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl_complete_yes`
  expect_equivalent(tbl_complete_yes, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl_complete_yes %>%
      tbl_match(
        tbl_compare = pointblank::small_table,
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  #
  # conjointly()
  #
  
  # Use the `conjointly()` function to perform
  # a conjoint validation validation step directly
  # on the `tbl` object
  suppressWarnings(
    tbl_result <- 
      tbl %>%
      conjointly(
        ~ col_vals_gt(., columns = vars(d), value = 200),
        ~ col_vals_lt(., columns = vars(c), value = 10),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      conjointly(
        ~ col_vals_gt(., columns = vars(d), value = 200),
        ~ col_vals_lt(., columns = vars(c), value = 10),
        actions = warn_on_fail()
      )
  )
  expect_warning(
    tbl_result <- 
      tbl %>%
      conjointly(
        ~ col_vals_gt(., columns = vars(d), value = 200),
        ~ col_vals_lt(., columns = vars(c), value = 10),
        actions = warn_on_fail()
      )
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      conjointly(
        ~ col_vals_gt(., columns = vars(d), value = 200),
        ~ col_vals_lt(., columns = vars(c), value = 10),
        actions = stop_on_fail()
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
})

test_that("Interrogating simply incorporates the `na_pass` option", {
  
  # Use the `col_vals_equal()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
      col_vals_equal(
        columns = vars(a),
        value = 1.5,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
      col_vals_equal(
        columns = vars(a),
        value = 1.5,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_not_equal()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
      col_vals_not_equal(
        columns = vars(a),
        value = 2.0,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
      col_vals_not_equal(
        columns = vars(a),
        value = 2.0,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_gt()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_gt(
        columns = vars(a),
        value = 0.5,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_gt(
        columns = vars(a),
        value = 0.5,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_gte()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_gte(
        columns = vars(a),
        value = 1.0,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_gte(
        columns = vars(a),
        value = 1.0,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_lt()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_lt(
        columns = vars(a),
        value = 3.0,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_lt(
        columns = vars(a),
        value = 3.0,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_lte()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_lte(
        columns = vars(a),
        value = 2.5,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_lte(
        columns = vars(a),
        value = 2.5,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_between()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_between(
        columns = vars(a),
        left = 0, right = 3.0,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_between(
        columns = vars(a),
        left = 0, right = 3.0,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_not_between()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_not_between(
        columns = vars(a),
        left = 3.0, right = 4.5,
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_not_between(
        columns = vars(a),
        left = 3.0, right = 4.5,
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
  
  # Use the `col_vals_regex()` function to perform
  # simple validation steps with NAs, switching the
  # value of the `na_pass` option
  expect_warning(
    regexp = NULL,
    dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
      col_vals_regex(
        columns = vars(a),
        regex = "[0-9]-[a-z]{3}-[0-9]{3}",
        na_pass = FALSE,
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
      col_vals_regex(
        columns = vars(a),
        regex = "[0-9]-[a-z]{3}-[0-9]{3}",
        na_pass = TRUE,
        actions = warn_on_fail()
      )
  )
})

test_that("The inclusivity options work well for the range-based validations", {

  simple_tbl <- dplyr::tibble(a = 1:10)
  
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(TRUE, TRUE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(FALSE, TRUE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(TRUE, FALSE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(FALSE, FALSE),
        actions = action_levels(warn_at = 3)
      )
  )
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(FALSE, FALSE),
        actions = action_levels(warn_at = 2)
      )
  )
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = 10, right = 12,
        inclusive = c(TRUE, TRUE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = 10, right = 12,
        inclusive = c(FALSE, TRUE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = 10, right = 12,
        inclusive = c(FALSE, FALSE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = -2, right = 1,
        inclusive = c(TRUE, TRUE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = -2, right = 1,
        inclusive = c(TRUE, FALSE),
        actions = warn_on_fail()
      )
  )
  
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = -2, right = 1,
        inclusive = c(FALSE, FALSE),
        actions = warn_on_fail()
      )
  )
})
