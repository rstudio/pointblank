context("Performing simple interrogations")

tbl <- 
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc") %>%
  dplyr::mutate(g = as.factor(f))

test_that("Interrogating simply returns the expected results", {
  
  # Use the `rows_distinct()` function to perform
  # a simple validation step
  tbl_result <- tbl[1:2, ] %>% rows_distinct()
  
  # Expect that `tbl_result` is equivalent to `tbl[1:2, ]`
  expect_equivalent(tbl[1:2, ], tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% rows_distinct(actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% rows_distinct(actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_character()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_character(columns = vars(b))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_character(columns = vars(a), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_character(columns = vars(a), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_factor()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_factor(columns = vars(g))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_factor(columns = vars(a), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_factor(columns = vars(a), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_character()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_character(columns = vars(b))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_character(columns = vars(a), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_character(columns = vars(a), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_numeric()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_numeric(columns = vars(a))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_numeric(columns = vars(b), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_numeric(columns = vars(b), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_posix()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_posix(columns = vars(date_time))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_posix(columns = vars(b), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_posix(columns = vars(b), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_date()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_date(columns = vars(date))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_date(columns = vars(b), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_date(columns = vars(b), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_integer()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_integer(columns = vars(a))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_integer(columns = vars(b), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_integer(columns = vars(b), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_is_logical()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_logical(columns = vars(e))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_logical(columns = vars(b), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_logical(columns = vars(b), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_between()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_between(
      columns = vars(d),
      left = 0, right = 10000
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
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_not_between()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_between(
      columns = vars(d),
      left = 15000, right = 20000
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
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(stop_at = 1)
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
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-20"),
      actions = action_levels(warn_at = 1)
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
        preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-20"),
        actions = action_levels(warn_at = 1)
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
        preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-20"),
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_equal()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_equal(columns = vars(d), value = 283.94)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_equal(columns = vars(d), value = 283.94, actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_equal(columns = vars(d), value = 283.94, actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_not_equal()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_equal(columns = vars(d), value = 283.94)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(columns = vars(d), value = 283.94, actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(columns = vars(d), value = 283.94, actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_in_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_in_set(columns = vars(f), set = c("low", "mid", "high"))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_in_set(
        columns = vars(f),
        set = c("low", "mid", "higher"),
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_not_in_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_in_set(
      columns = vars(f),
      set = c("lower", "higher")
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
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_not_null()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_null(columns = vars(a))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(columns = vars(c), actions = action_levels(warn_at = 1))
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(columns = vars(c), actions = action_levels(stop_at = 1))
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_not_null()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_null(
      columns = vars(c),
      preconditions = ~ tbl %>% dplyr::filter(date == "2016-01-06")
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_null(
        columns = vars(c),
        preconditions = ~ tbl %>% dplyr::filter(date != "2016-01-06"),
        actions = action_levels(warn_at = 1)
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
        preconditions = ~ tbl %>% dplyr::filter(date != "2016-01-06"),
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `col_vals_regex()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_regex(
      columns = vars(b),
      regex = "[0-9]-[a-z]{3}-[0-9]{3}"
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_regex(
        columns = vars(b),
        regex = "[0-9]-dmx-[0-9]{3}",
        actions = action_levels(warn_at = 1)
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
        regex = "[0-9]-dmx-[0-9]{3}",
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Use the `conjointly()` function to perform
  # a conjoint validation validation step directly
  # on the `tbl` object
  tbl_result <- 
    tbl %>%
    conjointly(
      ~ col_vals_gt(., columns = vars(d), value = 200),
      ~ col_vals_lt(., columns = vars(c), value = 10)
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
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(stop_at = 1)
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  # Using the `col_exists()` function to perform
  # a simple validation step results in a warning
  expect_warning(tbl %>% col_exists(columns = vars(g)))
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
      col_vals_equal(
        columns = vars(a),
        value = 1.5,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.5, 1.5, 1.5, NA)) %>%
      col_vals_not_equal(
        columns = vars(a),
        value = 2.0,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_gt(
        columns = vars(a),
        value = 0.5,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_gte(
        columns = vars(a),
        value = 1.0,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_lt(
        columns = vars(a),
        value = 3.0,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_lte(
        columns = vars(a),
        value = 2.5,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_between(
        columns = vars(a),
        left = 0, right = 3.0,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c(1.0, 1.5, 2.5, NA)) %>%
      col_vals_not_between(
        columns = vars(a),
        left = 3.0, right = 4.5,
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NA,
    dplyr::tibble(a = c("1-bcd-345", "3-ldm-038", NA)) %>%
      col_vals_regex(
        columns = vars(a),
        regex = "[0-9]-[a-z]{3}-[0-9]{3}",
        na_pass = TRUE,
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(FALSE, TRUE),
        actions = action_levels(warn_at = 1)
      )
  )
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_between(
        columns = vars(a), left = 1, right = 10,
        inclusive = c(TRUE, FALSE),
        actions = action_levels(warn_at = 1)
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
        actions = action_levels(warn_at = 1)
      )
  )
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = 10, right = 12,
        inclusive = c(FALSE, TRUE),
        actions = action_levels(warn_at = 1)
      )
  )
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = 10, right = 12,
        inclusive = c(FALSE, FALSE),
        actions = action_levels(warn_at = 1)
      )
  )
  
  
  expect_warning(
    regexp = NULL,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = -2, right = 1,
        inclusive = c(TRUE, TRUE),
        actions = action_levels(warn_at = 1)
      )
  )
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = -2, right = 1,
        inclusive = c(TRUE, FALSE),
        actions = action_levels(warn_at = 1)
      )
  )
  expect_warning(
    regexp = NA,
    simple_tbl %>%
      col_vals_not_between(
        columns = vars(a), left = -2, right = 1,
        inclusive = c(FALSE, FALSE),
        actions = action_levels(warn_at = 1)
      )
  )
})
