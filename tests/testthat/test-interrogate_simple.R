context("Performing simple interrogations")

tbl <- 
  system.file(
    "extdata", "small_table.csv",
    package = "pointblank"
  ) %>%
  readr::read_csv(col_types = "TDicidlc")

test_that("Interrogating simply returns the expected results", {
  
  # Use the `col_is_character()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_character(column = b)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_character(column = a, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_character(column = a, stop_count = 1)
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_is_numeric()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_numeric(column = a)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_numeric(column = b, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_numeric(column = b, stop_count = 1)
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_is_posix()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_posix(column = date_time)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_posix(column = b, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_posix(column = b, stop_count = 1)
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_is_date()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_date(column = date)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_date(column = b, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_date(column = b, stop_count = 1)
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_is_integer()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_integer(column = a)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_integer(column = b, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_integer(column = b, stop_count = 1)
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_is_logical()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_logical(column = e)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_logical(column = b, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_logical(column = b, stop_count = 1)
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_between()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_between(
      column = d,
      left = 0,
      right = 10000
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_between(
        column = d,
        left = 0,
        right = 1000,
        warn_count = 1
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
        column = d,
        left = 0,
        right = 1000,
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_not_between()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_between(
      column = d,
      left = 15000,
      right = 20000
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_between(
        column = d,
        left = 9000,
        right = 11000,
        warn_count = 1
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
        column = d,
        left = 9000,
        right = 11000,
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_not_between()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_not_between(
      column = d,
      left = 3000,
      right = 10000,
      preconditions = date > "2016-01-20",
      warn_count = 1
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_between(
        column = d,
        left = 0,
        right = 3000,
        preconditions = date > "2016-01-20",
        warn_count = 1
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
        column = d,
        left = 0,
        right = 3000,
        preconditions = date > "2016-01-20",
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_equal()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_equal(column = d, value = 283.94)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_equal(
        column = d,
        value = 283.94,
        warn_count = 1
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
        column = d,
        value = 283.94,
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_not_equal()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_equal(column = d, value = 283.94)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(
        column = d,
        value = 283.94,
        warn_count = 1
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
        column = d,
        value = 283.94,
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_in_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_in_set(
      column = f,
      set = c("low", "mid", "high")
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_in_set(
        column = f,
        set = c("low", "mid", "higher"),
        warn_count = 1
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
        column = f,
        set = c("low", "mid", "higher"),
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_not_in_set()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_in_set(
      column = f,
      set = c("lower", "higher")
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_in_set(
        column = f,
        set = c("lower", "mid", "higher"),
        warn_count = 1
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
        column = f,
        set = c("lower", "mid", "higher"),
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_not_null()` function to perform
  # a simple validation step
  tbl_result <- 
    tbl %>%
    col_vals_not_null(column = a)
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(
        column = c,
        warn_count = 1
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
        column = c,
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_not_null()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_null(
      column = c,
      preconditions = date == "2016-01-06"
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_null(
        column = c,
        preconditions = date != "2016-01-06",
        warn_count = 1
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
        column = c,
        preconditions = date != "2016-01-06",
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
  
  
  
  # Use the `col_vals_regex()` function to perform
  # a simple validation step (with a precondition)
  tbl_result <- 
    tbl %>%
    col_vals_regex(
      column = b,
      regex = "[0-9]-[a-z]{3}-[0-9]{3}"
    )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_regex(
        column = b,
        regex = "[0-9]-dmx-[0-9]{3}",
        warn_count = 1
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
        column = b,
        regex = "[0-9]-dmx-[0-9]{3}",
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
})
