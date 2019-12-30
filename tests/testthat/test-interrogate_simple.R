context("Performing simple interrogations")

tbl <- 
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

test_that("Interrogating simply returns the expected results", {
  
  # Use the `col_is_character()` function to perform
  # a simple validation step
  tbl_result <- tbl %>% col_is_character(columns = vars(b))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- tbl %>% col_is_character(columns = a, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_character(columns = vars(a), stop_count = 1)
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
    tbl_result <- tbl %>% col_is_numeric(columns = vars(b), warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_numeric(columns = vars(b), stop_count = 1)
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
    tbl_result <- tbl %>% col_is_posix(columns = vars(b), warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_posix(columns = vars(b), stop_count = 1)
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
    tbl_result <- tbl %>% col_is_date(columns = vars(b), warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_date(columns = vars(b), stop_count = 1)
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
    tbl_result <- tbl %>% col_is_integer(columns = vars(b), warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_integer(columns = vars(b), stop_count = 1)
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
    tbl_result <- tbl %>% col_is_logical(columns = vars(b), warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- tbl %>% col_is_logical(columns = vars(b), stop_count = 1)
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
        columns = vars(d),
        left = 0, right = 1000,
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
        columns = vars(d),
        left = 9000, right = 11000,
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
      columns = vars(d),
      left = 3000, right = 10000,
      preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-20"),
      warn_count = 1
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
        columns = vars(d),
        left = 0, right = 3000,
        preconditions = ~ tbl %>% dplyr::filter(date > "2016-01-20"),
        stop_count = 1
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
      col_vals_equal(columns = vars(d), value = 283.94, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_equal(columns = vars(d), value = 283.94, stop_count = 1)
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
      col_vals_not_equal(columns = vars(d), value = 283.94, warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_equal(columns = vars(d), value = 283.94, stop_count = 1)
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
        columns = vars(f),
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
        columns = vars(f),
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
    col_vals_not_null(columns = vars(a))
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  # Perform a simple validation that yields a warning
  expect_warning(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(columns = vars(c), warn_count = 1)
  )
  
  # Expect that `tbl_result` is equivalent to `tbl`
  expect_equivalent(tbl, tbl_result)
  
  rm(tbl_result)
  
  # Perform a simple validation step that results in stopping
  expect_error(
    tbl_result <- 
      tbl %>%
      col_vals_not_null(columns = vars(c), stop_count = 1)
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
        columns = vars(c),
        preconditions = ~ tbl %>% dplyr::filter(date != "2016-01-06"),
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
        columns = vars(b),
        regex = "[0-9]-dmx-[0-9]{3}",
        stop_count = 1
      )
  )
  
  # Expect that `tbl_result` is never created
  expect_false(exists("tbl_result"))
})
