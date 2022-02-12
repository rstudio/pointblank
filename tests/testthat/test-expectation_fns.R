skip_on_cran()

tbl <- 
  small_table %>%
  dplyr::mutate(g = as.factor(f))

tbl_conjointly <-
  dplyr::tibble(
    a = c(5, 7, 6, 5, 8, 7),
    b = c(3, 4, 6, 8, 9, 11),
    c = c(2, 6, 8, NA, 3, 8)
  )

tbl_serially <-
  dplyr::tibble(
    a = c(5, 2, 6),
    b = c(6, 4, 9),
    c = c(1, 2, 3)
  )

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

tbl_equal_c_3 <- tbl %>% dplyr::filter(c == 3)
tbl_not_equal_c_3 <- tbl %>% dplyr::filter(c != 3)
tbl_c_null <- tbl %>% dplyr::filter(is.na(c))
tbl_c_not_null <- tbl %>% dplyr::filter(!is.na(c))

eval_batch_expect_fns <- function(expect_fn, tbl_test, ...) {
  
  args <- list(...)
  
  cols_ <-
    list(
      "x",
      "y",
      "z",
      c("x", "y"),
      c("y", "x"),
      c("x", "z"),
      c("x", "y", "z"),
      c("y", "z"),
      c("z", "y")
    )
  
  fail_succeed <-
    list(
      expect_failure,
      expect_success,
      expect_success,
      expect_failure,
      expect_failure,
      expect_failure,
      expect_failure,
      expect_success,
      expect_success
    )
  
  for (i in seq_along(cols_)) {
    fail_succeed[[i]](do.call(expect_fn, c(list(object = tbl_test), list(columns = cols_[[i]]), args)))
  }
}

test_that("pointblank expectation function produce the correct results", {

  failed_beyond_absolute <- ".*validation failed beyond the absolute threshold level.*"
  failed_beyond_proportional <- ".*validation failed beyond the proportional threshold level.*"
  
  #
  # expect_col_vals_lt()
  #
  expect_col_vals_lt(tbl, columns = vars(d), value = 11000)
  expect_success(expect_col_vals_lt(tbl, columns = vars(d), value = 11000))

  expect_failure(expect_col_vals_lt(tbl, columns = vars(d), value = 9900))
  expect_success(expect_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 2))
  expect_success(expect_col_vals_lt(tbl, columns = vars(d), value = 1, threshold = 1000))
  
  expect_error(expect_col_vals_lt(tbl, columns = vars(d), value = 9900), class = "expectation_failure")

  expect_failure(expect_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_lt(tbl, columns = vars(d), value = 9900),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_lt,
    tbl_test =
      tibble::tibble(
        x = c(5, 6, 10),   # failing
        y = c(2, 3, 7),    # passing
        z = c(-3, 0, 6.7)  # passing
      ),
    value = 10
  )
  
  #
  # expect_col_vals_lte()
  #
  
  expect_col_vals_lte(tbl, columns = vars(a), value = 8)
  expect_success(expect_col_vals_lte(tbl, columns = vars(a), value = 8))
  
  expect_failure(expect_col_vals_lte(tbl, columns = vars(a), value = 7))
  expect_success(expect_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 2))
  expect_success(expect_col_vals_lte(tbl, columns = vars(a), value = 0, threshold = 1000))
  
  expect_error(expect_col_vals_lte(tbl, columns = vars(a), value = 7), class = "expectation_failure")
  
  expect_failure(expect_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_lte(tbl, columns = vars(a), value = 7),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_lte,
    tbl_test =
      tibble::tibble(
        x = c(5, 6, 10.5), # failing
        y = c(2, 3, 7),    # passing
        z = c(-3, 0, 6.7)  # passing
      ),
    value = 10
  )
  
  #
  # expect_col_vals_equal()
  #
  
  expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 3)
  expect_success(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 3))
  
  expect_failure(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7))
  expect_success(expect_col_vals_equal(tbl, columns = vars(c), value = 3, threshold = 0.95))
  expect_success(expect_col_vals_equal(tbl, columns = vars(c), value = 20, threshold = 1000))
  
  expect_error(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7), class = "expectation_failure")
  
  expect_failure(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7),
    "failure level \\(3\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_equal,
    tbl_test =
      tibble::tibble(
        x = c(5, 5, 6), # failing
        y = c(5, 5, 5), # passing
        z = c(5, 5, 5)  # passing
      ),
    value = 5
  )
  
  #
  # expect_col_vals_not_equal()
  #
  
  expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 3)
  expect_success(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 3))
  
  expect_failure(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7))
  expect_success(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 3, threshold = 0.95))
  expect_success(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 20, threshold = 1000))
  
  expect_error(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7), class = "expectation_failure")
  
  expect_failure(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_not_equal,
    tbl_test =
      tibble::tibble(
        x = c(8, 5, 6), # failing
        y = c(1, 2, 4), # passing
        z = c(6, 2, 8)  # passing
      ),
    value = 5
  )
  
  #
  # expect_col_vals_gte()
  #
  
  expect_col_vals_gte(tbl, columns = vars(a), value = 0)
  expect_col_vals_gte(tbl, columns = vars(c), value = 0, na_pass = TRUE)
  expect_success(expect_col_vals_gte(tbl, columns = vars(c), value = 0, na_pass = TRUE))
  
  expect_failure(expect_col_vals_gte(tbl, columns = vars(c), value = 0))
  expect_failure(expect_col_vals_gte(tbl, columns = vars(c), value = NA))
  expect_success(expect_col_vals_gte(tbl, columns = vars(c), value = 8, na_pass = TRUE, threshold = 0.6))
  expect_success(expect_col_vals_gte(tbl, columns = vars(c), value = 0, threshold = 1000))
  
  expect_error(expect_col_vals_gte(tbl, columns = vars(c), value = 0), class = "expectation_failure")
  
  expect_failure(expect_col_vals_gte(tbl, columns = vars(c), value = 0, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_gte(tbl, columns = vars(c), value = 0, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_gte(tbl, columns = vars(c), value = 0),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_gte,
    tbl_test =
      tibble::tibble(
        x = c(4, 1.5, 8.3),  # failing
        y = c(2, 3, 7),      # passing
        z = c(3, 4, 6.7)     # passing
      ),
    value = 2
  )
  
  #
  # expect_col_vals_gt()
  #
  
  expect_col_vals_gt(tbl, columns = vars(c), value = 1, na_pass = TRUE)
  expect_col_vals_gt(tbl, columns = vars(a), value = 1, threshold = 2)
  expect_success(expect_col_vals_gt(tbl, columns = vars(a), value = 1, threshold = 2))
  
  expect_failure(expect_col_vals_gt(tbl, columns = vars(c), value = 0))
  expect_failure(expect_col_vals_gt(tbl, columns = vars(c), value = NA))
  expect_success(expect_col_vals_gt(tbl, columns = vars(c), value = 8, na_pass = TRUE, threshold = 0.8))
  expect_success(expect_col_vals_gt(tbl, columns = vars(c), value = 0, threshold = 1000))
  
  expect_error(expect_col_vals_gt(tbl, columns = vars(c), value = 0), class = "expectation_failure")
  
  expect_failure(expect_col_vals_gt(tbl, columns = vars(c), value = 0, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_gt(tbl, columns = vars(c), value = 0, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_gt(tbl, columns = vars(c), value = 0),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_gt,
    tbl_test =
      tibble::tibble(
        x = c(4, 2, 8.3),  # failing
        y = c(2.5, 3, 7),  # passing
        z = c(3, 4, 6.7)   # passing
      ),
    value = 2
  )
  
  #
  # expect_col_vals_between()
  #

  expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 10000)
  expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 2)
  expect_success(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 2))
  
  expect_failure(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 1))
  expect_failure(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 500))
  expect_failure(expect_col_vals_between(tbl, columns = vars(c), left = 0, right = 10))
  expect_success(expect_col_vals_between(tbl, columns = vars(c), left = 0, right = 10, na_pass = TRUE))
  expect_success(expect_col_vals_between(tbl, columns = vars(a), left = -3, right = -1, threshold = 1000))
  
  expect_error(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 500), class = "expectation_failure")
  
  expect_failure(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 500, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 500, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 500),
    "failure level \\(11\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_between,
    tbl_test =
      tibble::tibble(
        x = c(4, 6, 7),     # failing
        y = c(4, 5, 6),     # passing
        z = c(4.5, 5, 5.9)  # passing
      ),
    left = 4,
    right = 6
  )
  
  #
  # expect_col_vals_not_between()
  #
  
  expect_col_vals_not_between(tbl, columns = vars(d), left = -100, right = 100)
  expect_col_vals_not_between(tbl, columns = vars(a), left = 7, right = 10, threshold = 3)
  expect_success(expect_col_vals_not_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 100))
  
  expect_failure(expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30))
  expect_success(expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30, na_pass = TRUE))
  expect_failure(expect_col_vals_not_between(tbl, columns = vars(d), left = 0, right = 500))
  expect_success(expect_col_vals_not_between(tbl, columns = vars(a), left = 0, right = 1, inclusive = c(TRUE, FALSE)))
  expect_failure(expect_col_vals_not_between(tbl, columns = vars(a), left = 0, right = 1, inclusive = c(TRUE, TRUE)))
  
  expect_error(expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30), class = "expectation_failure")
  
  expect_failure(expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_not_between,
    tbl_test =
      tibble::tibble(
        x = c(3, 5, 7),  # failing
        y = c(1, 2, 3),  # passing
        z = c(7, 8, 9)   # passing
      ),
    left = 4,
    right = 6
  )
  
  #
  # expect_col_vals_in_set()
  #
  
  expect_col_vals_in_set(tbl, columns = vars(c), set = tbl$c %>% unique())
  expect_col_vals_in_set(tbl, columns = vars(c), set = tbl$c)
  expect_col_vals_in_set(tbl, columns = vars(c), set = c(tbl$c, 20, 30))
  expect_success(expect_col_vals_in_set(tbl, columns = vars(b), set = tbl$b))
  expect_success(expect_col_vals_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  
  expect_failure(expect_col_vals_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA)))
  expect_failure(expect_col_vals_in_set(tbl, columns = vars(e), set = TRUE))
  
  expect_error(expect_col_vals_in_set(tbl, columns = vars(e), set = TRUE), class = "expectation_failure")
  
  expect_failure(expect_col_vals_in_set(tbl, columns = vars(e), set = TRUE, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_in_set(tbl, columns = vars(e), set = TRUE, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_in_set(tbl, columns = vars(e), set = TRUE),
    "failure level \\(5\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_in_set,
    tbl_test =
      tibble::tibble(
        x = c(4, 4, 7),  # failing
        y = c(4, 4, 5),  # passing
        z = c(6, 5, 4)   # passing
      ),
    set = c(4, 5, 6)
  )
  
  #
  # expect_col_vals_not_in_set()
  #
  
  expect_col_vals_not_in_set(tbl, columns = vars(a), set = tbl$d)
  expect_col_vals_not_in_set(tbl, columns = vars(d), set = tbl$a)
  expect_col_vals_not_in_set(tbl, columns = vars(c), set = c(20, 30))
  
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b))
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA)))
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(e), set = TRUE))
  
  expect_error(expect_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b), class = "expectation_failure")
  
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b),
    "failure level \\(13\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_not_in_set,
    tbl_test =
      tibble::tibble(
        x = c(1, 2, 6),  # failing
        y = c(1, 2, 3),  # passing
        z = c(7, 8, 9)   # passing
      ),
    set = c(4, 5, 6)
  )
  
  #
  # expect_col_vals_make_set()
  #
  
  expect_col_vals_make_set(tbl, columns = vars(c), set = tbl$c %>% unique())
  expect_col_vals_make_set(tbl, columns = vars(c), set = tbl$c)
  expect_success(expect_col_vals_make_set(tbl, columns = vars(b), set = tbl$b))
  expect_success(expect_col_vals_make_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  
  expect_failure(expect_col_vals_make_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA)))
  expect_failure(expect_col_vals_make_set(tbl, columns = vars(e), set = TRUE))
  
  expect_error(expect_col_vals_make_set(tbl, columns = vars(e), set = TRUE), class = "expectation_failure")
  
  expect_failure(expect_col_vals_make_set(tbl, columns = vars(e), set = TRUE, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_make_set(tbl, columns = vars(e), set = TRUE, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_make_set(tbl, columns = vars(e), set = TRUE),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_make_set,
    tbl_test =
      tibble::tibble(
        x = c(1, 2, 2),  # failing
        y = c(1, 2, 3),  # passing
        z = c(3, 2, 1)   # passing
      ),
    set = c(1, 2, 3)
  )
  
  #
  # expect_col_vals_make_subset()
  #
  
  expect_col_vals_make_subset(tbl, columns = vars(c), set = tbl$c %>% unique())
  expect_col_vals_make_subset(tbl, columns = vars(c), set = c(3, 8, 7))
  expect_col_vals_make_subset(tbl, columns = vars(c), set = c(3, 8, 7, NA))
  expect_success(expect_col_vals_make_subset(tbl, columns = vars(b), set = tbl$b))
  expect_success(expect_col_vals_make_subset(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  
  expect_failure(expect_col_vals_make_subset(tbl, columns = vars(c), set = c(2, 3, 4, 7, 25, NA)))
  expect_failure(expect_col_vals_make_subset(tbl, columns = vars(e), set = 99))
  
  expect_error(expect_col_vals_make_subset(tbl, columns = vars(e), set = ""), class = "expectation_failure")
  
  expect_failure(expect_col_vals_make_subset(tbl, columns = vars(e), set = "", threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_make_subset(tbl, columns = vars(e), set = "", threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_make_subset(tbl, columns = vars(e), set = ""),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_make_subset,
    tbl_test =
      tibble::tibble(
        x = c(1, 3, 3),  # failing
        y = c(1, 2, 3),  # passing
        z = c(3, 2, 1)   # passing
      ),
    set = c(1, 2)
  )
  
  #
  # expect_col_vals_increasing()
  #
  
  expect_col_vals_increasing(increasing_tbl, vars(a))
  expect_col_vals_increasing(increasing_tbl, vars(b), na_pass = TRUE)
  expect_col_vals_increasing(increasing_tbl, vars(c), allow_stationary = TRUE)
  expect_col_vals_increasing(increasing_tbl, vars(d), decreasing_tol = 0.001)
  expect_col_vals_increasing(increasing_tbl, vars(d), decreasing_tol = 0.0001)
  expect_col_vals_increasing(increasing_tbl, vars(d), allow_stationary = TRUE, decreasing_tol = 0.001)
  
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(d), allow_stationary = TRUE, decreasing_tol = 0.00001))
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(b)))
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(c)))
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(d), allow_stationary = TRUE))
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(d), decreasing_tol = 0.00001))
  
  expect_error(expect_col_vals_increasing(increasing_tbl, vars(b)), class = "expectation_failure")
  
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(b), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_increasing(increasing_tbl, vars(b), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_increasing(increasing_tbl, vars(b)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_increasing,
    tbl_test =
      tibble::tibble(
        x = c(2, 3, 2), # failing
        y = c(1, 3, 9), # passing
        z = c(6, 7, 8)  # passing
      )
  )
  
  #
  # expect_col_vals_decreasing()
  #
  
  expect_col_vals_decreasing(decreasing_tbl, vars(a))
  expect_col_vals_decreasing(decreasing_tbl, vars(b), na_pass = TRUE)
  expect_col_vals_decreasing(decreasing_tbl, vars(c), allow_stationary = TRUE)
  expect_col_vals_decreasing(decreasing_tbl, vars(d), increasing_tol = 0.001)
  expect_col_vals_decreasing(decreasing_tbl, vars(d), increasing_tol = 0.0001)
  expect_col_vals_decreasing(decreasing_tbl, vars(d), allow_stationary = TRUE, increasing_tol = 0.001)
  
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(d), allow_stationary = TRUE, increasing_tol = 0.00001))
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(b)))
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(c)))
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(d), allow_stationary = TRUE))
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(d), increasing_tol = 0.00001))
  
  expect_error(expect_col_vals_decreasing(decreasing_tbl, vars(b)), class = "expectation_failure")
  
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(b), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_decreasing(decreasing_tbl, vars(b), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_decreasing(decreasing_tbl, vars(b)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_decreasing,
    tbl_test =
      tibble::tibble(
        x = c(1, 2, 3), # failing
        y = c(9, 3, 1), # passing
        z = c(8, 7, 6)  # passing
      )
  )
  
  #
  # expect_col_vals_null()
  #
  
  expect_col_vals_null(tbl_c_null, columns = vars(c))
  expect_success(expect_col_vals_null(tbl_c_null, columns = vars(c)))
  expect_failure(expect_col_vals_null(tbl_c_not_null, columns = vars(c)))
  expect_success(expect_col_vals_null(tbl, columns = vars(c), threshold = 0.9))
  expect_failure(expect_col_vals_null(tbl, columns = vars(c), threshold = 0.5))
  
  expect_error(expect_col_vals_null(tbl_c_not_null, columns = vars(c)), class = "expectation_failure")
  
  expect_failure(expect_col_vals_null(tbl_c_not_null, columns = vars(c), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_null(tbl_c_not_null, columns = vars(c), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_null(tbl_c_not_null, columns = vars(c)),
    "failure level \\(11\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_null,
    tbl_test =
      tibble::tibble(
        x = c(1, NA, NA),  # failing
        y = c(NA, NA, NA), # passing
        z = c(NA, NA, NA)  # passing
      )
  )
  
  #
  # expect_col_vals_not_null()
  #
  
  expect_col_vals_not_null(tbl_c_not_null, columns = vars(c))
  expect_success(expect_col_vals_not_null(tbl_c_not_null, columns = vars(c)))
  expect_failure(expect_col_vals_not_null(tbl_c_null, columns = vars(c)))
  expect_success(expect_col_vals_not_null(tbl, columns = vars(c), threshold = 0.9))
  expect_success(expect_col_vals_not_null(tbl, columns = vars(c), threshold = 1000))
  
  expect_error(expect_col_vals_not_null(tbl_c_null, columns = vars(c)), class = "expectation_failure")
  
  expect_failure(expect_col_vals_not_null(tbl_c_null, columns = vars(c), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_not_null(tbl_c_null, columns = vars(c), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_not_null(tbl_c_null, columns = vars(c)),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_not_null,
    tbl_test =
      tibble::tibble(
        x = c(1, 2, NA), # failing
        y = c(4, 5, 6),  # passing
        z = c(7, 8, 9)   # passing
      )
  )
  
  #
  # expect_col_vals_regex()
  #
  
  expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$")
  expect_success(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$"))
  expect_failure(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"))
  expect_success(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 1000))
  
  expect_error(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"), class = "expectation_failure")
  
  expect_failure(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"),
    "failure level \\(13\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_regex,
    tbl_test =
      tibble::tibble(
        x = c("bean", "bane", "wean"), # failing
        y = c("sea", "eat", "tea"), # passing
        z = c("unchallangeable", "levelheadedness", "reauthorization")  # passing
      ),
    regex = "ea"
  )
  
  #
  # expect_col_vals_within_spec()
  #
  
  expect_col_vals_within_spec(specifications[1:5, ], vars(zip_codes), spec = "zip")
  expect_success(expect_col_vals_within_spec(specifications[1:5, ], vars(zip_codes), spec = "zip"))
  expect_failure(expect_col_vals_within_spec(specifications[1:6, ], vars(zip_codes), spec = "zip"))
  expect_success(expect_col_vals_within_spec(specifications, vars(zip_codes), spec = "zip", threshold = 1000))
  
  expect_error(expect_col_vals_within_spec(specifications, vars(zip_codes), spec = "isbn"), class = "expectation_failure")
  
  expect_failure(expect_col_vals_within_spec(specifications[1:6, ], vars(zip_codes), spec = "zip", threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_within_spec(specifications[1:6, ], vars(zip_codes), spec = "zip", threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_within_spec(specifications, vars(zip_codes), spec = "zip"),
    "failure level \\(3\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_vals_within_spec,
    tbl_test =
      tibble::tibble(
        x = c("test@test.com", "great@test.com", "no!@test.com"), # failing
        y = c("mail+mail@example.com", "atest@test.net", "hi@e.test.com"), # passing
        z = c("01234567890@numbers-in-local.net", "mail.email@test.com", "good@test.com")  # passing
      ),
    spec = "email"
  )
  
  #
  # expect_col_vals_expr()
  #
  
  expect_col_vals_expr(tbl, ~ a %% 1 == 0)
  expect_col_vals_expr(tbl, ~ c %% 1 == 0)
  expect_col_vals_expr(tbl, expr(a %% 1 == 0))
  expect_col_vals_expr(tbl, expr(c %% 1 == 0))
  expect_col_vals_expr(tbl, ~ case_when(
    b == 1 ~ a > 5 & c >= 1
  ))
  expect_col_vals_expr(tbl, expr(
    case_when(
      b == 1 ~ a > 5 & c >= 1
    )
  ))
  
  expect_success(expect_col_vals_expr(tbl, ~ a %% 1 == 0))
  expect_success(expect_col_vals_expr(tbl, ~ a %>% between(0, 10)))
  expect_failure(expect_col_vals_expr(tbl, ~ a < 5))
  expect_success(expect_col_vals_expr(tbl, expr(a %% 1 == 0)))
  expect_success(expect_col_vals_expr(tbl, expr(a %>% between(0, 10))))
  
  expect_failure(expect_col_vals_expr(tbl, ~ a < 5))
  expect_failure(expect_col_vals_expr(tbl, expr(a < 5)))
  
  expect_error(expect_col_vals_expr(tbl, expr(a < 5)), class = "expectation_failure")
  
  expect_failure(expect_col_vals_expr(tbl, expr(a < 5), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_vals_expr(tbl, expr(a < 5), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_vals_expr(tbl, expr(a < 5)),
    "failure level \\(3\\) >= failure threshold \\(1\\)"
  )
  
  #
  # expect_conjointly()
  #
  
  expect_conjointly(
    tbl_conjointly,
    ~ col_vals_gt(., vars(a), 3),
    ~ col_vals_lt(., vars(b), 12),
    ~ col_vals_not_null(., vars(b))
  )
  
  expect_success(
    expect_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c)),
      threshold = 5
    )
  )
  
  expect_failure(
    expect_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c))
    )
  )
  
  expect_error(
    expect_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c))
    ), 
    class = "expectation_failure"
  )
  
  expect_failure(
    expect_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c)),
      threshold = 1
    ), 
    failed_beyond_absolute
  )
  
  expect_failure(
    expect_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c)),
      threshold = 0.01
    ), 
    failed_beyond_proportional
  )
  
  expect_failure(
    expect_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c))
    ), 
    "failure level \\(4\\) >= failure threshold \\(1\\)"
  )
  
  #
  # expect_serially()
  #
  
  expect_success(
    expect_serially(
      tbl_serially,
      ~ test_col_is_numeric(., vars(a, b)),    # PASS
      ~ test_col_vals_not_null(., vars(a, b)), # PASS
      ~ col_vals_gt(., vars(b), vars(a))       # PASS
    )
  )
  
  expect_success(
    expect_serially(
      tbl_serially,
      ~ test_col_is_numeric(., vars(a, b)),    # PASS
      ~ test_col_vals_not_null(., vars(a, b)), # PASS
      ~ col_vals_gt(., vars(b), vars(a)),      # PASS
      threshold = 5
    )
  )
  
  expect_success(
    expect_serially(
      tbl_serially,
      ~ test_col_is_numeric(., vars(a, b)),    # PASS
      ~ test_col_vals_not_null(., vars(a, b)), # PASS
      ~ col_vals_gt(., vars(c), 1),            # PASS 2/3
      threshold = 2
    )
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_numeric(., vars(a, b)),    # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)), # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),            # PASS 2/3
      threshold = 1
    )
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b)),  # FAIL, would FAIL
      ~ test_col_vals_not_null(., vars(a, b)), # would PASS, PASS
      ~ col_vals_gt(., vars(b), vars(a)),      # would PASS
    )
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_numeric(., vars(a, b)),      # PASS, PASS
      ~ test_col_vals_increasing(., vars(c, b)), # PASS, FAIL
      ~ col_vals_gt(., vars(b), vars(a)),        # would PASS
    )
  )
  
  expect_success(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(b), vars(a))                      # PASS
    )
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),                           # FAIL
      threshold = 1
    )
  )
  
  expect_success(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),                           # PASS
      threshold = 2
    )
  )
  
  expect_error(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),                           # FAIL
    ), 
    class = "expectation_failure"
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),                           # FAIL
      threshold = 1
    ), 
    failed_beyond_absolute
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),                           # FAIL
      threshold = 0.01
    ), 
    failed_beyond_proportional
  )
  
  expect_failure(
    expect_serially(
      tbl_serially,
      ~ test_col_is_character(., vars(a, b), threshold = 2),  # PASS, PASS
      ~ test_col_vals_not_null(., vars(a, b)),                # PASS, PASS
      ~ col_vals_gt(., vars(c), 1),                           # FAIL
    ), 
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  #
  # expect_rows_distinct()
  #
  
  expect_rows_distinct(tbl %>% dplyr::distinct())
  expect_rows_distinct(tbl, columns = vars(date_time, date), threshold = 0.2)
  expect_success(expect_rows_distinct(tbl %>% dplyr::select(d) %>% dplyr::slice(5)))
  
  expect_failure(expect_rows_distinct(tbl))
  expect_failure(expect_rows_distinct(tbl, columns = vars(date_time, date)))
  
  expect_error(expect_rows_distinct(tbl), class = "expectation_failure")
  
  expect_failure(expect_rows_distinct(tbl, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_rows_distinct(tbl, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_rows_distinct(tbl),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  #
  # expect_rows_complete()
  #
  
  expect_rows_complete(tbl_complete_yes)
  expect_rows_complete(tbl_complete_no, columns = vars(b, c), threshold = 0.5)
  expect_success(expect_rows_complete(tbl_complete_no %>% dplyr::select(b, c) %>% dplyr::slice(1:2)))
  
  expect_failure(expect_rows_complete(tbl_complete_no))
  expect_failure(expect_rows_complete(tbl_complete_no, columns = vars(a, b)))
  
  expect_error(expect_rows_complete(tbl_complete_no), class = "expectation_failure")
  
  expect_failure(expect_rows_complete(tbl_complete_no, threshold = 1), failed_beyond_absolute)
  expect_failure(expect_rows_complete(tbl_complete_no, threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_rows_complete(tbl_complete_no),
    "failure level \\(2\\) >= failure threshold \\(1\\)"
  )
  
  #
  # expect_col_is_character()
  #
  
  expect_col_is_character(tbl, columns = vars(b))
  expect_col_is_character(tbl, columns = vars(f))
  
  expect_failure(expect_col_is_character(tbl, columns = vars(date_time)))
  expect_failure(expect_col_is_character(tbl, columns = vars(date)))
  expect_failure(expect_col_is_character(tbl, columns = vars(a)))
  expect_failure(expect_col_is_character(tbl, columns = vars(c)))
  expect_failure(expect_col_is_character(tbl, columns = vars(d)))
  expect_failure(expect_col_is_character(tbl, columns = vars(e)))
  expect_failure(expect_col_is_character(tbl, columns = vars(g)))
  expect_success(expect_col_is_character(tbl, columns = vars(g), threshold = 2))
  
  expect_error(expect_col_is_character(tbl, columns = vars(g)), class = "expectation_failure")
  
  expect_failure(expect_col_is_character(tbl, columns = vars(g), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_character(tbl, columns = vars(g), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_character(tbl, columns = vars(g)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_character,
    tbl_test =
      tibble::tibble(
        x = c(1, 2, 3),               # failing
        y = c("1", "2", "3"),         # passing
        z = c("one", "two", "three")  # passing
      )
  )
  
  #
  # expect_col_is_numeric()
  #
  
  expect_col_is_numeric(tbl, columns = vars(c))
  expect_col_is_numeric(tbl, columns = vars(d))
  
  expect_failure(expect_col_is_numeric(tbl, columns = vars(date_time)))
  expect_failure(expect_col_is_numeric(tbl, columns = vars(date)))
  expect_failure(expect_col_is_numeric(tbl, columns = vars(a)))
  expect_failure(expect_col_is_numeric(tbl, columns = vars(b)))
  expect_failure(expect_col_is_numeric(tbl, columns = vars(e)))
  expect_failure(expect_col_is_numeric(tbl, columns = vars(f)))
  expect_failure(expect_col_is_numeric(tbl, columns = vars(g)))
  expect_success(expect_col_is_numeric(tbl, columns = vars(g), threshold = 2))
  
  expect_error(expect_col_is_numeric(tbl, columns = vars(g)), class = "expectation_failure")
  
  expect_failure(expect_col_is_numeric(tbl, columns = vars(g), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_numeric(tbl, columns = vars(g), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_numeric(tbl, columns = vars(g)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_numeric,
    tbl_test =
      tibble::tibble(
        x = c("1", "2", "3"),  # failing
        y = c(1.2, 2.3, 3.4),  # passing
        z = c(1.1, 2.2, 3.3)   # passing
      )
  )
  
  #
  # expect_col_is_integer()
  #
  
  expect_col_is_integer(tbl, columns = vars(a))
  
  expect_failure(expect_col_is_integer(tbl, columns = vars(date_time)))
  expect_failure(expect_col_is_integer(tbl, columns = vars(date)))
  expect_failure(expect_col_is_integer(tbl, columns = vars(b)))
  expect_failure(expect_col_is_integer(tbl, columns = vars(d)))
  expect_failure(expect_col_is_integer(tbl, columns = vars(e)))
  expect_failure(expect_col_is_integer(tbl, columns = vars(f)))
  expect_failure(expect_col_is_integer(tbl, columns = vars(g)))
  expect_success(expect_col_is_integer(tbl, columns = vars(g), threshold = 2))
  
  expect_error(expect_col_is_integer(tbl, columns = vars(g)), class = "expectation_failure")
  
  expect_failure(expect_col_is_integer(tbl, columns = vars(g), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_integer(tbl, columns = vars(g), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_integer(tbl, columns = vars(g)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_integer,
    tbl_test =
      tibble::tibble(
        x = c(1.1, 2.2, 3.3),  # failing
        y = c(1L, 2L, 3L),     # passing
        z = c(4L, 5L, 6L)      # passing
      )
  )
  
  #
  # expect_col_is_logical()
  #
  
  expect_col_is_logical(tbl, columns = vars(e))
  
  expect_failure(expect_col_is_logical(tbl, columns = vars(date_time)))
  expect_failure(expect_col_is_logical(tbl, columns = vars(date)))
  expect_failure(expect_col_is_logical(tbl, columns = vars(a)))
  expect_failure(expect_col_is_logical(tbl, columns = vars(b)))
  expect_failure(expect_col_is_logical(tbl, columns = vars(d)))
  expect_failure(expect_col_is_logical(tbl, columns = vars(f)))
  expect_failure(expect_col_is_logical(tbl, columns = vars(g)))
  expect_success(expect_col_is_logical(tbl, columns = vars(g), threshold = 2))
  
  expect_error(expect_col_is_logical(tbl, columns = vars(g)), class = "expectation_failure")
  
  expect_failure(expect_col_is_logical(tbl, columns = vars(g), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_logical(tbl, columns = vars(g), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_logical(tbl, columns = vars(g)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_logical,
    tbl_test =
      tibble::tibble(
        x = c(1, 0, 1),            # failing
        y = c(TRUE, FALSE, TRUE),  # passing
        z = c(TRUE, NA, FALSE)     # passing
      )
  )
  
  #
  # expect_col_is_date()
  #
  
  expect_col_is_date(tbl, columns = vars(date))
  
  expect_failure(expect_col_is_date(tbl, columns = vars(date_time)))
  expect_failure(expect_col_is_date(tbl, columns = vars(a)))
  expect_failure(expect_col_is_date(tbl, columns = vars(b)))
  expect_failure(expect_col_is_date(tbl, columns = vars(d)))
  expect_failure(expect_col_is_date(tbl, columns = vars(e)))
  expect_failure(expect_col_is_date(tbl, columns = vars(f)))
  expect_failure(expect_col_is_date(tbl, columns = vars(g)))
  expect_success(expect_col_is_date(tbl, columns = vars(g), threshold = 2))
  
  expect_error(expect_col_is_date(tbl, columns = vars(g)), class = "expectation_failure")
  
  expect_failure(expect_col_is_date(tbl, columns = vars(g), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_date(tbl, columns = vars(g), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_date(tbl, columns = vars(g)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_date,
    tbl_test =
      tibble::tibble(
        x = tbl$date_time[1:3],  # failing
        y = tbl$date[1:3],       # passing
        z = tbl$date[4:6]        # passing
      )
  )
  
  #
  # expect_col_is_posix()
  #
  
  expect_col_is_posix(tbl, columns = vars(date_time))
  
  expect_failure(expect_col_is_posix(tbl, columns = vars(date)))
  expect_failure(expect_col_is_posix(tbl, columns = vars(a)))
  expect_failure(expect_col_is_posix(tbl, columns = vars(b)))
  expect_failure(expect_col_is_posix(tbl, columns = vars(d)))
  expect_failure(expect_col_is_posix(tbl, columns = vars(e)))
  expect_failure(expect_col_is_posix(tbl, columns = vars(f)))
  expect_failure(expect_col_is_posix(tbl, columns = vars(g)))
  expect_success(expect_col_is_posix(tbl, columns = vars(g), threshold = 2))
  
  expect_error(expect_col_is_posix(tbl, columns = vars(g)), class = "expectation_failure")
  
  expect_failure(expect_col_is_posix(tbl, columns = vars(g), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_posix(tbl, columns = vars(g), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_posix(tbl, columns = vars(g)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_posix,
    tbl_test =
      tibble::tibble(
        x = tbl$date[1:3],       # failing
        y = tbl$date_time[1:3],  # passing
        z = tbl$date_time[4:6]   # passing
      )
  )
  
  #
  # expect_col_is_factor()
  #
  
  expect_col_is_factor(tbl, columns = vars(g))
  
  expect_failure(expect_col_is_factor(tbl, columns = vars(date_time)))
  expect_failure(expect_col_is_factor(tbl, columns = vars(date)))
  expect_failure(expect_col_is_factor(tbl, columns = vars(a)))
  expect_failure(expect_col_is_factor(tbl, columns = vars(b)))
  expect_failure(expect_col_is_factor(tbl, columns = vars(d)))
  expect_failure(expect_col_is_factor(tbl, columns = vars(e)))
  expect_failure(expect_col_is_factor(tbl, columns = vars(f)))
  expect_success(expect_col_is_factor(tbl, columns = vars(date), threshold = 2))
  
  expect_error(expect_col_is_factor(tbl, columns = vars(f)), class = "expectation_failure")
  
  expect_failure(expect_col_is_factor(tbl, columns = vars(f), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_is_factor(tbl, columns = vars(f), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_is_factor(tbl, columns = vars(f)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_is_factor,
    tbl_test =
      tibble::tibble(
        x = tbl$f[1:3],  # failing
        y = tbl$g[1:3],  # passing
        z = tbl$g[4:6]   # passing
      )
  )
  
  #
  # expect_col_exists()
  #
  
  expect_col_exists(tbl, columns = vars(date_time))
  expect_col_exists(tbl, columns = vars(date))
  expect_col_exists(tbl, columns = vars(a))
  expect_col_exists(tbl, columns = vars(b))
  expect_col_exists(tbl, columns = vars(d))
  expect_col_exists(tbl, columns = vars(e))
  expect_col_exists(tbl, columns = vars(f))
  expect_col_exists(tbl, columns = vars(g))
  expect_col_exists(tbl, columns = "g")
  
  expect_failure(expect_col_exists(tbl, columns = vars(h)))
  expect_failure(expect_col_exists(tbl, columns = "h"))
  
  expect_error(expect_col_exists(tbl, columns = vars(h)), class = "expectation_failure")
  
  expect_failure(expect_col_exists(tbl, columns = vars(h), threshold = 1), failed_beyond_absolute)
  expect_failure(expect_col_exists(tbl, columns = vars(h), threshold = 0.01), failed_beyond_proportional)
  
  expect_failure(
    expect_col_exists(tbl, columns = vars(h)),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  eval_batch_expect_fns(
    expect_fn = expect_col_exists,
    tbl_test =
      tibble::tibble(
        a = c("1", "2", "3"),  # failing
        y = c(1.2, 2.3, 3.4),  # passing
        z = c(1.1, 2.2, 3.3)   # passing
      )
  )
  
  #
  # expect_col_schema_match()
  #
  
  expect_col_schema_match(tbl, schema = col_schema(.tbl = tbl))
  
  expect_col_schema_match(
    tbl, 
    schema = col_schema(
      date_time = c("POSIXct", "POSIXt"),
      date = "Date",
      a = "integer",
      b = "character",
      c = "numeric",
      d = "numeric",
      e = "logical",
      f = "character",
      g = "factor"
    )
  )
  
  expect_failure(
    expect_col_schema_match(
      tbl, 
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character",
        g = "factor"
      )
    )
  )
  
  expect_error(
    expect_col_schema_match(
      tbl, 
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character",
        g = "factor"
      )
    ), 
    class = "expectation_failure"
  )
  
  expect_failure(
    expect_col_schema_match(
      tbl, 
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character",
        g = "factor"
      ),
      threshold = 1
    ), 
    failed_beyond_absolute
  )
  
  expect_failure(
    expect_col_schema_match(
      tbl, 
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character",
        g = "factor"
      ),
      threshold = 0.01
    ), 
    failed_beyond_proportional
  )
  
  expect_failure(
    expect_col_schema_match(
      tbl, 
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character",
        g = "factor"
      )
    ), 
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  #
  # expect_row_count_match()
  #
  
  expect_row_count_match(tbl, count = tbl)
  expect_row_count_match(tbl, count = pointblank::small_table)
  expect_row_count_match(tbl, count = ~ pointblank::small_table)
  expect_row_count_match(tbl, count = function() pointblank::small_table)
  
  expect_failure(
    expect_row_count_match(tbl, count = tbl_conjointly),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  expect_error(
    expect_row_count_match(tbl, count = tbl_conjointly),
    class = "expectation_failure"
  )
  
  expect_row_count_match(tbl_conjointly, count = tbl_complete_yes)
  expect_row_count_match(tbl_complete_yes, count = tbl_conjointly)
  
  expect_row_count_match(tbl_complete_no, count = tbl_complete_yes)
  expect_row_count_match(tbl_complete_yes, count = tbl_complete_no)
  
  expect_row_count_match(increasing_tbl, count = decreasing_tbl)
  expect_row_count_match(decreasing_tbl, count = increasing_tbl)
  
  
  #
  # expect_tbl_match()
  #
  
  expect_tbl_match(tbl, tbl_compare = tbl)
  expect_tbl_match(pointblank::small_table, tbl_compare = pointblank::small_table)
  expect_tbl_match(pointblank::small_table, tbl_compare = ~ pointblank::small_table)
  expect_tbl_match(pointblank::small_table, tbl_compare = function() pointblank::small_table)
  
  expect_failure(
    expect_tbl_match(tbl, tbl_compare = tbl_conjointly),
    "failure level \\(1\\) >= failure threshold \\(1\\)"
  )
  
  expect_error(
    expect_tbl_match(tbl, tbl_compare = tbl_conjointly),
    class = "expectation_failure"
  )
  
  expect_tbl_match(tbl, tbl_compare = tbl)
  expect_tbl_match(pointblank::small_table, tbl_compare = pointblank::small_table)
  expect_tbl_match(pointblank::small_table, tbl_compare = ~ pointblank::small_table)
  expect_tbl_match(pointblank::small_table, tbl_compare = function() pointblank::small_table)
  expect_tbl_match(tbl_complete_no, tbl_compare = tbl_complete_no)
  expect_tbl_match(tbl_complete_yes, tbl_compare = tbl_complete_yes)
  expect_tbl_match(tbl_conjointly, tbl_compare = tbl_conjointly)
  expect_tbl_match(increasing_tbl, tbl_compare = increasing_tbl)
  expect_tbl_match(decreasing_tbl, tbl_compare = decreasing_tbl)
  
  expect_tbl_match(gt::countrypops, tbl_compare = gt::countrypops)
  expect_tbl_match(gt::sza, tbl_compare = gt::sza)
  expect_tbl_match(gt::gtcars, tbl_compare = gt::gtcars)
  expect_tbl_match(gt::sp500, tbl_compare = gt::sp500)
  expect_tbl_match(gt::pizzaplace, tbl_compare = gt::pizzaplace)
  expect_tbl_match(gt::exibble, tbl_compare = gt::exibble)
  expect_tbl_match(ggplot2::diamonds, tbl_compare = ggplot2::diamonds)
  expect_tbl_match(ggplot2::economics_long, tbl_compare = ggplot2::economics_long)
  expect_tbl_match(ggplot2::faithfuld, tbl_compare = ggplot2::faithfuld)
  expect_tbl_match(ggplot2::luv_colours, tbl_compare = ggplot2::luv_colours)
  expect_tbl_match(ggplot2::midwest, tbl_compare = ggplot2::midwest)
  expect_tbl_match(ggplot2::mpg, tbl_compare = ggplot2::mpg)
  expect_tbl_match(ggplot2::msleep, tbl_compare = ggplot2::msleep)
  expect_tbl_match(ggplot2::presidential, tbl_compare = ggplot2::presidential)
  expect_tbl_match(ggplot2::seals, tbl_compare = ggplot2::seals)
  expect_tbl_match(ggplot2::txhousing, tbl_compare = ggplot2::txhousing)
  expect_tbl_match(dplyr::band_instruments, tbl_compare = dplyr::band_instruments)
  expect_tbl_match(dplyr::band_members, tbl_compare = dplyr::band_members)
  expect_tbl_match(dplyr::starwars, tbl_compare = dplyr::starwars)
  expect_tbl_match(dplyr::storms, tbl_compare = dplyr::storms)
  expect_tbl_match(tidyr::billboard, tbl_compare = tidyr::billboard)
  expect_tbl_match(tidyr::construction, tbl_compare = tidyr::construction)
  expect_tbl_match(tidyr::fish_encounters, tbl_compare = tidyr::fish_encounters)
  expect_tbl_match(tidyr::population, tbl_compare = tidyr::population)
  expect_tbl_match(tidyr::relig_income, tbl_compare = tidyr::relig_income)
  expect_tbl_match(tidyr::smiths, tbl_compare = tidyr::smiths)
  expect_tbl_match(tidyr::us_rent_income, tbl_compare = tidyr::us_rent_income)
  expect_tbl_match(tidyr::who, tbl_compare = tidyr::who)
  expect_tbl_match(tidyr::world_bank_pop, tbl_compare = tidyr::world_bank_pop)
  expect_tbl_match(lubridate::lakers, tbl_compare = lubridate::lakers)
  expect_tbl_match(datasets::airquality, tbl_compare = datasets::airquality)
  expect_tbl_match(datasets::chickwts, tbl_compare = datasets::chickwts)
  expect_tbl_match(datasets::iris, tbl_compare = datasets::iris)
  expect_tbl_match(datasets::LifeCycleSavings, tbl_compare = datasets::LifeCycleSavings)
  expect_tbl_match(datasets::longley, tbl_compare = datasets::longley)
  expect_tbl_match(datasets::morley, tbl_compare = datasets::morley)
  expect_tbl_match(datasets::mtcars, tbl_compare = datasets::mtcars)
  expect_tbl_match(datasets::Orange, tbl_compare = datasets::Orange)
  expect_tbl_match(datasets::pressure, tbl_compare = datasets::pressure)
  expect_tbl_match(datasets::quakes, tbl_compare = datasets::quakes)
  expect_tbl_match(datasets::rock, tbl_compare = datasets::rock)
  expect_tbl_match(datasets::swiss, tbl_compare = datasets::swiss)
  expect_tbl_match(datasets::USJudgeRatings, tbl_compare = datasets::USJudgeRatings)
  
  expect_error(expect_tbl_match(tbl, tbl_compare = tbl %>% dplyr::slice_head(n = 12)), class = "expectation_failure")
  expect_error(expect_tbl_match(tbl, tbl_compare = tbl %>% dplyr::rename(datetime = date_time)), class = "expectation_failure")
  expect_error(expect_tbl_match(tbl, tbl_compare = tbl %>% dplyr::select(a, dplyr::everything())), class = "expectation_failure")
  
  expect_tbl_match(tbl, tbl_compare = tbl %>% dplyr::group_by(e, f))
  expect_tbl_match(tbl %>% dplyr::group_by(e, g), tbl_compare = tbl %>% dplyr::group_by(e, f))
  expect_tbl_match(tbl %>% dplyr::group_by(e, g), tbl_compare = tbl)
})

test_that("expect errors to be expressed by pointblank under some conditions", {
  
  no_col_msg <- "The value for `column` doesn't correspond to a column name."
  
  # Errors caught and expressed when a column doesn't exist
  expect_error(expect_col_vals_lt(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_lte(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_equal(tbl, columns = vars(z), value = 3), regexp = no_col_msg)
  expect_error(expect_col_vals_not_equal(tbl, columns = vars(z), value = 3), regexp = no_col_msg)
  expect_error(expect_col_vals_gte(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_gt(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_between(tbl, columns = vars(z), left = 0, right = 10000), regexp = no_col_msg)
  expect_error(expect_col_vals_not_between(tbl, columns = vars(z), left = 0, right = 10000), regexp = no_col_msg)
  expect_error(expect_col_vals_in_set(tbl, columns = vars(z), set = LETTERS), regexp = no_col_msg)
  expect_error(expect_col_vals_not_in_set(tbl, columns = vars(z), set = LETTERS), regexp = no_col_msg)
  expect_error(expect_col_vals_null(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_vals_not_null(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_vals_regex(tbl, vars(z), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$"), regexp = no_col_msg)
  expect_error(expect_col_vals_within_spec(tbl, vars(z), spec = "isbn"), regexp = no_col_msg)
  expect_error(expect_col_is_character(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_numeric(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_integer(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_posix(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_logical(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_date(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_factor(tbl, columns = vars(z)), regexp = no_col_msg)
})
