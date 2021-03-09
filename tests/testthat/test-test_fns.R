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

test_that("pointblank expectation functions produce the correct results", {

  #
  # test_col_vals_lt()
  #
  
  expect_true(test_col_vals_lt(tbl, columns = vars(d), value = 11000))
  expect_false(test_col_vals_lt(tbl, columns = vars(d), value = 9900))
  expect_true(test_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 2))
  expect_true(test_col_vals_lt(tbl, columns = vars(d), value = 1, threshold = 1000))
  expect_false(test_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 1))
  expect_false(test_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 0.01))
  
  #
  # test_col_vals_lte()
  #
  
  expect_true(test_col_vals_lte(tbl, columns = vars(a), value = 8))
  expect_false(test_col_vals_lte(tbl, columns = vars(a), value = 7))
  expect_true(test_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 2))
  expect_true(test_col_vals_lte(tbl, columns = vars(a), value = 0, threshold = 1000))
  expect_false(test_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 1))
  expect_false(test_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 0.01))
  
  #
  # test_col_vals_equal()
  #
  
  expect_true(test_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 3))
  expect_false(test_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7))
  expect_true(test_col_vals_equal(tbl, columns = vars(c), value = 3, threshold = 0.95))
  expect_true(test_col_vals_equal(tbl, columns = vars(c), value = 20, threshold = 1000))
  expect_false(test_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7, threshold = 1))
  expect_false(test_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7, threshold = 0.01))
  
  #
  # test_col_vals_not_equal()
  #
  
  expect_true(test_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 3))
  expect_false(test_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7))
  expect_true(test_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 3, threshold = 0.95))
  expect_true(test_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 20, threshold = 1000))
  expect_false(test_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7, threshold = 1))
  expect_false(test_col_vals_not_equal(tbl_not_equal_c_3, columns = vars(c), value = 7, threshold = 0.01))
  
  #
  # test_col_vals_gte()
  #
  
  expect_true(test_col_vals_gte(tbl, columns = vars(c), value = 0, na_pass = TRUE))
  expect_false(test_col_vals_gte(tbl, columns = vars(c), value = 0))
  expect_false(test_col_vals_gte(tbl, columns = vars(c), value = NA))
  expect_true(test_col_vals_gte(tbl, columns = vars(c), value = 8, na_pass = TRUE, threshold = 0.6))
  expect_true(test_col_vals_gte(tbl, columns = vars(c), value = 0, threshold = 1000))
  expect_false(test_col_vals_gte(tbl, columns = vars(c), value = 0, threshold = 1))
  expect_false(test_col_vals_gte(tbl, columns = vars(c), value = 0, threshold = 0.01))

  #
  # test_col_vals_gt()
  #
  
  expect_true(test_col_vals_gt(tbl, columns = vars(a), value = 1, threshold = 2))
  expect_false(test_col_vals_gt(tbl, columns = vars(c), value = 0))
  expect_false(test_col_vals_gt(tbl, columns = vars(c), value = NA))
  expect_true(test_col_vals_gt(tbl, columns = vars(c), value = 8, na_pass = TRUE, threshold = 0.8))
  expect_true(test_col_vals_gt(tbl, columns = vars(c), value = 0, threshold = 1000))
  expect_false(test_col_vals_gt(tbl, columns = vars(c), value = 0, threshold = 1))
  expect_false(test_col_vals_gt(tbl, columns = vars(c), value = 0, threshold = 0.01))

  #
  # test_col_vals_between()
  #
  
  expect_true(test_col_vals_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 2))
  expect_false(test_col_vals_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 1))
  expect_false(test_col_vals_between(tbl, columns = vars(d), left = 0, right = 500))
  expect_false(test_col_vals_between(tbl, columns = vars(c), left = 0, right = 10))
  expect_true(test_col_vals_between(tbl, columns = vars(c), left = 0, right = 10, na_pass = TRUE))
  expect_true(test_col_vals_between(tbl, columns = vars(a), left = -3, right = -1, threshold = 1000))
  expect_false(test_col_vals_between(tbl, columns = vars(d), left = 0, right = 500, threshold = 1))
  expect_false(test_col_vals_between(tbl, columns = vars(d), left = 0, right = 500, threshold = 0.01))

  #
  # test_col_vals_not_between()
  #
  
  expect_true(test_col_vals_not_between(tbl, columns = vars(d), left = 0, right = 9000, threshold = 100))
  expect_false(test_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30))
  expect_true(test_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30, na_pass = TRUE))
  expect_false(test_col_vals_not_between(tbl, columns = vars(d), left = 0, right = 500))
  expect_true(test_col_vals_not_between(tbl, columns = vars(a), left = 0, right = 1, inclusive = c(TRUE, FALSE)))
  expect_false(test_col_vals_not_between(tbl, columns = vars(a), left = 0, right = 1, inclusive = c(TRUE, TRUE)))
  expect_false(test_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30, threshold = 1))
  expect_false(test_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30, threshold = 0.01))

  #
  # test_col_vals_in_set()
  #
  
  expect_true(test_col_vals_in_set(tbl, columns = vars(b), set = tbl$b))
  expect_true(test_col_vals_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  expect_false(test_col_vals_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA)))
  expect_false(test_col_vals_in_set(tbl, columns = vars(e), set = TRUE))
  expect_false(test_col_vals_in_set(tbl, columns = vars(e), set = TRUE, threshold = 1))
  expect_false(test_col_vals_in_set(tbl, columns = vars(e), set = TRUE, threshold = 0.01))

  #
  # test_col_vals_not_in_set()
  #
  
  expect_false(test_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b))
  expect_false(test_col_vals_not_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  expect_false(test_col_vals_not_in_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA)))
  expect_false(test_col_vals_not_in_set(tbl, columns = vars(e), set = TRUE))
  expect_false(test_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b, threshold = 1))
  expect_false(test_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b, threshold = 0.01))

  #
  # expect_col_vals_make_set()
  #
  
  expect_true(test_col_vals_make_set(tbl, columns = vars(c), set = tbl$c %>% unique()))
  expect_true(test_col_vals_make_set(tbl, columns = vars(c), set = tbl$c))
  expect_true(test_col_vals_make_set(tbl, columns = vars(b), set = tbl$b))
  expect_true(test_col_vals_make_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  expect_false(test_col_vals_make_set(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA)))
  expect_false(test_col_vals_make_set(tbl, columns = vars(e), set = TRUE))
  
  
  #
  # expect_col_vals_make_subset()
  #
  
  expect_true(test_col_vals_make_subset(tbl, columns = vars(c), set = tbl$c %>% unique()))
  expect_true(test_col_vals_make_subset(tbl, columns = vars(c), set = tbl$c))
  expect_true(test_col_vals_make_subset(tbl, columns = vars(b), set = tbl$b))
  expect_true(test_col_vals_make_subset(tbl, columns = vars(c), set = c(3, 8)))
  expect_true(test_col_vals_make_subset(tbl, columns = vars(c), set = c(2, 3, 4, 7, 9, NA), threshold = 3))
  expect_false(test_col_vals_make_subset(tbl, columns = vars(c), set = c(99, 2, 3, 4, 7, 9, NA)))
  expect_false(test_col_vals_make_subset(tbl, columns = vars(e), set = ""))
  
  #
  # expect_col_vals_increasing()
  #
  
  expect_true(test_col_vals_increasing(increasing_tbl, vars(a)))
  expect_true(test_col_vals_increasing(increasing_tbl, vars(b), na_pass = TRUE))
  expect_true(test_col_vals_increasing(increasing_tbl, vars(c), allow_stationary = TRUE))
  expect_true(test_col_vals_increasing(increasing_tbl, vars(d), decreasing_tol = 0.001))
  expect_true(test_col_vals_increasing(increasing_tbl, vars(d), decreasing_tol = 0.0001))
  expect_true(test_col_vals_increasing(increasing_tbl, vars(d), allow_stationary = TRUE, decreasing_tol = 0.001))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(d), allow_stationary = TRUE, decreasing_tol = 0.00001))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(b)))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(c)))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(d), allow_stationary = TRUE))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(d), decreasing_tol = 0.00001))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(b), threshold = 1))
  expect_false(test_col_vals_increasing(increasing_tbl, vars(b), threshold = 0.01))
  
  #
  # expect_col_vals_decreasing()
  #
  
  expect_true(test_col_vals_decreasing(decreasing_tbl, vars(a)))
  expect_true(test_col_vals_decreasing(decreasing_tbl, vars(b), na_pass = TRUE))
  expect_true(test_col_vals_decreasing(decreasing_tbl, vars(c), allow_stationary = TRUE))
  expect_true(test_col_vals_decreasing(decreasing_tbl, vars(d), increasing_tol = 0.001))
  expect_true(test_col_vals_decreasing(decreasing_tbl, vars(d), increasing_tol = 0.0001))
  expect_true(test_col_vals_decreasing(decreasing_tbl, vars(d), allow_stationary = TRUE, increasing_tol = 0.001))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(d), allow_stationary = TRUE, increasing_tol = 0.00001))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(b)))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(c)))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(d), allow_stationary = TRUE))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(d), increasing_tol = 0.00001))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(b), threshold = 1))
  expect_false(test_col_vals_decreasing(decreasing_tbl, vars(b), threshold = 0.01))
  
  #
  # test_col_vals_null()
  #
  
  expect_true(test_col_vals_null(tbl_c_null, columns = vars(c)))
  expect_false(test_col_vals_null(tbl_c_not_null, columns = vars(c)))
  expect_true(test_col_vals_null(tbl, columns = vars(c), threshold = 0.9))
  expect_false(test_col_vals_null(tbl, columns = vars(c), threshold = 0.5))
  expect_false(test_col_vals_null(tbl_c_not_null, columns = vars(c), threshold = 1))
  expect_false(test_col_vals_null(tbl_c_not_null, columns = vars(c), threshold = 0.01))

  #
  # test_col_vals_not_null()
  #
  
  expect_true(test_col_vals_not_null(tbl_c_not_null, columns = vars(c)))
  expect_false(test_col_vals_not_null(tbl_c_null, columns = vars(c)))
  expect_true(test_col_vals_not_null(tbl, columns = vars(c), threshold = 0.9))
  expect_true(test_col_vals_not_null(tbl, columns = vars(c), threshold = 1000))
  expect_false(test_col_vals_not_null(tbl_c_null, columns = vars(c), threshold = 1))
  expect_false(test_col_vals_not_null(tbl_c_null, columns = vars(c), threshold = 0.01))
  
  #
  # test_col_vals_regex()
  #
  
  expect_true(test_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$"))
  expect_false(test_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"))
  expect_true(test_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 1000))
  expect_false(test_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 1))
  expect_false(test_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 0.01))
  
  #
  # test_col_vals_expr()
  #
  
  expect_true(test_col_vals_expr(tbl, ~ a %% 1 == 0))
  expect_true(test_col_vals_expr(tbl, ~ c %% 1 == 0))
  expect_true(test_col_vals_expr(tbl, expr(a %% 1 == 0)))
  expect_true(test_col_vals_expr(tbl, expr(c %% 1 == 0)))
              
  expect_true(
    test_col_vals_expr(tbl, ~ case_when(
      b == 1 ~ a > 5 & c >= 1
    ))
  )
  expect_true(
    test_col_vals_expr(tbl, expr(
      case_when(
        b == 1 ~ a > 5 & c >= 1
      )
    ))
  )
  
  expect_true(test_col_vals_expr(tbl, ~ a < 0, threshold = 1000))
  expect_false(test_col_vals_expr(tbl, expr(a < 5), threshold = 1))
  expect_false(test_col_vals_expr(tbl, expr(a < 5), threshold = 0.01))
  expect_false(test_col_vals_expr(tbl, expr(between(a, 5, 10)), threshold = 0.01))

  #
  # test_conjointly()
  #
  
  expect_true(
    test_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c)),
      threshold = 5
    )
  )
  
  expect_false(
    test_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c))
    )
  )
  
  expect_false(
    test_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c)),
      threshold = 1
    ), 
    failed_beyond_absolute
  )
  
  expect_false(
    test_conjointly(
      tbl_conjointly,
      ~ col_vals_gt(., vars(a), 6),
      ~ col_vals_lt(., vars(b), 10),
      ~ col_vals_not_null(., vars(c)),
      threshold = 0.01
    ), 
    failed_beyond_proportional
  )
  
  #
  # test_rows_distinct()
  #
  
  expect_true(test_rows_distinct(tbl %>% dplyr::select(d) %>% dplyr::slice(5)))
  expect_false(test_rows_distinct(tbl))
  expect_false(test_rows_distinct(tbl, columns = vars(date_time, date)))
  expect_false(test_rows_distinct(tbl, threshold = 1))
  expect_false(test_rows_distinct(tbl, threshold = 0.01))

  #
  # test_col_is_character()
  #
  
  expect_false(test_col_is_character(tbl, columns = vars(date_time)))
  expect_false(test_col_is_character(tbl, columns = vars(date)))
  expect_false(test_col_is_character(tbl, columns = vars(a)))
  expect_false(test_col_is_character(tbl, columns = vars(c)))
  expect_false(test_col_is_character(tbl, columns = vars(d)))
  expect_false(test_col_is_character(tbl, columns = vars(e)))
  expect_false(test_col_is_character(tbl, columns = vars(g)))
  expect_true(test_col_is_character(tbl, columns = vars(g), threshold = 2))
  expect_false(test_col_is_character(tbl, columns = vars(g), threshold = 1))
  expect_false(test_col_is_character(tbl, columns = vars(g), threshold = 0.01))

  #
  # test_col_is_numeric()
  #
  
  expect_false(test_col_is_numeric(tbl, columns = vars(date_time)))
  expect_false(test_col_is_numeric(tbl, columns = vars(date)))
  expect_false(test_col_is_numeric(tbl, columns = vars(a)))
  expect_false(test_col_is_numeric(tbl, columns = vars(b)))
  expect_false(test_col_is_numeric(tbl, columns = vars(e)))
  expect_false(test_col_is_numeric(tbl, columns = vars(f)))
  expect_false(test_col_is_numeric(tbl, columns = vars(g)))
  expect_true(test_col_is_numeric(tbl, columns = vars(g), threshold = 2))
  expect_false(test_col_is_numeric(tbl, columns = vars(g), threshold = 1))
  expect_false(test_col_is_numeric(tbl, columns = vars(g), threshold = 0.01))

  #
  # test_col_is_integer()
  #

  expect_false(test_col_is_integer(tbl, columns = vars(date_time)))
  expect_false(test_col_is_integer(tbl, columns = vars(date)))
  expect_false(test_col_is_integer(tbl, columns = vars(b)))
  expect_false(test_col_is_integer(tbl, columns = vars(d)))
  expect_false(test_col_is_integer(tbl, columns = vars(e)))
  expect_false(test_col_is_integer(tbl, columns = vars(f)))
  expect_false(test_col_is_integer(tbl, columns = vars(g)))
  expect_true(test_col_is_integer(tbl, columns = vars(g), threshold = 2))
  expect_false(test_col_is_integer(tbl, columns = vars(g), threshold = 1))
  expect_false(test_col_is_integer(tbl, columns = vars(g), threshold = 0.01))

  #
  # test_col_is_posix()
  #

  expect_false(test_col_is_posix(tbl, columns = vars(date)))
  expect_false(test_col_is_posix(tbl, columns = vars(a)))
  expect_false(test_col_is_posix(tbl, columns = vars(b)))
  expect_false(test_col_is_posix(tbl, columns = vars(d)))
  expect_false(test_col_is_posix(tbl, columns = vars(e)))
  expect_false(test_col_is_posix(tbl, columns = vars(f)))
  expect_false(test_col_is_posix(tbl, columns = vars(g)))
  expect_true(test_col_is_posix(tbl, columns = vars(g), threshold = 2))
  expect_false(test_col_is_posix(tbl, columns = vars(g), threshold = 1))
  expect_false(test_col_is_posix(tbl, columns = vars(g), threshold = 0.01))

  #
  # test_col_is_logical()
  #
  
  expect_false(test_col_is_logical(tbl, columns = vars(date_time)))
  expect_false(test_col_is_logical(tbl, columns = vars(date)))
  expect_false(test_col_is_logical(tbl, columns = vars(a)))
  expect_false(test_col_is_logical(tbl, columns = vars(b)))
  expect_false(test_col_is_logical(tbl, columns = vars(d)))
  expect_false(test_col_is_logical(tbl, columns = vars(f)))
  expect_false(test_col_is_logical(tbl, columns = vars(g)))
  expect_true(test_col_is_logical(tbl, columns = vars(g), threshold = 2))
  expect_false(test_col_is_logical(tbl, columns = vars(g), threshold = 1))
  expect_false(test_col_is_logical(tbl, columns = vars(g), threshold = 0.01))

  #
  # test_col_is_date()
  #

  expect_false(test_col_is_date(tbl, columns = vars(date_time)))
  expect_false(test_col_is_date(tbl, columns = vars(a)))
  expect_false(test_col_is_date(tbl, columns = vars(b)))
  expect_false(test_col_is_date(tbl, columns = vars(d)))
  expect_false(test_col_is_date(tbl, columns = vars(e)))
  expect_false(test_col_is_date(tbl, columns = vars(f)))
  expect_false(test_col_is_date(tbl, columns = vars(g)))
  expect_true(test_col_is_date(tbl, columns = vars(g), threshold = 2))
  expect_false(test_col_is_date(tbl, columns = vars(g), threshold = 1))
  expect_false(test_col_is_date(tbl, columns = vars(g), threshold = 0.01))

  #
  # test_col_is_factor()
  #
  
  expect_false(test_col_is_factor(tbl, columns = vars(date_time)))
  expect_false(test_col_is_factor(tbl, columns = vars(date)))
  expect_false(test_col_is_factor(tbl, columns = vars(a)))
  expect_false(test_col_is_factor(tbl, columns = vars(b)))
  expect_false(test_col_is_factor(tbl, columns = vars(d)))
  expect_false(test_col_is_factor(tbl, columns = vars(e)))
  expect_false(test_col_is_factor(tbl, columns = vars(f)))
  expect_true(test_col_is_factor(tbl, columns = vars(date), threshold = 2))
  expect_false(test_col_is_factor(tbl, columns = vars(f), threshold = 1))
  expect_false(test_col_is_factor(tbl, columns = vars(f), threshold = 0.01))

  #
  # test_col_exists()
  #
  
  expect_true(test_col_exists(tbl, columns = vars(date_time)))
  expect_true(test_col_exists(tbl, columns = vars(date)))
  expect_true(test_col_exists(tbl, columns = vars(a)))
  expect_true(test_col_exists(tbl, columns = vars(b)))
  expect_true(test_col_exists(tbl, columns = vars(d)))
  expect_true(test_col_exists(tbl, columns = vars(e)))
  expect_true(test_col_exists(tbl, columns = vars(f)))
  expect_true(test_col_exists(tbl, columns = vars(g)))
  expect_true(test_col_exists(tbl, columns = "g"))
  expect_false(test_col_exists(tbl, columns = vars(h)))
  expect_false(test_col_exists(tbl, columns = "h"))
  expect_false(test_col_exists(tbl, columns = vars(h), threshold = 1))
  expect_false(test_col_exists(tbl, columns = vars(h), threshold = 0.01))
  
  #
  # test_col_schema_match()
  #
  
  expect_true(
    test_col_schema_match(
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
  )
  
  expect_false(
    test_col_schema_match(
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
  
  expect_false(
    test_col_schema_match(
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
  
  expect_false(
    test_col_schema_match(
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
    )
  )
})

test_that("expect errors to be expressed by pointblank under some conditions", {
  
  no_col_msg <- "The value for `column` doesn't correspond to a column name."
  
  # Errors caught and expressed when a column doesn't exist
  expect_error(test_col_vals_lt(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(test_col_vals_lte(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(test_col_vals_equal(tbl, columns = vars(z), value = 3), regexp = no_col_msg)
  expect_error(test_col_vals_not_equal(tbl, columns = vars(z), value = 3), regexp = no_col_msg)
  expect_error(test_col_vals_gte(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(test_col_vals_gt(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(test_col_vals_between(tbl, columns = vars(z), left = 0, right = 10000), regexp = no_col_msg)
  expect_error(test_col_vals_not_between(tbl, columns = vars(z), left = 0, right = 10000), regexp = no_col_msg)
  expect_error(test_col_vals_in_set(tbl, columns = vars(z), set = LETTERS), regexp = no_col_msg)
  expect_error(test_col_vals_not_in_set(tbl, columns = vars(z), set = LETTERS), regexp = no_col_msg)
  expect_error(test_col_vals_null(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_vals_not_null(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_vals_regex(tbl, vars(z), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$"), regexp = no_col_msg)
  expect_error(test_col_is_character(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_is_numeric(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_is_integer(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_is_posix(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_is_logical(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_is_date(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(test_col_is_factor(tbl, columns = vars(z)), regexp = no_col_msg)
})
