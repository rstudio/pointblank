
tbl <- 
  small_table %>%
  dplyr::mutate(g = as.factor(f))

tbl_conjointly <-
  dplyr::tibble(
    a = c(5, 7, 6, 5, 8, 7),
    b = c(3, 4, 6, 8, 9, 11),
    c = c(2, 6, 8, NA, 3, 8)
  )

tbl_c_null <- tbl %>% dplyr::filter(is.na(c))
tbl_c_not_null <- tbl %>% dplyr::filter(!is.na(c))

test_that("pointblank expectation function produce the correct results", {

  failed_beyond <- ".*expectation failed beyond the absolute threshold level.*"
  
  #
  # expect_col_vals_lt()
  #
  expect_col_vals_lt(tbl, columns = vars(d), value = 11000)
  expect_success(expect_col_vals_lt(tbl, columns = vars(d), value = 11000))

  expect_failure(expect_col_vals_lt(tbl, columns = vars(d), value = 9900))
  expect_success(expect_col_vals_lt(tbl, columns = vars(d), value = 9900, threshold = 2))
  expect_success(expect_col_vals_lt(tbl, columns = vars(d), value = 1, threshold = 1000))
  
  expect_error(expect_col_vals_lt(tbl, columns = vars(d), value = 9900), class = "expectation_failure")
  expect_failure(expect_col_vals_lt(tbl, columns = vars(d), value = 9900), failed_beyond)
  
  #
  # expect_col_vals_lte()
  #
  
  expect_col_vals_lte(tbl, columns = vars(a), value = 8)
  expect_success(expect_col_vals_lte(tbl, columns = vars(a), value = 8))
  
  expect_failure(expect_col_vals_lte(tbl, columns = vars(a), value = 7))
  expect_success(expect_col_vals_lte(tbl, columns = vars(a), value = 7, threshold = 2))
  expect_success(expect_col_vals_lte(tbl, columns = vars(a), value = 0, threshold = 1000))
  
  expect_error(expect_col_vals_lte(tbl, columns = vars(a), value = 7), class = "expectation_failure")
  expect_failure(expect_col_vals_lte(tbl, columns = vars(a), value = 7), failed_beyond)
  
  #
  # expect_col_vals_equal()
  #
  
  tbl_equal_c_3 <- tbl %>% dplyr::filter(c == 3)
  
  expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 3)
  expect_success(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 3))
  
  expect_failure(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7))
  expect_success(expect_col_vals_equal(tbl, columns = vars(c), value = 3, threshold = 0.95))
  expect_success(expect_col_vals_equal(tbl, columns = vars(c), value = 20, threshold = 1000))
  
  expect_error(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7), class = "expectation_failure")
  expect_failure(expect_col_vals_equal(tbl_equal_c_3, columns = vars(c), value = 7), failed_beyond)
  
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
  expect_failure(expect_col_vals_gte(tbl, columns = vars(c), value = 0), failed_beyond)
  
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
  expect_failure(expect_col_vals_gt(tbl, columns = vars(c), value = 0), failed_beyond)
  
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
  expect_failure(expect_col_vals_between(tbl, columns = vars(d), left = 0, right = 500), failed_beyond)
  
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
  expect_failure(expect_col_vals_not_between(tbl, columns = vars(c), left = 20, right = 30), failed_beyond)
  
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
  expect_failure(expect_col_vals_in_set(tbl, columns = vars(e), set = TRUE), failed_beyond)
  
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
  expect_failure(expect_col_vals_not_in_set(tbl, columns = vars(b), set = tbl$b), failed_beyond)
  
  #
  # expect_col_vals_null()
  #
  
  expect_col_vals_null(tbl_c_null, columns = vars(c))
  expect_success(expect_col_vals_null(tbl_c_null, columns = vars(c)))
  expect_failure(expect_col_vals_null(tbl_c_not_null, columns = vars(c)))
  expect_success(expect_col_vals_null(tbl, columns = vars(c), threshold = 0.9))
  expect_failure(expect_col_vals_null(tbl, columns = vars(c), threshold = 0.5))
  
  expect_error(expect_col_vals_null(tbl_c_not_null, columns = vars(c)), class = "expectation_failure")
  expect_failure(expect_col_vals_null(tbl_c_not_null, columns = vars(c)), failed_beyond)
  
  #
  # expect_col_vals_not_null()
  #
  
  expect_col_vals_not_null(tbl_c_not_null, columns = vars(c))
  expect_success(expect_col_vals_not_null(tbl_c_not_null, columns = vars(c)))
  expect_failure(expect_col_vals_not_null(tbl_c_null, columns = vars(c)))
  expect_success(expect_col_vals_not_null(tbl, columns = vars(c), threshold = 0.9))
  expect_success(expect_col_vals_not_null(tbl, columns = vars(c), threshold = 1000))
  
  expect_error(expect_col_vals_not_null(tbl_c_null, columns = vars(c)), class = "expectation_failure")
  expect_failure(expect_col_vals_not_null(tbl_c_null, columns = vars(c)), failed_beyond)
  
  #
  # expect_col_vals_regex()
  #
  
  expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$")
  expect_success(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$"))
  expect_failure(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"))
  expect_success(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$", threshold = 1000))
  
  expect_error(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"), class = "expectation_failure")
  expect_failure(expect_col_vals_regex(tbl, vars(b), regex = "^[0-9]-[a-z]{4}-[0-9]{3}$"), failed_beyond)
  
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
      ~ col_vals_not_null(., vars(c))
    ), 
    failed_beyond
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
  expect_failure(expect_rows_distinct(tbl), failed_beyond)
  
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
  expect_failure(expect_col_is_character(tbl, columns = vars(g)), failed_beyond)
  
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
  expect_failure(expect_col_is_numeric(tbl, columns = vars(g)), failed_beyond)
  
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
  expect_failure(expect_col_is_integer(tbl, columns = vars(g)), failed_beyond)
  
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
  expect_failure(expect_col_is_posix(tbl, columns = vars(g)), failed_beyond)
  
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
  expect_failure(expect_col_is_logical(tbl, columns = vars(g)), failed_beyond)
  
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
  expect_failure(expect_col_is_date(tbl, columns = vars(g)), failed_beyond)
  
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
  expect_failure(expect_col_is_factor(tbl, columns = vars(f)), failed_beyond)
  
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
  expect_failure(expect_col_exists(tbl, columns = vars(h)), failed_beyond)
  
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
      )
    ), 
    failed_beyond
  )
})

test_that("expect errors to be expressed by pointblank under some conditions", {
  
  no_col_msg <- "The value for `column` doesn't correspond to a column name."
  
  # Errors caught and expressed when a column doesn't exist
  expect_error(expect_col_vals_lt(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_lte(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_equal(tbl, columns = vars(z), value = 3), regexp = no_col_msg)
  expect_error(expect_col_vals_gte(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_gt(tbl, columns = vars(z), value = 0), regexp = no_col_msg)
  expect_error(expect_col_vals_between(tbl, columns = vars(z), left = 0, right = 10000), regexp = no_col_msg)
  expect_error(expect_col_vals_not_between(tbl, columns = vars(z), left = 0, right = 10000), regexp = no_col_msg)
  expect_error(expect_col_vals_in_set(tbl, columns = vars(z), set = LETTERS), regexp = no_col_msg)
  expect_error(expect_col_vals_not_in_set(tbl, columns = vars(z), set = LETTERS), regexp = no_col_msg)
  expect_error(expect_col_vals_null(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_vals_not_null(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_vals_regex(tbl, vars(z), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$"), regexp = no_col_msg)
  
  # expect_error(
  #   expect_conjointly(
  #     tbl_conjointly,
  #     ~ col_vals_gt(., vars(x), 3),
  #     ~ col_vals_lt(., vars(y), 12)
  #   ),
  #   regexp = no_col_msg
  # )
  
  expect_error(expect_col_is_character(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_numeric(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_integer(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_posix(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_logical(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_date(tbl, columns = vars(z)), regexp = no_col_msg)
  expect_error(expect_col_is_factor(tbl, columns = vars(z)), regexp = no_col_msg)
})
