test_that("Agent interrogations with segments yields the correct results", {
  
  #
  # col_vals_increasing
  #

  increasing_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_inc_1", 5), rep("group_inc_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(1:5, 5:14, 5:1, 1:10)
    )
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation_1 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      segments = a ~ c("group_inc_1", "group_inc_2")) %>%
    interrogate()
  
  # Expect certain values in `validation_1$validation_set`
  expect_equal(
    validation_1$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  # Targeted skipping of this test for {covr}
  if (isFALSE(as.logical(Sys.getenv("R_COVR", "false")))) {
    expect_equal(validation_1$validation_set$column %>% unlist(), rep("b", 2))
  }
  expect_equal(
    validation_1$validation_set$seg_expr[[1]],
    validation_1$validation_set$seg_expr[[2]]
  )
  expect_equal(validation_1$validation_set$seg_col, rep("a", 2))
  expect_equal(validation_1$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equal(validation_1$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation_1$validation_set$n, c(5, 10))
  expect_equal(nrow(validation_1$validation_set), 2)
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by two grouping values from a single column; the
  # variation here is that the segments expression uses `vars(a)` instead
  # of `a` on the LHS
  validation_2 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      segments = vars(a) ~ c("group_inc_1", "group_inc_2")) %>%
    interrogate()
  
  # Expect certain values in `validation_2$validation_set`
  expect_equal(
    validation_2$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equal(validation_2$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation_2$validation_set$seg_expr[[1]],
    validation_2$validation_set$seg_expr[[2]]
  )
  expect_equal(validation_2$validation_set$seg_col, rep("a", 2))
  expect_equal(validation_2$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equal(validation_2$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation_2$validation_set$n, c(5, 10))
  expect_equal(nrow(validation_2$validation_set), 2)
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by *all* grouping values from a single column (`a`)
  validation_3 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b), segments = vars(a)) %>%
    interrogate()
  
  # Expect certain values in `validation_3$validation_set`
  expect_equal(
    validation_3$validation_set$assertion_type, rep("col_vals_increasing", 4)
  )
  expect_equal(validation_3$validation_set$column %>% unlist(), rep("b", 4))
  expect_equal(
    validation_3$validation_set$seg_expr[[1]],
    validation_3$validation_set$seg_expr[[2]],
    validation_3$validation_set$seg_expr[[3]],
    validation_3$validation_set$seg_expr[[4]],
  )
  expect_equal(validation_3$validation_set$seg_col, rep("a", 4))
  expect_equal(
    validation_3$validation_set$seg_val,
    c("group_inc_1", "group_inc_2", "group_not", NA_character_)
  )
  expect_equal(validation_3$validation_set$all_passed, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(validation_3$validation_set$n, c(5, 10, 5, 30))
  expect_equal(nrow(validation_3$validation_set), 4)
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by *all* grouping values from a single column (`a`),
  # but, use preconditions first to trim the table to only contain the
  # "group_inc_1" and "group_inc_2" rows; the result is that only two validation
  # steps are effectively generated from this call of `col_vals_increasing()`
  validation_4 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      preconditions = ~ . %>% dplyr::slice_head(n = 15),
      segments = vars(a)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation_4$validation_set`
  expect_equal(
    validation_4$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equal(validation_4$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation_4$validation_set$seg_expr[[1]],
    validation_4$validation_set$seg_expr[[2]]
  )
  expect_equal(validation_4$validation_set$seg_col, rep("a", 2))
  expect_equal(validation_4$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equal(validation_4$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation_4$validation_set$n, c(5, 10))
  expect_equal(nrow(validation_4$validation_set), 2)

  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by two grouping values from a single column; this
  # splits up the segments input into a list of two formula expressions
  validation_5 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      segments = list(a ~ "group_inc_1", a ~ "group_inc_2")
    )%>%
    interrogate()
  
  # Expect certain values in `validation_5$validation_set`
  expect_equal(
    validation_5$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equal(validation_5$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation_5$validation_set$seg_expr[[1]],
    validation_5$validation_set$seg_expr[[2]]
  )
  expect_equal(validation_5$validation_set$seg_col, rep("a", 2))
  expect_equal(validation_5$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equal(validation_5$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation_5$validation_set$n, c(5, 10))
  expect_equal(nrow(validation_5$validation_set), 2)
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by two grouping values from a single column: one
  # that exists and one that does not
  # TODO: ensure that the second validation step is 'inactive'
  validation_6 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      segments = vars(a) ~ c("group_inc_1", "group_missing")
    ) %>%
    interrogate()
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by a single grouping value that does not exist
  # TODO: ensure that the one and only validation step is 'inactive'
  validation_7 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      segments = vars(a) ~ "group_missing"
    ) %>%
    interrogate()
  
  #
  # col_vals_decreasing
  #
  
  decreasing_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_dec_1", 5), rep("group_dec_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(5:1, 14:5, 1:5, 10:1)
    )
  
  # Use the `col_vals_decreasing()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = decreasing_tbl) %>%
    col_vals_decreasing(
      columns = vars(b),
      segments = a ~ c("group_dec_1", "group_dec_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_decreasing", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_dec_1", "group_dec_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_lt
  #
  
  comparison_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 5), rep("group_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(rep(4, 5), rep(5, 10), rep(10, 5), rep(12, 10))
    )
  
  # Use the `col_vals_lt()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = comparison_tbl) %>%
    col_vals_lt(
      columns = vars(b), value = 5,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_lt", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, FALSE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_lte
  #

  comparison_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 5), rep("group_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(rep(4, 5), rep(5, 10), rep(10, 5), rep(12, 10))
    )
  
  # Use the `col_vals_lte()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = comparison_tbl) %>%
    col_vals_lte(
      columns = vars(b), value = 5,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_lte", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_equal
  #
  
  comparison_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 5), rep("group_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(rep(4, 5), rep(5, 10), rep(10, 5), rep(12, 10))
    )
  
  # Use the `col_vals_equal()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = comparison_tbl) %>%
    col_vals_equal(
      columns = vars(b), value = 5,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_equal", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(FALSE, TRUE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_not_equal
  #
  
  comparison_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 5), rep("group_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(rep(4, 5), rep(5, 10), rep(10, 5), rep(12, 10))
    )
  
  # Use the `col_vals_not_equal()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = comparison_tbl) %>%
    col_vals_not_equal(
      columns = vars(b), value = 5,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_not_equal", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, FALSE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_gte
  #
    
  comparison_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 5), rep("group_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(rep(4, 5), rep(5, 10), rep(10, 5), rep(12, 10))
    )
  
  # Use the `col_vals_gte()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = comparison_tbl) %>%
    col_vals_gte(
      columns = vars(b), value = 4,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_gte", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_gt
  #
  
  comparison_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 5), rep("group_2", 10),
        rep("group_not", 5), rep(NA_character_, 10)
      ),
      b = c(rep(4, 5), rep(5, 10), rep(10, 5), rep(12, 10))
    )
  
  # Use the `col_vals_gt()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = comparison_tbl) %>%
    col_vals_gt(
      columns = vars(b), value = 4,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_gt", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(FALSE, TRUE))
  expect_equal(validation$validation_set$n, c(5, 10))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_between
  #
  
  between_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 4), rep("group_2", 4), rep(NA_character_, 4)
      ),
      b = c(3:6, 2:5, rep(10, 4))
    )
  
  # Use the `col_vals_between()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = between_tbl) %>%
    col_vals_between(
      columns = vars(b), left = 2, right = 6,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_between", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(4, 4))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_not_between
  #
  
  # Use the `col_vals_not_between()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = between_tbl) %>%
    col_vals_not_between(
      columns = vars(b), left = 2, right = 6,
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_not_between", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(FALSE, FALSE))
  expect_equal(validation$validation_set$n, c(4, 4))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_in_set
  #
  
  set_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 2)
      ),
      b = c(c("a", "b"), c("b", "c"), c("f", "g"))
    )
  
  # Use the `col_vals_in_set()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = set_tbl) %>%
    col_vals_in_set(
      columns = vars(b), set = c("a", "b", "c", "d"),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_in_set", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_not_in_set
  #
  
  # Use the `col_vals_not_in_set()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = set_tbl) %>%
    col_vals_not_in_set(
      columns = vars(b), set = c("f", "g"),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_not_in_set", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_make_set
  #
  
  make_set_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 3), rep(NA_character_, 2)
      ),
      b = c(c("a", "b"), c("a", "b", "c"), c("f", "g"))
    )
  
  # Use the `col_vals_make_set()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = make_set_tbl) %>%
    col_vals_make_set(
      columns = vars(b), set = c("a", "b", "c"),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_make_set", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(FALSE, TRUE))
  expect_equal(validation$validation_set$n, c(4, 4))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_make_subset
  #
  
  make_subset_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 3), rep(NA_character_, 2)
      ),
      b = c(c("a", "b"), c("a", "b", "c"), c("f", "g"))
    )
  
  # Use the `col_vals_make_subset()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = make_subset_tbl) %>%
    col_vals_make_subset(
      columns = vars(b), set = "a",
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_make_subset", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(1, 1))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_null
  #
  
  na_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 4)
      ),
      b = c(rep(NA, 4), rep(10, 4))
    )
  
  # Use the `col_vals_null()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = na_tbl) %>%
    col_vals_null(
      columns = vars(b),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_null", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_not_null
  #
  
  some_na_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 4)
      ),
      b = c(rep(NA, 2), 1:2, rep(10, 4))
    )
  
  # Use the `col_vals_not_null()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = some_na_tbl) %>%
    col_vals_not_null(
      columns = vars(b),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_not_null", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(FALSE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_within_spec
  #
  
  spec_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 4)
      ),
      b = c(specifications$vin_numbers[1:4], rep("2B7J!21Y0XK524320", 4))
    )
  
  # Use the `col_vals_within_spec()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = spec_tbl) %>%
    col_vals_within_spec(
      columns = vars(b), spec = "vin",
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_within_spec", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_regex
  #
  
  regex_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 4)
      ),
      b = c(specifications$zip_codes[1:4], rep("2308", 4))
    )
  
  # Use the `col_vals_regex()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = regex_tbl) %>%
    col_vals_regex(
      columns = vars(b), regex = "[0-9]{5}",
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_regex", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # col_vals_expr
  #
  
  expr_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 4)
      ),
      b = c(specifications$zip_codes[1:4], rep("2308", 4))
    )
  
  # Use the `col_vals_expr()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = expr_tbl) %>%
    col_vals_expr(
      expr = expr(grepl("[0-9]{5}", b)),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("col_vals_expr", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(2, 2))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # conjointly
  #
  
  conjointly_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 4), rep("group_2", 4), rep(NA_character_, 2)
      ),
      b = c(c(1, 2, 3, 4), c(3, 4, 5, 6), NA, NA),
      c = c(rep(0, 4), rep(2, 4), rep(10, 2))
    )
  
  # Use the `conjointly()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = conjointly_tbl) %>%
    conjointly(
      ~ col_vals_increasing(., vars(b)),
      ~ col_vals_lt(., vars(c), value = 5),
      ~ col_vals_not_null(., vars(b)),
      segments = a ~ c("group_1", "group_2")
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("conjointly", 2)
  )
  expect_null(validation$validation_set$column %>% unlist())
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, TRUE))
  expect_equal(validation$validation_set$n, c(4, 4))
  expect_equal(nrow(validation$validation_set), 2)
  
  #
  # rows_distinct
  #
  
  distinct_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 4), rep("group_2", 4), rep(NA_character_, 2)
      ),
      b = c(c(1, 2, 3, 4), c(1, 2, 3, 1), NA, NA)
    )
  
  # Use the `rows_distinct()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation <-
    create_agent(tbl = distinct_tbl) %>%
    rows_distinct(
      columns = vars(b),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(
    validation$validation_set$assertion_type, rep("rows_distinct", 2)
  )
  expect_equal(validation$validation_set$column %>% unlist(), rep("b", 2))
  expect_equal(
    validation$validation_set$seg_expr[[1]],
    validation$validation_set$seg_expr[[2]]
  )
  expect_equal(validation$validation_set$seg_col, rep("a", 2))
  expect_equal(validation$validation_set$seg_val, c("group_1", "group_2"))
  expect_equal(validation$validation_set$all_passed, c(TRUE, FALSE))
  expect_equal(validation$validation_set$n, c(4, 4))
  expect_equal(nrow(validation$validation_set), 2)
})
