skip_on_cran()

increasing_tbl <-
  dplyr::tibble(
    a = c(
      rep("group_inc_1", 5), rep("group_inc_2", 10),
      rep("group_not", 5), rep(NA_character_, 10)
    ),
    b = c(1:5, 5:14, 5:1, 1:10)
  )

decreasing_tbl <-
  dplyr::tibble(
    a = c(
      rep("group_dec_1", 5), rep("group_dec_2", 10),
      rep("group_not", 5), rep(NA_character_, 10)
    ),
    b = c(5:1, 14:5, 1:5, 10:1)
  )

test_that("Agent interrogations with segments yields the correct results", {
  
  #
  # col_vals_increasing
  #
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by two grouping values from a single column
  validation_1 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b),
      segments = a ~ c("group_inc_1", "group_inc_2")) %>%
    interrogate()
  
  # Expect certain values in `validation_1$validation_set`
  expect_equivalent(
    validation_1$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equivalent(validation_1$validation_set$column %>% unlist(), rep("b", 2))
  expect_equivalent(
    validation_1$validation_set$seg_expr[[1]],
    validation_1$validation_set$seg_expr[[2]]
  )
  expect_equivalent(validation_1$validation_set$seg_col, rep("a", 2))
  expect_equivalent(validation_1$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equivalent(validation_1$validation_set$all_passed, c(TRUE, TRUE))
  expect_equivalent(validation_1$validation_set$n, c(5, 10))
  expect_equivalent(nrow(validation_1$validation_set), 2)
  
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
  expect_equivalent(
    validation_2$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equivalent(validation_2$validation_set$column %>% unlist(), rep("b", 2))
  expect_equivalent(
    validation_2$validation_set$seg_expr[[1]],
    validation_2$validation_set$seg_expr[[2]]
  )
  expect_equivalent(validation_2$validation_set$seg_col, rep("a", 2))
  expect_equivalent(validation_2$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equivalent(validation_2$validation_set$all_passed, c(TRUE, TRUE))
  expect_equivalent(validation_2$validation_set$n, c(5, 10))
  expect_equivalent(nrow(validation_2$validation_set), 2)
  
  # Use the `col_vals_increasing()` function with `segments`,
  # segmenting the data by *all* grouping values from a single column (`a`)
  validation_3 <-
    create_agent(tbl = increasing_tbl) %>%
    col_vals_increasing(
      columns = vars(b), segments = vars(a)) %>%
    interrogate()
  
  # Expect certain values in `validation_3$validation_set`
  expect_equivalent(
    validation_3$validation_set$assertion_type, rep("col_vals_increasing", 4)
  )
  expect_equivalent(validation_3$validation_set$column %>% unlist(), rep("b", 4))
  expect_equivalent(
    validation_3$validation_set$seg_expr[[1]],
    validation_3$validation_set$seg_expr[[2]],
    validation_3$validation_set$seg_expr[[3]],
    validation_3$validation_set$seg_expr[[4]],
  )
  expect_equivalent(validation_3$validation_set$seg_col, rep("a", 4))
  expect_equivalent(
    validation_3$validation_set$seg_val,
    c("group_inc_1", "group_inc_2", "group_not", NA_character_)
  )
  expect_equivalent(validation_3$validation_set$all_passed, c(TRUE, TRUE, FALSE, FALSE))
  expect_equivalent(validation_3$validation_set$n, c(5, 10, 5, 30))
  expect_equivalent(nrow(validation_3$validation_set), 4)
  
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
  expect_equivalent(
    validation_4$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equivalent(validation_4$validation_set$column %>% unlist(), rep("b", 2))
  expect_equivalent(
    validation_4$validation_set$seg_expr[[1]],
    validation_4$validation_set$seg_expr[[2]]
  )
  expect_equivalent(validation_4$validation_set$seg_col, rep("a", 2))
  expect_equivalent(validation_4$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equivalent(validation_4$validation_set$all_passed, c(TRUE, TRUE))
  expect_equivalent(validation_4$validation_set$n, c(5, 10))
  expect_equivalent(nrow(validation_4$validation_set), 2)

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
  expect_equivalent(
    validation_5$validation_set$assertion_type, rep("col_vals_increasing", 2)
  )
  expect_equivalent(validation_5$validation_set$column %>% unlist(), rep("b", 2))
  expect_equivalent(
    validation_5$validation_set$seg_expr[[1]],
    validation_5$validation_set$seg_expr[[2]]
  )
  expect_equivalent(validation_5$validation_set$seg_col, rep("a", 2))
  expect_equivalent(validation_5$validation_set$seg_val, c("group_inc_1", "group_inc_2"))
  expect_equivalent(validation_5$validation_set$all_passed, c(TRUE, TRUE))
  expect_equivalent(validation_5$validation_set$n, c(5, 10))
  expect_equivalent(nrow(validation_5$validation_set), 2)
  
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
  
})
