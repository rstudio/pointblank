test_that("col_vals_str_len works with agent workflow", {

  tbl <- dplyr::tibble(
    x = c("AB", "CDE", "FGHI", "JK"),
    y = c(1, 2, 3, 4)
  )

  agent <-
    create_agent(tbl = tbl) %>%
    col_vals_str_len(columns = x, min = 2, max = 4) %>%
    interrogate()

  expect_true(all_passed(agent))

  agent_fail <-
    create_agent(tbl = tbl) %>%
    col_vals_str_len(columns = x, min = 3, max = 3) %>%
    interrogate()

  expect_false(all_passed(agent_fail))
})

test_that("col_vals_str_len works with min only", {

  tbl <- dplyr::tibble(x = c("abc", "de", "fghij"))

  expect_true(test_col_vals_str_len(tbl, x, min = 2))
  expect_false(test_col_vals_str_len(tbl, x, min = 3))
})

test_that("col_vals_str_len works with max only", {

  tbl <- dplyr::tibble(x = c("abc", "de", "fghij"))

  expect_true(test_col_vals_str_len(tbl, x, max = 5))
  expect_false(test_col_vals_str_len(tbl, x, max = 4))
})

test_that("col_vals_str_len handles NA values", {

  tbl <- dplyr::tibble(x = c("ab", NA, "cde"))

  expect_false(test_col_vals_str_len(tbl, x, min = 2, max = 3))
  expect_true(test_col_vals_str_len(tbl, x, min = 2, max = 3, na_pass = TRUE))
})

test_that("col_vals_str_len errors when no min or max given", {

  tbl <- dplyr::tibble(x = c("ab", "cd"))

  expect_error(
    col_vals_str_len(tbl, x),
    "At least one of `min` or `max`"
  )
})

test_that("col_vals_str_len table pipeline mode works", {

  tbl <- dplyr::tibble(x = c("ab", "cde"))

  result <- tbl %>% col_vals_str_len(x, min = 2, max = 3)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("expect_col_vals_str_len works", {

  tbl <- dplyr::tibble(x = c("ab", "cde", "fghi"))

  expect_col_vals_str_len(tbl, x, min = 2, max = 4)

  expect_failure(
    expect_col_vals_str_len(tbl, x, min = 3, max = 3)
  )
})

test_that("col_vals_str_len exact length check works", {

  tbl <- dplyr::tibble(x = c("abc", "def", "ghi"))

  expect_true(test_col_vals_str_len(tbl, x, min = 3, max = 3))
  expect_false(test_col_vals_str_len(tbl, x, min = 4, max = 4))
})
