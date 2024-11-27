test_that("Interrogating conjointly with an agent yields the correct results", {

  tbl <-
    dplyr::tibble(
      a = c(5, 7, 6,  5, 8,  7),
      b = c(3, 4, 6,  8, 9, 11),
      c = c(2, 6, 8, NA, 3,  8)
    )

  # Use three validation step functions in a single
  # `conjointly()` validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = tbl) %>%
    conjointly(
      ~ col_vals_gt(., columns = vars(a), value = 4),
      ~ col_vals_lt(., columns = vars(b), value = 10),
      ~ col_vals_not_null(., columns = vars(c))
    ) %>%
    interrogate()

  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)

  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "tbl")
  expect_equal(validation$validation_set$i, 1)
  expect_equal(validation$validation_set$assertion_type, "conjointly")
  expect_type(validation$validation_set$column, "list")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(is.na(validation$validation_set$na_pass))
  expect_type(validation$validation_set$preconditions, "list")
  expect_type(validation$validation_set$actions, "list")
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 6)
  expect_equal(validation$validation_set$n_passed, 4)
  expect_equal(validation$validation_set$n_failed, 2)
  expect_equal(validation$validation_set$f_passed, 0.66667)
  expect_equal(validation$validation_set$f_failed, 0.33333)

  validation$validation_set[["values"]] %>% unlist() %>% length() %>%
    expect_equal(3)

  expect_s3_class(validation$validation_set[["values"]] %>% unlist() %>% .[[1]], "formula")
  expect_s3_class(validation$validation_set[["values"]] %>% unlist() %>% .[[2]], "formula")
  expect_s3_class(validation$validation_set[["values"]] %>% unlist() %>% .[[3]], "formula")

  validation$validation_set[["values"]] %>% unlist() %>% .[[1]] %>% as.character() %>%
    expect_equal(c("~", "col_vals_gt(., columns = vars(a), value = 4)"))
  validation$validation_set[["values"]] %>% unlist() %>% .[[2]] %>% as.character() %>%
    expect_equal(c("~", "col_vals_lt(., columns = vars(b), value = 10)"))
  validation$validation_set[["values"]] %>% unlist() %>% .[[3]] %>% as.character() %>%
    expect_equal(c("~", "col_vals_not_null(., columns = vars(c))"))

  validation$validation_set[["preconditions"]] %>% unlist() %>% length() %>%
    expect_equal(0)

  # Use three validation step functions in a single
  # `conjointly()` validation step, then, `interrogate()`;
  # use `preconditions` to mutate the `tbl` beforehand
  validation <-
    create_agent(tbl = tbl) %>%
    conjointly(
      ~ col_vals_gt(., columns = vars(a), value = 4),
      ~ col_vals_lt(., columns = vars(b), value = 10),
      ~ col_vals_not_null(., columns = vars(c)),
      preconditions = ~ . %>% head(2)
    ) %>%
    interrogate()

  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)

  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "tbl")
  expect_equal(validation$validation_set$i, 1)
  expect_equal(validation$validation_set$assertion_type, "conjointly")
  expect_true(is.list(validation$validation_set$column))
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(is.na(validation$validation_set$na_pass))
  expect_true(is.list(validation$validation_set$preconditions))
  expect_true(is.list(validation$validation_set$actions))
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 2)
  expect_equal(validation$validation_set$n_passed, 2)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)

  validation$validation_set[["values"]] %>% unlist() %>% length() %>%
    expect_equal(3)

  validation$validation_set[["preconditions"]] %>% unlist() %>% length() %>%
    expect_equal(1)

  expect_s3_class(
    validation$validation_set[["values"]] %>%
      unlist() %>%
      .[[1]],
    "formula"
  )

  expect_s3_class(
    validation$validation_set[["values"]] %>%
      unlist() %>%
      .[[2]],
    "formula"
  )

  expect_s3_class(
    validation$validation_set[["values"]] %>%
      unlist() %>%
      .[[3]],
    "formula"
  )

  validation$validation_set[["values"]] %>%
    unlist() %>%
    .[[1]] %>%
    as.character() %>%
    expect_equal(c("~", "col_vals_gt(., columns = vars(a), value = 4)"))

  validation$validation_set[["values"]] %>%
    unlist() %>%
    .[[2]] %>%
    as.character() %>%
    expect_equal(c("~", "col_vals_lt(., columns = vars(b), value = 10)"))

  validation$validation_set[["values"]] %>%
    unlist() %>%
    .[[3]] %>%
    as.character() %>%
    expect_equal(c("~", "col_vals_not_null(., columns = vars(c))"))

  validation$validation_set[["preconditions"]] %>%
    unlist() %>%
    .[[1]] %>%
    as.character() %>%
    expect_equal(c("~", ". %>% head(2)"))

  # Use a single validation step function in a single
  # `conjointly()` validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = tbl) %>%
    conjointly(
      ~ col_vals_gt(., columns = vars(a), value = 2)
    ) %>%
    interrogate()

  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)

  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "tbl")
  expect_equal(validation$validation_set$i, 1)
  expect_equal(validation$validation_set$assertion_type, "conjointly")
  expect_true(is.list(validation$validation_set$column))
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_true(is.na(validation$validation_set$na_pass))
  expect_true(is.list(validation$validation_set$preconditions))
  expect_true(is.list(validation$validation_set$actions))
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 6)
  expect_equal(validation$validation_set$n_passed, 6)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)

  validation$validation_set[["values"]] %>%
    unlist() %>%
    expect_length(1)

  validation$validation_set[["preconditions"]] %>%
    unlist() %>%
    expect_length(0)

  expect_s3_class(
    validation$validation_set[["values"]] %>%
      unlist() %>%
      .[[1]],
    "formula"
  )

  validation$validation_set[["values"]] %>%
    unlist() %>%
    .[[1]] %>%
    as.character() %>%
    expect_equal(c("~", "col_vals_gt(., columns = vars(a), value = 2)"))

  # Expect an evaluation error if there are no validation
  # step functions supplied to `conjointly()`
  expect_true(
    create_agent(tbl = tbl) %>%
      conjointly() %>%
      interrogate() %>%
      .$validation_set %>%
      .$eval_error
  )
})
