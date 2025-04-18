z <- rlang::missing_arg()

test_that("the `has_columns()` function works when used directly with data", {

  # Expect TRUE when *all* of the given column names is present
  expect_true(small_table %>% has_columns("date"))
  expect_true(small_table %>% has_columns(c("a", "b")))
  expect_true(small_table %>% has_columns(vars(a, b)))
  expect_true(small_table %>% has_columns(c(vars(a, b), vars(c))))

  # Expect FALSE when *any* of the given column names is absent
  expect_false(small_table %>% has_columns(vars(z)))
  expect_false(small_table %>% has_columns("z"))
  expect_false(small_table %>% has_columns(vars(a, z)))
  expect_false(small_table %>% has_columns(vars(z, zz)))
  expect_false(small_table %>% has_columns(c("a", "z")))
  expect_false(small_table %>% has_columns(c("z", "zz")))

  # Expect that using inputs that are not tabular result in errors
  expect_error(has_columns(list(a = "2"), "a"))
  expect_error(has_columns(c(a = 1, b = 2), "a"))
  expect_error(has_columns(matrix(c(1, 2, 3, 4), nrow = 2), "b"))
})

test_that("the `has_columns()` function works with tidyselect", {

  # Select helpers work
  expect_true(small_table %>% has_columns(contains("te")))
  expect_true(small_table %>% has_columns(starts_with("date")))
  expect_true(small_table %>% has_columns(ends_with("time")))
  expect_true(small_table %>% has_columns(everything()))
  expect_true(small_table %>% has_columns(matches(".*te")))

  # Empty selection from helpers is FALSE
  expect_false(small_table %>% has_columns(contains("z")))
  expect_false(small_table %>% has_columns(starts_with("z")))
  expect_false(small_table %>% has_columns(ends_with("z")))
  expect_false(small_table %>% has_columns(last_col() + 1))
  expect_false(small_table %>% has_columns(matches("z")))

  # Expect FALSE when *any* of the given column names is absent
  expect_false(small_table %>% has_columns(c(a, z)))

  # Genuine evaluation errors are re-thrown and short-circuits
  expect_error(small_table %>% has_columns(stop("Oh no!")), "Oh no!")
  expect_error(small_table %>% has_columns(c(stop("Oh no!"))), "Oh no!")
  expect_error(small_table %>% has_columns(c(a, stop("Oh no!"))), "Oh no!")
  expect_error(small_table %>% has_columns(c(stop("Oh no!"), stop("Don't reach me!"))), "Oh no!")

  # Mix of selections work like AND
  expect_true(small_table %>% has_columns(c(contains("da"), contains("te"))))
  expect_true(small_table %>% has_columns(c(a, contains("te"))))
  expect_false(small_table %>% has_columns(c(z, contains("te"))))
  expect_false(small_table %>% has_columns(c(a, contains("z"))))
  expect_false(small_table %>% has_columns(c(contains("da"), contains("z"))))

  # Nested vector patterns
  expect_identical(
    small_table %>% has_columns(c(vars(a, b), vars(c))),
    small_table %>% has_columns(c(c(a, b), c))
  )
  expect_identical(
    small_table %>% has_columns(c(vars(a, b), vars(c))),
    small_table %>% has_columns(c(all_of(c("a", "b")), c))
  )

  # `any_of()`/`all_of()` patterns with external vectors
  has_one <- c("a", "y", "z")
  has_all <- c("a", "b", "c")
  has_none <- c("x", "y", "z")
  expect_true(small_table %>% has_columns(any_of(has_one)))
  expect_true(small_table %>% has_columns(any_of(has_all)))
  expect_false(small_table %>% has_columns(any_of(has_none)))
  expect_false(small_table %>% has_columns(all_of(has_one)))
  expect_true(small_table %>% has_columns(all_of(has_all)))
  expect_false(small_table %>% has_columns(all_of(has_none)))

})
