expect_equal_unlist <- function(object, expected, ...) {
  expect_equal(
    unlist(object),
    expected
  )
}
