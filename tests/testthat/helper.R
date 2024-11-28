expect_equal_unlist <- function(object, expected, ...) {
  expect_equal(
    unlist(object),
    expected
  )
}

skip_if_not_utf8 <- function() {
  # likely on Windows for R < 4.2
  skip_if_not(l10n_info()$`UTF-8`)
}
