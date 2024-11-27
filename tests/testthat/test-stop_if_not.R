test_that("The `stop_if_not()` function works well", {
  
  expect_error(stop_if_not(FALSE))
  expect_error(stop_if_not(c(FALSE, FALSE, FALSE, FALSE)))
  expect_error(stop_if_not(c(TRUE, FALSE, FALSE, FALSE)))
  expect_no_error(stop_if_not(TRUE))
  
  expect_error(stop_if_not(5 + "B"), regexp = "non-numeric argument to binary operator")
  expect_error(stop_if_not(stop()))
  
  expect_null(stop_if_not(5))
  expect_null(stop_if_not(NULL))
  expect_null(stop_if_not())
  expect_null(stop_if_not(c()))
  expect_null(stop_if_not(stop_if_not()))
})
