test_that("log4r_step() emits a deprecation warning", {

  expect_warning(
    log4r_step(x = list()),
    "deprecated"
  )
})
