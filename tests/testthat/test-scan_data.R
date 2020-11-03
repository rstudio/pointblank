test_that("Using `scan_data()` results in an HTML document", {
  
  skip_on_cran()
  skip_on_ci()
  
  scan_data_html <- 
    scan_data(
      tbl = small_table, 
      sections = c("overview", "variables")
    ) %>%
    as.character()
  
  expect_equal(length(scan_data_html), 1)
  
  expect_true(
    grepl(
      "^<!doctype html>.*</body>.*?</html>$",
      scan_data_html
    )
  )
  
  scan_data_sqlite_html <- 
    scan_data(
      tbl = small_table_sqlite(), 
      sections = c("overview", "variables")
    ) %>%
    as.character()
  
  expect_equal(length(scan_data_sqlite_html), 1)
  
  expect_true(
    grepl(
      "^<!doctype html>.*</body>.*?</html>$",
      scan_data_sqlite_html
    )
  )
  
  expect_error(
    scan_data(
      tbl = small_table,
      sections = NULL
    )
  )
  
  expect_error(
    scan_data(
      tbl = small_table,
      sections = c()
    )
  )
  
  expect_error(
    scan_data(
      tbl = small_table,
      sections = "extra"
    )
  )
})
