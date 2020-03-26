test_that("Using `scan_data()` results in an HTML document", {
  
  if (requireNamespace("gt", quietly = TRUE)) {
    
    scan_data_html <- scan_data(tbl = datasets::airquality) %>% as.character()
    
    expect_equal(length(scan_data_html), 1)
    
    expect_true(
      grepl(
        "^<!doctype html>.*</body>.*?</html>$",
        scan_data_html
      )
    )
    
    scan_data_html <- scan_data(tbl = small_table) %>% as.character()
    
    expect_equal(length(scan_data_html), 1)
    
    expect_true(
      grepl(
        "^<!doctype html>.*</body>.*?</html>$",
        scan_data_html
      )
    )
    
    scan_data_html <- small_table %>% scan_data() %>% as.character()
    
    expect_equal(length(scan_data_html), 1)
    
    expect_true(
      grepl(
        "^<!doctype html>.*</body>.*?</html>$",
        scan_data_html
      )
    )
    
    expect_error(
      small_table_sqlite() %>% scan_data() %>% as.character()
    )
  }
})
