skip_on_ci()

test_that("scan_data works with dittodb-mocked Postgres database connection", {
  
  # Create a connection to the `trade_statistics`
  # database hosted publicly at "tradestatistics.io"
  
  dittodb::with_mock_db({
    # start_db_capturing()
    con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = "trade_statistics",
      user = "guest",
      password = "",
      host = "tradestatistics.io",
      port = 5432
    )
    
    # Access the `full_region` table
    yrpc <- DBI::dbGetQuery(con, "SELECT * FROM hs07_yrpc LIMIT 100")
  
    scan_results <- expect_warning(scan_data(yrpc))
    
    DBI::dbDisconnect(con)
    # stop_db_capturing()
    
    expect_is(scan_results, "examination_page")
    expect_is(scan_results, "shiny.tag.list")
    expect_is(scan_results, "list")
  })
})
