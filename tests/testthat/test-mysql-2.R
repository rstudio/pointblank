test_that("scan_data works with dittodb-mocked MySQL database connection", {
  # Create a connection to the `aedes_aegypti_core_55_1d`
  # database hosted publicly at "ensembldb.ensembl.org"
  
  testthat::skip_on_os("solaris")
  
  dittodb::with_mock_db({
    # start_db_capturing()
    con <- DBI::dbConnect(
      drv = RMariaDB::MariaDB(),
      dbname = "aedes_aegypti_core_55_1d",
      username = "anonymous",
      password = "",
      host = "ensembldb.ensembl.org",
      port = 3306
    )

    # Access the `assembly` table
    assembly <- DBI::dbGetQuery(con, "SELECT * FROM assembly LIMIT 100")

    # Use the `scan_data()` function
    scan_results <- expect_warning(scan_data(assembly))
    
    DBI::dbDisconnect(con)
    # stop_db_capturing()
    
    expect_is(scan_results, "examination_page")
    expect_is(scan_results, "shiny.tag.list")
    expect_is(scan_results, "list")
  })
})
