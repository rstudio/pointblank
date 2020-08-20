test_that("pointblank agent works with dittodb-mocked SQL Server database connection", {
  # Create a connection to the `aedes_aegypti_core_55_1d`
  # database hosted publicly at "ensembldb.ensembl.org"
  dittodb::with_mock_db({
    # start_db_capturing()
    con <- DBI::dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 17 for SQL Server",
      Server = "127.0.0.1",
      UID = "pointblank",
      PWD = "P01ntBl4nk",
      Port = 1433
    )
    
    # Set failure thresholds and functions that are
    # actioned from exceeding certain error levels
    al <-  action_levels(warn_at = 0.02, stop_at = 0.05, notify_at = 0.10)
    
    # Validate the `flights` table in the `master` DB
    # the expect_warning is used to suppres the message
    # dbFetch `n` is ignored while mocking databases.
    agent <- dplyr::tbl(con, "planes") %>%
      create_agent(
        name = "master: 'planes' table",
        actions = al
      ) %>%
      col_vals_lte(vars(year), 2013L) %>%
      # col_vals_gte(vars(year), 1956L) %>%
      # col_vals_gt(vars(engines), 0L) %>%
      # col_vals_gt(vars(seats), 0L) %>%
      # col_vals_in_set(vars(manufacturer), "BOEING") %>%
      # col_schema_match(
      #   schema = col_schema(
      #     manufacturer = "character",
      #     year = "integer",
      #     engines = "integer",
      #     seats = "integer"
      #   )
      # ) %>%
      interrogate()
    
    DBI::dbDisconnect(con)
    # stop_db_capturing()
  })
})
