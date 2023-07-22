skip_on_ci()

test_that("pointblank agent works with dittodb-mocked Postgres database connection", {
  
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
    
    # Set failure thresholds and functions that are
    # actioned from exceeding certain error levels
    al <- action_levels(warn_at = 0.02, stop_at = 0.05, notify_at = 0.10)
    
    # Validate the `assembly` table in the `aedes_aegypti_core_55_1d` DB
    # the expect_warning is used to suppres the message
    # dbFetch `n` is ignored while mocking databases.
    agent <- expect_warning(
      dplyr::tbl(con, "hs07_yrp") %>%
        create_agent(
          label = "trade_statistics: 'hs07_yrp' table",
          actions = al
        ) %>%
        col_vals_gte(vars(export_value_usd), 0) %>% 
        col_vals_gte(vars(import_value_usd), 0) %>% 
        col_schema_match(
          schema = col_schema(
            year = "integer",
            reporter_iso = "character",
            partner_iso = "character",
            export_value_usd = "numeric",
            import_value_usd = "numeric"
          )
        ) %>%
        interrogate())
    
    DBI::dbDisconnect(con)
    # stop_db_capturing()
    
    expect_equal(agent$label, "trade_statistics: 'hs07_yrp' table")
    expect_equal(agent$tbl_name, NA_character_)
    expect_equal(agent$tbl_src, "postgres")
    expect_equal(agent$tbl_src_details, "postgres  [guest@tradestatistics.io:5432/trade_statistics]")
    expect_equal(agent$col_names, c("year", "reporter_iso", "partner_iso", "export_value_usd", "import_value_usd"))
    expect_equal(agent$col_types, c("integer", "character", "character", "numeric", "numeric"))
    expect_equal(agent$db_col_types, c("integer", "character varying", "character varying", "numeric", "numeric"))
    expect_equal(nrow(agent$validation_set), 3)
    expect_equal(ncol(agent$validation_set), 29)
  })
})
