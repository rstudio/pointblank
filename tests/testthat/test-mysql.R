test_that("pointblank agent works with dittodb-mocked MySQL database connection", {
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
    
    # Set failure thresholds and functions that are
    # actioned from exceeding certain error levels
    al <-  action_levels(warn_at = 0.02, stop_at = 0.05, notify_at = 0.10)
    
    # Validate the `assembly` table in the `aedes_aegypti_core_55_1d` DB
    # the expect_warning is used to suppres the message
    # dbFetch `n` is ignored while mocking databases.
    agent <- expect_warning(dplyr::tbl(con, "assembly") %>%
      create_agent(
        name = "aedes_aegypti_core_55_1d: 'assembly' table",
        actions = al
      ) %>%
      col_vals_equal(vars(cmp_start), 1) %>%
      col_vals_equal(vars(ori), 1) %>%
      col_vals_gt(vars(asm_seq_region_id), 1) %>%
      col_vals_gt(vars(cmp_seq_region_id), 1) %>%
      col_vals_gt(vars(asm_end), vars(asm_start)) %>%
      col_vals_gt(vars(cmp_end), vars(cmp_start)) %>%
      col_schema_match(
        schema = col_schema(
          asm_seq_region_id = "integer",
          cmp_seq_region_id = "integer",
          asm_start = "integer",
          asm_end = "integer",
          cmp_start = "integer",
          cmp_end = "integer",
          ori = "integer"
        )
      ) %>%
      interrogate())
    
    DBI::dbDisconnect(con)
    # stop_db_capturing()
    
    expect_equal(agent$name, "aedes_aegypti_core_55_1d: 'assembly' table")
    expect_equal(agent$tbl_name, "table")
    expect_equal(agent$tbl_src, "mysql")
    expect_equal(agent$tbl_src_details, "mysql  [anonymous@ensembldb.ensembl.org:NA/aedes_aegypti_core_55_1d]")
    expect_equal(agent$col_names, c("asm_seq_region_id", "cmp_seq_region_id", "asm_start", "asm_end", "cmp_start", "cmp_end", "ori"))
    expect_equal(agent$col_types, c("integer", "integer", "integer", "integer", "integer", "integer", "integer"))
    expect_equal(agent$db_col_types, c("int", "int", "int", "int", "int", "int", "tinyint"))
    expect_equal(nrow(agent$validation_set), 7)
    expect_equal(ncol(agent$validation_set), 27)
  })
})
