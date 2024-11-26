test_that("Reading an informant from YAML is possible", {
  
  # Generate an informant object, add two snippets with `info_snippet()`,
  # add information with some other `info_*()` functions
  informant <- 
    create_informant(
      tbl = ~ readr::read_csv(file = "test_table.csv", col_types = "TDdcddlc"),
      tbl_name = "test_table",
      label = "An example."
    ) %>%
    info_snippet(
      snippet_name = "row_count",
      fn = ~ . %>% nrow()
    ) %>%
    info_snippet(
      snippet_name = "col_count",
      fn = ~ . %>% ncol()
    ) %>%
    info_tabular(
      info = "Table is obtained from `test_table.csv`."
    ) %>%
    info_columns(
      columns = vars(a),
      info = "In the range of 1 to 10. (SIMPLE)"
    ) %>%
    info_columns(
      columns = starts_with("date"),
      info = "Time-based values (e.g., `Sys.time()`)."
    ) %>%
    info_columns(
      columns = "date",
      info = "The date part of `date_time`. (CALC)"
    ) %>%
    info_section(
      section_name = "rows",
      row_count = "There are {row_count} rows available."
    )
  yaml_write(informant, filename = "informant-test_table.yml")
  
  informant_from_yaml <- yaml_read_informant(filename = "informant-test_table.yml")
  
  # Expect that the original informant and the one read
  # back from YAML are both of the class `ptblank_informant`
  expect_is(informant, "ptblank_informant")
  expect_is(informant_from_yaml, "ptblank_informant")
  
  # Expect both informant objects to successfully produce a report
  expect_no_error(
    report_1 <- get_informant_report(informant)
  )
  expect_no_error(
    report_1_from_yaml <- get_informant_report(informant_from_yaml)
  )
  expect_is(report_1, "gt_tbl")
  expect_is(report_1_from_yaml, "gt_tbl")
  
  # Expect that the informant (which never had `incorporate()`
  # run on it) is equivalent to the informant object created
  # via `yaml_read_informant()` (i.e., reading in the YAML file)
  # - *Except* private fields which are note written
  informant$metadata$`_private` <- NULL
  expect_equivalent(informant, informant_from_yaml)
  
  # Use `incorporate()` on the informant; this creates the list
  # component `metadata_rev` in the `informant` which is for
  # metadata revisions (the latest only)
  informant_inc <- informant %>% incorporate()
  
  yaml_write(informant_inc, filename = "informant-test_table-2.yml")
  
  informant_inc_from_yaml <- yaml_read_informant(filename = "informant-test_table-2.yml")
  
  # Expect that the `informant_inc` object has the revised
  # metadata list component but that the derived object from
  # the YAML (made from `informant_inc`) does not contain it
  expect_true(!is.null(informant_inc$metadata_rev))
  expect_null(informant_inc_from_yaml$metadata_rev)
  
  # Expect both informant objects to successfully produce a report
  expect_no_error(
    report_1_inc <- get_informant_report(informant_inc)
  )
  expect_no_error(
    report_1_from_yaml_inc <- get_informant_report(informant_inc_from_yaml)
  )
  expect_is(report_1_inc, "gt_tbl")
  expect_is(report_1_from_yaml_inc, "gt_tbl")
})

fs::file_delete(path = "informant-test_table.yml")
fs::file_delete(path = "informant-test_table-2.yml")
