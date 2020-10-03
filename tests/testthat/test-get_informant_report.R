test_that("Getting an information report is possible", {
  
  # Generate an informant object, add two snippets with `info_snippet()`,
  # add information with some other `info_*()` functions and then
  # `incorporate()` the snippets into the info text
  informant <- 
    create_informant(
      read_fn = ~ readr::read_csv(file = "test_table.csv", col_types = "TDdcddlc"),
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
  
  informant_inc <- informant %>% incorporate()
  
  # Get an information report
  report <- get_informant_report(informant_inc)
  
  # Expect that the report is a gt table
  expect_is(report, "gt_tbl")
})
