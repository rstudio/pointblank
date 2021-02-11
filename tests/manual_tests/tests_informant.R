library(pointblank)

test_table <- small_table

informant <- 
  create_informant(
    read_fn = ~ test_table,
    tbl_name = "test_table"
  ) %>%
  info_columns(
    vars(a),
    info = "In the range of 1 to 10. (((SIMPLE)))"
  ) %>%
  info_snippet(snippet_name = "row_count", fn = ~ . %>% nrow()) %>%
  info_snippet(snippet_name = "col_count", fn = ~ . %>% ncol()) %>%
  info_columns(
    starts_with("date"),
    info = "Time-based values (e.g., `Sys.time()`)."
  ) %>%
  info_columns(
    "date",
    info = "The date part of `date_time`. (((calculation)))"
  ) %>%
  info_section(
    section_name = "summary",
    row_count = "There are {row_count} rows available."
  ) %>%
  info_tabular(
    summary = "This table is available in the **pointblank** package."
  ) %>%
  incorporate()

informant

test_table <- 
  dplyr::bind_rows(test_table, test_table) %>%
  dplyr::mutate(h = a + c)

informant <-
  informant %>% incorporate()

get_informant_report(informant = informant)

yaml_write(informant, filename = "informant-test.yml")
