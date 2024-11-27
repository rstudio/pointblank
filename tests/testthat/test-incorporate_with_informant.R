test_that("Incorporating an informant yields the correct results", {

  # Generate an informant object, add two snippets with `info_snippet()`,
  # add information with some other `info_*()` functions and then
  # `incorporate()` the snippets into the info text
  informant <-
    create_informant(
      tbl = ~ readr::read_csv(file = testthat::test_path("test_table.csv"), col_types = "TDdcddlc"),
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

  # Expect that names in an informant after using
  # `incorporate()` match a prescribed set of names
  expect_named(
    informant_inc,
    c(
      "tbl", "read_fn", "tbl_name", "info_label", "meta_snippets", "lang",
      "locale", "metadata", "metadata_rev"
    )
  )

  # Expect certain names in `informant$metadata` and
  # `informant$metadata_rev`
  expect_equal(
    names(informant_inc$metadata),
    c("table", "columns", "_private", "rows")
  )
  expect_equal(
    names(informant_inc$metadata_rev),
    c("info_label", "table", "columns", "rows", "updated")
  )

  # Expect informant objects of class `ptblank_informant`
  expect_s3_class(informant, "ptblank_informant")
  expect_s3_class(informant_inc, "ptblank_informant")

  # Don't expect an error if the `read_fn` is valid
  expect_no_error(
    informant %>% incorporate()
  )
  expect_no_error(
    informant %>%
      set_tbl(
        tbl = function() readr::read_csv(file = testthat::test_path("test_table.csv"), col_types = "TDdcddlc")
      ) %>%
      incorporate()
  )

  # Get the row and column count values
  n_row_i <- informant_inc$metadata_rev$table$`_rows`
  n_columns_i <- informant_inc$metadata_rev$table$`_columns`

  # Modify the internal `read_fn` to read in a slightly altered table
  informant_inc_2 <-
    informant_inc %>%
    set_tbl(
      tbl = ~ readr::read_csv(file = test_path("test_table_2.csv"), col_types = "TDdcddlcd")
    ) %>%
    incorporate()

  # Get the row and column count values
  n_row_f <- informant_inc_2$metadata_rev$table$`_rows`
  n_columns_f <- informant_inc_2$metadata_rev$table$`_columns`

  expect_equal(
    c(n_row_i, n_row_f), c("13", "26")
  )
  expect_equal(
    c(n_columns_i, n_columns_f), c("8", "9")
  )
})

test_that("Incorporating an informant from YAML yields the correct results", {

  # Generate an informant object, add two snippets with `info_snippet()`,
  # add information with some other `info_*()` functions
  informant <-
    create_informant(
      tbl = ~ readr::read_csv(file = test_path("test_table.csv"), col_types = "TDdcddlc"),
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

  informant_inc_yaml <- informant_from_yaml %>% incorporate()

  # Expect that names in an informant after using
  # `incorporate()` match a prescribed set of names
  expect_true(
    all(
      names(informant_inc_yaml) ==
        c(
          "tbl", "read_fn", "tbl_name", "info_label",
          "meta_snippets",  "lang", "locale",
          "metadata", "metadata_rev"
        )
    )
  )

  # Expect certain names in `informant$metadata` and
  # `informant$metadata_rev`
  expect_equal(
    names(informant_inc_yaml$metadata),
    c("table", "columns", "rows")
  )
  expect_equal(
    names(informant_inc_yaml$metadata_rev),
    c("info_label", "table", "columns", "rows", "updated")
  )

  # Expect informant objects of class `ptblank_informant`
  expect_s3_class(informant, "ptblank_informant")
  expect_s3_class(informant_inc_yaml, "ptblank_informant")

  # Don't expect an error if the `read_fn` is valid
  expect_no_error(
    informant_inc_yaml %>% incorporate()
  )
  expect_no_error(
    informant_inc_yaml %>%
      set_tbl(
        tbl = function() readr::read_csv(file = test_path("test_table.csv"), col_types = "TDdcddlc")
      ) %>%
      incorporate()
  )

  # Get the row and column count values
  n_row_i <- informant_inc_yaml$metadata_rev$table$`_rows`
  n_columns_i <- informant_inc_yaml$metadata_rev$table$`_columns`

  # Modify the `read_fn` in the YAML file to read in a slightly altered table
  yaml_file_lines <- readLines("informant-test_table.yml")

  read_fn_line <- which(grepl("tbl:", yaml_file_lines))

  yaml_file_lines[read_fn_line] <-
    yaml_file_lines[read_fn_line] %>%
    gsub("test_table.csv", "test_table_2.csv", .) %>%
    gsub("TDdcddlc", "TDdcddlcd", .)

  writeLines(yaml_file_lines, con = "informant-test_table.yml")

  # Incorporate the informant in the YAML file with
  # `yaml_informant_incorporate()` function
  informant_inc_yaml_2 <- yaml_informant_incorporate(filename = "informant-test_table.yml")

  # Get the row and column count values
  n_row_f <- informant_inc_yaml_2$metadata_rev$table$`_rows`
  n_columns_f <- informant_inc_yaml_2$metadata_rev$table$`_columns`

  expect_equal(
    c(n_row_i, n_row_f), c("13", "26")
  )
  expect_equal(
    c(n_columns_i, n_columns_f), c("8", "9")
  )
})

fs::file_delete(path = "informant-test_table.yml")
