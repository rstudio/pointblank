skip_on_ci()
skip_on_os(os = "windows")

small_table_sqlite <- small_table_sqlite()
small_table_duckdb <- db_tbl(table = small_table, dbname = ":memory:", dbtype = "duckdb")

spec_table_sqlite <- db_tbl(table = specifications, dbname = ":memory:", dbtype = "sqlite")
spec_table_duckdb <- db_tbl(table = specifications, dbname = ":memory:", dbtype = "duckdb")

check_missing_in_column <- function(tbl, has_missing, not_missing, col_names) {

  for (col in col_names) {

    missing_in_bin <-
      any(!(
        tbl %>%
          dplyr::filter(col_name == .env$col) %>%
          dplyr::pull(value) %>%
          is.na()
      ))

    if (col %in% has_missing) {
      expect_true(missing_in_bin)
    } else {
      expect_false(missing_in_bin)
    }
  }
}

test_that("the `get_table_column_names()` function works", {

  expect_equal(
    get_table_column_names(small_table),
    c("date_time", "date", "a", "b", "c", "d", "e", "f")
  )
  expect_equal(
    get_table_column_names(small_table_sqlite),
    c("date_time", "date", "a", "b", "c", "d", "e", "f")
  )
  expect_equal(
    get_table_column_names(small_table_duckdb),
    c("date_time", "date", "a", "b", "c", "d", "e", "f")
  )
})

test_that("the `get_table_total_rows()` function works", {

  expect_equal(get_table_total_rows(small_table), 13)
  expect_equal(get_table_total_rows(small_table_sqlite), 13)
  expect_equal(get_table_total_rows(small_table_duckdb), 13)
})

test_that("the `get_table_total_columns()` function works", {

  expect_equal(get_table_total_columns(small_table), 8)
  expect_equal(get_table_total_columns(small_table_sqlite), 8)
  expect_equal(get_table_total_columns(small_table_duckdb), 8)
})

test_that("the `get_table_total_missing_values()` function works", {

  expect_equal(get_table_total_missing_values(specifications), 12)
  expect_equal(get_table_total_missing_values(spec_table_sqlite), 12)
  expect_equal(get_table_total_missing_values(spec_table_duckdb), 12)
})

test_that("the `get_table_total_distinct_rows()` function works", {

  expect_equal(get_table_total_distinct_rows(small_table), 12)
  expect_equal(get_table_total_distinct_rows(small_table_sqlite), 12)
  expect_equal(get_table_total_distinct_rows(small_table_duckdb), 12)

  expect_equal(get_table_total_distinct_rows(specifications), 8)
  expect_equal(get_table_total_distinct_rows(spec_table_sqlite), 8)
  expect_equal(get_table_total_distinct_rows(spec_table_duckdb), 8)
})

test_that("the `get_table_column_distinct_rows()` function works", {

  small_table_c <- small_table %>% dplyr::select(c)
  small_table_sqlite_c <- small_table_sqlite %>% dplyr::select(c)
  small_table_duckdb_c <- small_table_duckdb %>% dplyr::select(c)

  expect_equal(get_table_column_distinct_rows(small_table_c), 7)
  expect_equal(get_table_column_distinct_rows(small_table_sqlite_c), 7)
  expect_equal(get_table_column_distinct_rows(small_table_duckdb_c), 7)
})

test_that("the `get_table_column_na_values()` function works", {

  small_table_c <- small_table %>% dplyr::select(c)
  small_table_sqlite_c <- small_table_sqlite %>% dplyr::select(c)
  small_table_duckdb_c <- small_table_duckdb %>% dplyr::select(c)

  expect_equal(get_table_column_na_values(small_table_c), 2)
  expect_equal(get_table_column_na_values(small_table_sqlite_c), 2)
  expect_equal(get_table_column_na_values(small_table_duckdb_c), 2)
})

test_that("the `get_table_column_inf_values()` function works", {

  tbl_inf <- dplyr::tibble(a = c(2.3, NA, 6.1, 0.8, Inf, 5.2, -Inf, Inf, 3.2, -Inf))
  small_table_sqlite_c <- small_table_sqlite %>% dplyr::select(c)
  small_table_duckdb_c <- small_table_duckdb %>% dplyr::select(c)

  expect_equal(get_table_column_inf_values(tbl_inf), 4)
  expect_equal(get_table_column_inf_values(small_table_sqlite_c), 0)
  expect_equal(get_table_column_inf_values(small_table_duckdb_c), 0)
})

test_that("the `get_table_column_summary()` function works", {

  small_table_c <- small_table %>% dplyr::select(c)
  small_table_sqlite_c <- small_table_sqlite %>% dplyr::select(c)
  small_table_duckdb_c <- small_table_duckdb %>% dplyr::select(c)

  expect_equal(
    get_table_column_summary(small_table_c),
    dplyr::tibble(mean = 5.73, min = 2, max = 9)
  )
  expect_equal(
    get_table_column_summary(small_table_sqlite_c),
    dplyr::tibble(mean = 5.73, min = 2, max = 9)
  )
  expect_equal(
    get_table_column_summary(small_table_duckdb_c),
    dplyr::tibble(mean = 5.73, min = 2, max = 9)
  )
})

test_that("the `get_df_column_qtile_stats()` function works for data frames", {

  small_table_d <- small_table %>% dplyr::select(d)

  expected_qtile_stats <-
    list(
      min = 108.34,
      p05 = 213.7,
      q_1 = 837.93,
      med = 1035.64,
      q_3 = 3291.03,
      p95 = 6335.44,
      max = 9999.99,
      iqr = 2453.1,
      range = 9891.65
    )

  expect_equal(
    get_df_column_qtile_stats(small_table_d),
    expected_qtile_stats
  )
})

test_that("the `get_dbi_column_qtile_stats()` function works for `tbl_dbi` data", {

  small_table_sqlite_d <- small_table_sqlite %>% dplyr::select(d)
  small_table_duckdb_d <- small_table_duckdb %>% dplyr::select(d)

  expected_qtile_stats <-
    list(
      min = 108.34,
      p05 = 108.34,
      q_1 = 833.98,
      med = 843.34,
      q_3 = 2343.23,
      p95 = 3892.4,
      max = 9999.99,
      iqr = 1509.25,
      range = 9891.65
    )

  expect_equal(
    get_dbi_column_qtile_stats(small_table_sqlite_d),
    expected_qtile_stats
  )

  expect_equal(
    get_dbi_column_qtile_stats(small_table_duckdb_d),
    expected_qtile_stats
  )
})

test_that("the `get_dbi_column_mean()` function works for `tbl_dbi` data", {

  small_table_sqlite_c <- small_table_sqlite %>% dplyr::select(c)
  small_table_duckdb_c <- small_table_duckdb %>% dplyr::select(c)

  small_table_sqlite_d <- small_table_sqlite %>% dplyr::select(d)
  small_table_duckdb_d <- small_table_duckdb %>% dplyr::select(d)

  expect_equal(get_dbi_column_mean(small_table_sqlite_c), 5.727, tolerance = 0.1)
  expect_equal(get_dbi_column_mean(small_table_duckdb_c), 5.727, tolerance = 0.1)

  expect_equal(get_dbi_column_mean(small_table_sqlite_d), 2304.702, tolerance = 0.1)
  expect_equal(get_dbi_column_mean(small_table_duckdb_d), 2304.702, tolerance = 0.1)
})

test_that("the `get_dbi_column_variance()` function works for `tbl_dbi` data", {

  small_table_sqlite_c <- small_table_sqlite %>% dplyr::select(c)
  small_table_duckdb_c <- small_table_duckdb %>% dplyr::select(c)

  small_table_sqlite_d <- small_table_sqlite %>% dplyr::select(d)
  small_table_duckdb_d <- small_table_duckdb %>% dplyr::select(d)

  c_mean <- 5.727
  d_mean <- 2304.702

  expect_equal(get_dbi_column_variance(small_table_sqlite_c, mean_value = c_mean), 6.74, tolerance = 0.1)
  expect_equal(get_dbi_column_variance(small_table_duckdb_c, mean_value = c_mean), 6.74, tolerance = 0.1)

  expect_equal(get_dbi_column_variance(small_table_sqlite_d, mean_value = d_mean), 6391448, tolerance = 1)
  expect_equal(get_dbi_column_variance(small_table_duckdb_d, mean_value = d_mean), 6391448, tolerance = 1)
})

test_that("the `get_table_column_histogram()` function works", {

  small_table_f <- small_table %>% dplyr::select(f)
  small_table_sqlite_f <- small_table_sqlite %>% dplyr::select(f)
  small_table_duckdb_f <- small_table_duckdb %>% dplyr::select(f)

  expect_s3_class(get_table_column_histogram(small_table_f, lang = "en", locale = "en"), c("gg", "ggplot"))
  expect_s3_class(get_table_column_histogram(small_table_sqlite_f, lang = "en", locale = "en"), c("gg", "ggplot"))
  expect_s3_class(get_table_column_histogram(small_table_duckdb_f, lang = "en", locale = "en"), c("gg", "ggplot"))
})

test_that("the `get_tbl_df_missing_tbl()` function works", {

  missing_tbl_small_table <- get_tbl_df_missing_tbl(small_table)
  missing_tbl_diamonds <- get_tbl_df_missing_tbl(ggplot2::diamonds)
  missing_tbl_airquality <- get_tbl_df_missing_tbl(airquality)

  colnames_small_table <- colnames(small_table)
  has_missing <- "c"
  not_missing <- base::setdiff(colnames_small_table, has_missing)
  check_missing_in_column(missing_tbl_small_table, has_missing, not_missing, colnames_small_table)

  colnames_diamonds <- colnames(ggplot2::diamonds)
  has_missing <- ""
  not_missing <- colnames_diamonds
  check_missing_in_column(missing_tbl_diamonds, has_missing, not_missing, colnames_diamonds)

  colnames_diamonds <- colnames(airquality)
  has_missing <- c("Ozone", "Solar.R")
  not_missing <- base::setdiff(colnames_diamonds, has_missing)
  check_missing_in_column(missing_tbl_airquality, has_missing, not_missing, colnames_diamonds)
})

test_that("the `get_tbl_dbi_missing_tbl()` function works", {

  missing_tbl_small_table_sqlite <- get_tbl_dbi_missing_tbl(small_table_sqlite)
  missing_tbl_small_table_duckdb <- get_tbl_dbi_missing_tbl(small_table_duckdb)

  missing_tbl_spec_table_sqlite <- get_tbl_dbi_missing_tbl(spec_table_sqlite)
  missing_tbl_spec_table_duckdb <- get_tbl_dbi_missing_tbl(spec_table_duckdb)

  colnames_small_table <- colnames(small_table)
  has_missing <- "c"
  not_missing <- base::setdiff(colnames_small_table, has_missing)
  check_missing_in_column(missing_tbl_small_table_sqlite, has_missing, not_missing, colnames_small_table)
  check_missing_in_column(missing_tbl_small_table_duckdb, has_missing, not_missing, colnames_small_table)

  colnames_specifications <- colnames(specifications)
  has_missing <- colnames_specifications
  not_missing <- ""
  check_missing_in_column(missing_tbl_spec_table_sqlite, has_missing, not_missing, colnames_specifications)
  check_missing_in_column(missing_tbl_spec_table_duckdb, has_missing, not_missing, colnames_specifications)
})

test_that("the `get_missing_by_column_tbl()` function works", {

  missing_by_col_small_table <- get_missing_by_column_tbl(small_table)
  missing_by_col_small_table_sqlite <- get_missing_by_column_tbl(small_table_sqlite)
  missing_by_col_small_table_duckdb <- get_missing_by_column_tbl(small_table_duckdb)

  expect_equal(missing_by_col_small_table, missing_by_col_small_table_sqlite)
  expect_equal(missing_by_col_small_table, missing_by_col_small_table_duckdb)

  expect_equal(
    missing_by_col_small_table %>% dplyr::mutate(col_name = as.character(col_name)),
    dplyr::tibble(
      value = c(0, 0, 0, 0, 0.15, 0, 0, 0),
      col_num = 1:8,
      col_name = colnames(small_table)
    )
  )

  missing_by_col_spec_table <- get_missing_by_column_tbl(specifications)
  missing_by_col_spec_table_sqlite <- get_missing_by_column_tbl(spec_table_sqlite)
  missing_by_col_spec_table_duckdb <- get_missing_by_column_tbl(spec_table_duckdb)

  expect_equal(missing_by_col_spec_table, missing_by_col_spec_table_sqlite)
  expect_equal(missing_by_col_spec_table, missing_by_col_spec_table_duckdb)

  expect_equal(
    missing_by_col_spec_table %>% dplyr::mutate(col_name = as.character(col_name)),
    dplyr::tibble(
      value = rep(0.12, 12),
      col_num = 1:12,
      col_name = colnames(specifications)
    )
  )
})
