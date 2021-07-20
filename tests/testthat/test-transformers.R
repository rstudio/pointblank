library(data.table)

test_that("the `tt_summary_stats()` function works", {
  
  # Generate a summary table based on `game_revenue`
  summary_stats_tbl <- tt_summary_stats(game_revenue)
  
  # Expect a fixed schema for the summary table
  expect_col_schema_match(
    summary_stats_tbl,
    schema = col_schema(
      .stat. = "character",
      item_revenue = "numeric",
      session_duration = "numeric"
    )
  )
  
  # Expect that the summary table has 9 rows
  expect_equal(nrow(summary_stats_tbl), 9)
  
  # Expect a fixed sequence of character values in the first column
  expect_equal(
    summary_stats_tbl$.stat.,
    c("min", "p05", "q_1", "med", "q_3", "p95", "max", "iqr", "range")
  )
  
  # Expect no NA values in the numeric columns
  expect_col_vals_not_null(summary_stats_tbl, "item_revenue")
  expect_col_vals_not_null(summary_stats_tbl, "session_duration")
  
  # Generate a summary table from a data.table version of `game_revenue`
  summary_stats_dt <- tt_summary_stats(data.table(game_revenue))
  
  # Expect that a data.table input table will produce the
  # same summary table as the original
  expect_equal(summary_stats_tbl, summary_stats_dt)
})

test_that("the `tt_string_info()` function works", {
  
  # Generate a string info table based on `game_revenue`
  string_info_tbl <- tt_string_info(game_revenue)
  
  # Expect a fixed schema for the info table
  expect_col_schema_match(
    string_info_tbl,
    schema = col_schema(
      .param. = "character",
      player_id = "numeric",
      session_id = "numeric",
      item_type = "numeric",
      item_name = "numeric",
      acquisition = "numeric",
      country = "numeric"
    )
  )
  
  # Expect that the info table has 3 rows
  expect_equal(nrow(string_info_tbl), 3)
  
  # Expect a fixed sequence of character values in the first column
  expect_equal(
    string_info_tbl$.param.,
    c("length_mean", "length_min", "length_max")
  )
  
  # Expect no NA values in the numeric columns
  expect_col_vals_not_null(string_info_tbl, "player_id")
  expect_col_vals_not_null(string_info_tbl, "session_id")
  expect_col_vals_not_null(string_info_tbl, "item_type")
  expect_col_vals_not_null(string_info_tbl, "item_name")
  expect_col_vals_not_null(string_info_tbl, "acquisition")
  expect_col_vals_not_null(string_info_tbl, "country")
  
  # Generate an info table from a data.table version of `game_revenue`
  string_info_dt <- tt_string_info(data.table(game_revenue))
  
  # Expect that a data.table input table will produce the
  # same info table as the original
  expect_equal(string_info_tbl, string_info_dt)
})

test_that("the `tt_tbl_dims()` function works", {
  
  # Generate a dimensions table based on `game_revenue`
  dims_tbl <- tt_tbl_dims(game_revenue)
  
  # Expect a fixed schema for the dimensions table
  expect_col_schema_match(
    dims_tbl,
    schema = col_schema(
      dim = "character",
      value = "integer"
    )
  )
  
  # Expect that the dimensions table has 3 rows
  expect_equal(nrow(dims_tbl), 2)
  
  # Expect a fixed sequence of character values in the first column
  expect_equal(
    dims_tbl$dim,
    c("rows", "columns")
  )
  
  # Expect no NA values in the numeric column
  expect_col_vals_not_null(dims_tbl, "value")

  # Generate an dimensions table from a data.table version of `game_revenue`
  dims_dt <- tt_tbl_dims(data.table(game_revenue))
  
  # Expect that a data.table input table will produce the
  # same dimensions table as the original
  expect_equal(dims_tbl, dims_dt)
})

test_that("the `tt_time_shift()` function works", {
  
  # Shift the `game_revenue` table forward 6 years using
  # a character string for the `time_shift` spec
  game_revenue_1 <- tt_time_shift(game_revenue, time_shift = "6y")
  
  # Expect the schema for the revised table matches that
  # of the input table
  expect_col_schema_match(
    game_revenue_1,
    schema = col_schema(.tbl = game_revenue)
  )
  
  # Expect that the number of rows in the revised table
  # matches that of the input table
  expect_equal(nrow(game_revenue), nrow(game_revenue_1))
  
  # Expect that time values in the revised table are all
  # six years (2192 days in this case) ahead of the input table
  expect_equal(
    unique(game_revenue_1$session_start - game_revenue$session_start),
    2192
  )
  expect_equal(
    unique(game_revenue_1$time - game_revenue$time),
    2192
  )
  expect_equal(
    unique(game_revenue_1$start_day - game_revenue$start_day),
    2192
  )
  
  # Shift the `game_revenue` table *back* 6 years using
  # a character string for the `time_shift` spec
  game_revenue_2 <- tt_time_shift(game_revenue, time_shift = "-6y")
  
  # Expect the schema for the revised table matches that
  # of the input table
  expect_col_schema_match(
    game_revenue_2,
    schema = col_schema(.tbl = game_revenue)
  )
  
  # Expect that the number of rows in the revised table
  # matches that of the input table
  expect_equal(nrow(game_revenue), nrow(game_revenue_2))
  
  # Expect that time values in the revised table are all
  # six years (2191 days in this case) behind the input table
  expect_equal(
    unique(game_revenue$session_start - game_revenue_2$session_start),
    2191
  )
  expect_equal(
    unique(game_revenue$time - game_revenue_2$time),
    2191
  )
  expect_equal(
    unique(game_revenue$start_day - game_revenue_2$start_day),
    2191
  )
})
