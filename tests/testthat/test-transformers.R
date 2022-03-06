test_that("the `tt_summary_stats()` function works", {
  
  # Generate a summary table based on `game_revenue`
  summary_stats_tbl <- tt_summary_stats(game_revenue)
  
  # Expect a fixed schema for the summary table
  expect_col_schema_match(
    summary_stats_tbl,
    schema = col_schema(
      .param. = "character",
      item_revenue = "numeric",
      session_duration = "numeric"
    )
  )
  
  # Expect that the summary table has 9 rows
  expect_equal(nrow(summary_stats_tbl), 9)
  
  # Expect a fixed sequence of character values in the first column
  expect_equal(
    summary_stats_tbl$.param.,
    c("min", "p05", "q_1", "med", "q_3", "p95", "max", "iqr", "range")
  )
  
  # Expect no NA values in the numeric columns
  expect_col_vals_not_null(summary_stats_tbl, "item_revenue")
  expect_col_vals_not_null(summary_stats_tbl, "session_duration")
  
  # Generate a summary table from a data.table version of `game_revenue`
  summary_stats_dt <- tt_summary_stats(data.table::as.data.table(game_revenue))
  
  # Expect that a data.table input table will produce the
  # same summary table as the original
  expect_equal(summary_stats_tbl, summary_stats_dt)
  
  # Expect an error if the `tbl` object is not a table
  expect_error(tt_summary_stats(as.matrix(small_table)))
  
  # Expect that infinite values will result in `NA` stats
  tbl_inf <- dplyr::tibble(a = c(Inf, Inf, Inf))
  summary_stats_tbl_na <- tt_summary_stats(tbl_inf)
  
  expect_equal(unique(summary_stats_tbl_na$a), NA_real_)
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
  string_info_dt <- tt_string_info(data.table::as.data.table(game_revenue))
  
  # Expect that a data.table input table will produce the
  # same info table as the original
  expect_equal(string_info_tbl, string_info_dt)
  
  # Expect an error if the `tbl` object is not a table
  expect_error(tt_string_info(as.matrix(small_table)))
})

test_that("the `tt_tbl_dims()` function works", {
  
  # Generate a dimensions table based on `game_revenue`
  dims_tbl <- tt_tbl_dims(game_revenue)
  
  # Expect a fixed schema for the dimensions table
  expect_col_schema_match(
    dims_tbl,
    schema = col_schema(
      .param. = "character",
      value = "integer"
    )
  )
  
  # Expect that the dimensions table has 3 rows
  expect_equal(nrow(dims_tbl), 2)
  
  # Expect a fixed sequence of character values in the first column
  expect_equal(
    dims_tbl$.param.,
    c("rows", "columns")
  )
  
  # Expect no NA values in the numeric column
  expect_col_vals_not_null(dims_tbl, "value")

  # Generate an dimensions table from a data.table version of `game_revenue`
  dims_dt <- tt_tbl_dims(data.table::as.data.table(game_revenue))
  
  # Expect that a data.table input table will produce the
  # same dimensions table as the original
  expect_equal(dims_tbl, dims_dt)
  
  # Expect an error if the `tbl` object is not a table
  expect_error(tt_tbl_dims(as.matrix(small_table)))
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
  
  # Expect that all non-time columns are untouched
  non_time_expr <- ~ !lubridate::is.POSIXct(.x) && !lubridate::is.Date(.x)
  expect_equal(
    game_revenue %>% dplyr::select(where(non_time_expr)),
    game_revenue_1 %>% dplyr::select(where(non_time_expr))
  )
  expect_equal(
    game_revenue %>% dplyr::select(where(non_time_expr)),
    game_revenue_2 %>% dplyr::select(where(non_time_expr))
  )
  
  # Create a `difftime` object of +6 years (should be equivalent
  # to the time-shift spec of "6y")
  difftime_6y <- lubridate::as.difftime(lubridate::duration(6, "years"))
  
  # Use the `difftime` object as a value for the `time_shift` arg
  game_revenue_3 <- tt_time_shift(game_revenue, time_shift = difftime_6y)
  
  # Expect that this time shift using a `difftime` object results in the
  # same table as the one generated by the equivalent time-shift spec
  expect_equal(game_revenue_1, game_revenue_3)
  
  # Create a `difftime` object of -2191 days (should be equivalent
  # to the time-shift spec of "-6y" for the years spanned)
  difftime_6y_back <- lubridate::as.difftime(lubridate::duration(-2191, "days"))
  
  # Use the `difftime` object as a value for the `time_shift` arg
  game_revenue_4 <- tt_time_shift(game_revenue, time_shift = difftime_6y_back)
  
  # Expect that this time shift using a `difftime` object results in the
  # same table as the one generated by the equivalent time-shift spec
  expect_equal(game_revenue_2, game_revenue_4)
  
  # Create a time-spec with a time granularity greater than days; this
  # will effectively truncate the time spec to days
  game_revenue_5 <- tt_time_shift(game_revenue, time_shift = "6y 6H 30M 20S")
  
  # Expect that this time shift using "6y 6H 30M 20S" results in the
  # same table as the one generated by the time-shift spec of "6y"
  expect_equal(game_revenue_1, game_revenue_5)
  
  # Create a time-spec with a time granularity greater than days (truncates
  # the time spec to days) but instead move in the reverse direction
  game_revenue_6 <- tt_time_shift(game_revenue, time_shift = "-6y 6H 30M 20S")
  
  # Expect that this time shift using "6y 6H 30M 20S" results in the
  # same table as the one generated by the time-shift spec of "6y"
  expect_equal(game_revenue_2, game_revenue_6)
  
  # Create a `difftime` object of 2192.25 days (should be rounded internally
  # to a difftime value of 2192 days)
  difftime_6y_2 <- lubridate::as.difftime(lubridate::duration(2192.25, "days"))
  
  # Use the `difftime` object as a value for the `time_shift` arg
  game_revenue_7 <- tt_time_shift(game_revenue, time_shift = difftime_6y_2)
  
  # Expect that this time shift using 2192.25 days results in the
  # same table as the one generated by using 2192 days (and "6y")
  expect_equal(game_revenue_1, game_revenue_7)
  expect_equal(game_revenue_3, game_revenue_7)
  
  # Remove the date-based column from `game_revenue`
  game_revenue_dttm_only <- dplyr::select(game_revenue, -start_day)
  
  # Use a time-spec with a time granularity that includes hours
  game_revenue_dttm_only_1 <- 
    tt_time_shift(game_revenue_dttm_only, time_shift = "-6y 6H")
  
  # Expect that this results in a different time shift to the date-time
  # columns by virtue of the date-based column (`start_day`) not being present;
  # there is a "-6H" difference compared to using "-6y"
  expect_equal(
    unique(game_revenue_dttm_only_1$session_start - game_revenue_2$session_start),
    -6
  )
  
  # Expect an error if the `tbl` object is not a table
  expect_error(tt_time_shift(as.matrix(small_table), time_shift = "6y"))
})

test_that("the `tt_time_slice()` function works", {
  
  # Trim down the size of the `game_revenue` by removing a few columns
  # and only keeping rows where the `session_start` date-time value is
  # before midnight on 2015-01-10
  game_revenue_select <- 
    game_revenue %>%
    dplyr::select(session_start, time, start_day, item_revenue, country) %>%
    dplyr::filter(session_start < lubridate::as_date("2015-01-10"))
  
  # Slice the table into `left` and `right` pieces
  # via the `session_start` column
  game_revenue_select_1_left <- 
    tt_time_slice(
      game_revenue_select,
      time_column = "session_start",
      slice_point = 0.5
    )
  game_revenue_select_1_right <- 
    tt_time_slice(
      game_revenue_select,
      time_column = "session_start",
      slice_point = 0.5,
      keep = "right"
    )
  
  # Expect that putting the slices back together (in the correct order!) will
  # give us the original table
  expect_equal(
    dplyr::bind_rows(game_revenue_select_1_left, game_revenue_select_1_right),
    game_revenue_select
  )
  
  # Slice the table into `left` and `right` pieces
  # via the `time` column
  game_revenue_select_2_left <- 
    tt_time_slice(
      game_revenue_select,
      time_column = "time",
      slice_point = 0.5
    )
  game_revenue_select_2_right <- 
    tt_time_slice(
      game_revenue_select,
      time_column = "time",
      slice_point = 0.5,
      keep = "right"
    )
  
  # Expect that putting the slices back together will give us the original table
  expect_equal(
    dplyr::bind_rows(game_revenue_select_2_left, game_revenue_select_2_right),
    game_revenue_select
  )
  
  # Obtain a 10-row version of the `game_revenue_select` table
  game_revenue_10 <- game_revenue_select[1:10, ]
  
  # Create a tibble that has `time` values that are out of order (original
  # table is ordered by the `time` column)
  game_revenue_10_reorder <-
    dplyr::bind_rows(
      game_revenue_10[2:5, ],
      game_revenue_10[1, ],
      game_revenue_10[7:10, ],
      game_revenue_10[6, ]
    )
  
  # Slice at "2015-01-01 12:00:00" (using the `time` column)
  game_revenue_10_reorder_1_left <-
    tt_time_slice(
      game_revenue_10_reorder,
      time_column = "time",
      slice_point = "2015-01-01 12:00:00"
    )
  game_revenue_10_reorder_1_right <-
    tt_time_slice(
      game_revenue_10_reorder,
      time_column = "time",
      slice_point = "2015-01-01 12:00:00",
      keep = "right"
    )
  
  # Expect that putting the slices back together (in this case) will
  # give us the original table
  expect_equal(
    dplyr::bind_rows(game_revenue_10_reorder_1_left, game_revenue_10_reorder_1_right),
    game_revenue_10_reorder
  )
  
  # Don't expect that the `time` column's values in
  # `game_revenue_10_reorder_1_left` and `game_revenue_10_reorder_1_right`
  # are ordered (they weren't in the input data table)
  expect_col_vals_not_equal(
    game_revenue_10_reorder_1_left %>%
      dplyr::select(time) %>%
      dplyr::bind_cols(
        game_revenue_10_reorder_1_left %>%
          dplyr::select(time_arranged = time) %>%
          dplyr::arrange(time_arranged)
      ),
    columns = vars(time),
    value = vars(time_arranged)
  )
  expect_col_vals_not_equal(
    game_revenue_10_reorder_1_right %>%
      dplyr::select(time) %>%
      dplyr::bind_cols(
        game_revenue_10_reorder_1_right %>%
          dplyr::select(time_arranged = time) %>%
          dplyr::arrange(time_arranged)
      ),
    columns = vars(time),
    value = vars(time_arranged)
  )

  # Slice at "2015-01-01 12:00:00" (using the `time` column) but, this
  # time, order the slices by the `time` column
  game_revenue_10_reorder_2_left <-
    tt_time_slice(
      game_revenue_10_reorder,
      time_column = "time",
      slice_point = "2015-01-01 12:00:00",
      arrange = TRUE
    )
  game_revenue_10_reorder_2_right <-
    tt_time_slice(
      game_revenue_10_reorder,
      time_column = "time",
      slice_point = "2015-01-01 12:00:00",
      keep = "right",
      arrange = TRUE
    )
  
  # Expect that putting the slices back together will give us the
  # original table but only if that input table is arranged
  expect_equal(
    dplyr::bind_rows(game_revenue_10_reorder_2_left, game_revenue_10_reorder_2_right),
    game_revenue_10_reorder %>% dplyr::arrange(time)
  )
  
  # Create a tibble that has some NA values in the `time` column
  game_revenue_10_reorder_na <- game_revenue_10_reorder
  game_revenue_10_reorder_na[c(2, 7, 8), "time"] <- NA
  
  # Slice at "2015-01-01 12:00:00" (using the `time` column)
  game_revenue_10_reorder_na_1_left <-
    tt_time_slice(
      game_revenue_10_reorder_na,
      time_column = "time",
      slice_point = "2015-01-01 12:00:00"
    )
  game_revenue_10_reorder_na_1_right <-
    tt_time_slice(
      game_revenue_10_reorder_na,
      time_column = "time",
      slice_point = "2015-01-01 12:00:00",
      keep = "right"
    )
  
  # Get the date-time values (excluding NAs) for the left (earlier) and
  # right (later) time slices
  left_times <- game_revenue_10_reorder_na$time
  left_times <- left_times[!is.na(left_times)]
  left_times <- left_times[left_times < lubridate::ymd_hms("2015-01-01 12:00:00")]
  
  right_times <- game_revenue_10_reorder_na$time
  right_times <- right_times[!is.na(right_times)]
  right_times <- right_times[right_times >= lubridate::ymd_hms("2015-01-01 12:00:00")]
  
  # Expect that each of the time slices contain just the pre-determined
  # datetime values for each slice
  expect_col_vals_make_set(
    game_revenue_10_reorder_na_1_left,
    columns = vars(time),
    set = left_times
  )
  expect_col_vals_make_set(
    game_revenue_10_reorder_na_1_right,
    columns = vars(time),
    set = right_times
  )
  
  # Expect an error if the `tbl` object is not a table
  expect_error(tt_time_slice(as.matrix(small_table), slice_point = 0.5))
  
  # Expect an error if the slice point isn't between `0` and `1`
  expect_error(tt_time_slice(small_table, slice_point = -0.1))
  expect_error(tt_time_slice(small_table, slice_point = 1.0001))
  
  # Expect that a `slice_point` of `0` will always provide a table with no rows
  expect_equal(
    nrow(tt_time_slice(small_table, time_column = "date_time", slice_point = 0)),
    0
  )
  expect_equal(
    nrow(tt_time_slice(small_table, time_column = "date", slice_point = 0)),
    0
  )

  expect_equal(
    nrow(tt_time_slice(game_revenue, time_column = "session_start", slice_point = 0)),
    0
  )
  expect_equal(
    nrow(tt_time_slice(game_revenue, time_column = "time", slice_point = 0)),
    0
  )
  expect_equal(
    nrow(tt_time_slice(game_revenue, time_column = "start_day", slice_point = 0)),
    0
  )
})
