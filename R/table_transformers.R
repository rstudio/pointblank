#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Table Transformer: obtain a summary stats table for numeric columns
#' 
#' @description
#' 
#' With any table object, you can produce a summary table that is scoped to the
#' numeric column values. The output summary table will have a leading column
#' called `".param."` with labels for each of the nine rows, each corresponding
#' to the following summary statistics:
#' 
#' 1. Minimum (`"min"`)
#' 2. 5th Percentile (`"p05"`)
#' 3. 1st Quartile (`"q_1"`)
#' 4. Median (`"med"`)
#' 5. 3rd Quartile (`"q_3"`)
#' 6. 95th Percentile (`"p95"`)
#' 7. Maximum (`"max"`)
#' 8. Interquartile Range (`"iqr"`)
#' 9. Range (`"range"`)
#' 
#' Only numerical data from the input table will generate columns in the output
#' table. Column names from the input will be used in the output, preserving
#' order as well.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @section Examples:
#' 
#' Get summary statistics for the `game_revenue` dataset that is included in the
#' **pointblank** package.
#' 
#' ```{r}
#' tt_summary_stats(tbl = game_revenue)
#' ```
#' 
#' Table transformers work great in conjunction with validation functions. Let's
#' ensure that the maximum revenue for individual purchases in the
#' `game_revenue` table is less than $150.
#' 
#' ```{r}
#' tt_summary_stats(tbl = game_revenue) %>%
#'   col_vals_lt(
#'     columns = vars(item_revenue),
#'     value = 150,
#'     segments = .param. ~ "max"
#'   )
#' ```
#' 
#' We see data, and not an error, so the validation was successful!
#' 
#' Let's do another: for in-app purchases in the `game_revenue` table, check
#' that the median revenue is somewhere between $8 and $12.
#' 
#' ```{r}
#' game_revenue %>% 
#'   dplyr::filter(item_type == "iap") %>%
#'   tt_summary_stats() %>%
#'   col_vals_between(
#'     columns = vars(item_revenue),
#'     left = 8, right = 12,
#'     segments = .param. ~ "med"
#'   )
#' ```
#'
#' We can get more creative with this transformer. Why not use a transformed
#' table in a validation plan? While performing validations of the
#' `game_revenue` table with an agent we can include the same revenue check as
#' above by using `tt_summary_stats()` in the `preconditions` argument. This
#' transforms the target table into a summary table for the validation step. The
#' final step of the transformation in `preconditions` is a `dplyr::filter()`
#' step that isolates the row of the median statistic.
#' 
#' ```r
#' agent <- 
#'   create_agent(
#'     tbl = game_revenue,
#'     tbl_name = "game_revenue",
#'     label = "`tt_summary_stats()` example.",
#'     actions = action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'       notify_at = 0.35
#'     )
#'   ) %>%
#'   rows_complete() %>%
#'   rows_distinct() %>%
#'   col_vals_between(
#'     columns = vars(item_revenue),
#'     left = 8, right = 12,
#'     preconditions = ~ . %>%
#'       dplyr::filter(item_type == "iap") %>%
#'       tt_summary_stats() %>%
#'       dplyr::filter(.param. == "med")
#'   ) %>%
#'   interrogate()
#' ```
#' 
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report. Take note of the final step
#' (`STEP 3`) as it shows the entry that corresponds to the [col_vals_between()]
#' validation step that uses the summary stats table as its target.
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_tt_summary_stats_1.png")`
#' }
#' }
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-1
#' 
#' @export
tt_summary_stats <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  n_cols <- get_table_total_columns(data = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  col_names <- tbl_info$col_names
  r_col_types <- tbl_info$r_col_types
  
  summary_stats_tbl <- 
    dplyr::tibble(
      `.param.` = c(
        "min", "p05", "q_1", "med", "q_3",
        "p95", "max", "iqr", "range"
      )
    )
  
  for (i in seq_len(n_cols)) {
    
    if (r_col_types[i] %in% c("integer", "numeric")) {
      
      data_col <- dplyr::select(tbl, col_names[i])
      
      # nocov start
      
      suppressWarnings({
        if (inherits(tbl, "data.frame")) {
          stats_list <- get_df_column_qtile_stats(data_column = data_col)
        } else if (inherits(tbl, "tbl_dbi")) {
          stats_list <- get_dbi_column_qtile_stats(data_column = data_col)
        } else if (inherits(tbl, "tbl_spark")) {
          stats_list <- get_spark_column_qtile_stats(data_column = data_col)
        }
      })
      
      # nocov end
      
      stats_col <- 
        tibble::enframe(
          unlist(stats_list),
          name = NULL,
          value = col_names[i]
        )
      
      if (!(any(is.finite(stats_col[, 1, drop = TRUE])))) {
        stats_col[[1]] <- rep(NA_real_, 9)
      }
      
      summary_stats_tbl <- dplyr::bind_cols(summary_stats_tbl, stats_col)
    }
  }

  attr(summary_stats_tbl, which = "tt_type") <- "summary_stats"
  
  summary_stats_tbl
}

#' Table Transformer: obtain a summary table for string columns
#' 
#' @description
#' 
#' With any table object, you can produce a summary table that is scoped to
#' string-based columns. The output summary table will have a leading column
#' called `".param."` with labels for each of the three rows, each corresponding
#' to the following pieces of information pertaining to string length:
#'
#' 1. Mean String Length (`"length_mean"`)
#' 2. Minimum String Length (`"length_min"`)
#' 3. Maximum String Length (`"length_max"`)
#'
#' Only string data from the input table will generate columns in the output
#' table. Column names from the input will be used in the output, preserving
#' order as well.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @section Examples:
#' 
#' Get string information for the string-based columns in the `game_revenue`
#' dataset that is included in the **pointblank** package.
#' 
#' ```{r}
#' tt_string_info(tbl = game_revenue)
#' ```
#' 
#' Ensure that `player_id` and `session_id` values always have the same fixed
#' numbers of characters (`15` and `24`, respectively) throughout the table.
#' 
#' ```{r}
#' tt_string_info(tbl = game_revenue) %>%
#'   col_vals_equal(
#'     columns = vars(player_id),
#'     value = 15
#'   ) %>%
#'   col_vals_equal(
#'     columns = vars(session_id),
#'     value = 24
#'   )
#' ```
#' 
#' We see data, and not an error, so both validations were successful!
#' 
#' Let's use a `tt_string_info()`-transformed table with the
#' [test_col_vals_lte()] to check that the maximum string length in column `f`
#' of the `small_table` dataset is no greater than `4`.
#' 
#' ```{r}
#' tt_string_info(tbl = small_table) %>%
#'   test_col_vals_lte(
#'     columns = vars(f),
#'     value = 4
#'   )
#' ```
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-2
#' 
#' @export
tt_string_info <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  n_cols <- get_table_total_columns(data = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  col_names <- tbl_info$col_names
  r_col_types <- tbl_info$r_col_types
  
  string_info_tbl <- 
    dplyr::tibble(
      .param. = c("length_mean", "length_min", "length_max")
    )
  
  for (i in seq_len(n_cols)) {
    
    if (r_col_types[i] == "character") {
      
      data_col <- dplyr::select(tbl, col_names[i])
      
      suppressWarnings({
        info_list <- get_table_column_nchar_stats(data_column = data_col)
      })
      
      info_col <- 
        tibble::enframe(
          unlist(info_list),
          name = NULL,
          value = col_names[i]
        )
      
      string_info_tbl <- dplyr::bind_cols(string_info_tbl, info_col)
    }
  }
  
  attr(string_info_tbl, which = "tt_type") <- "string_info"

  string_info_tbl
}

#' Table Transformer: get the dimensions of a table
#' 
#' @description
#' 
#' With any table object, you can produce a summary table that contains nothing
#' more than the table's dimensions: the number of rows and the number of
#' columns. The output summary table will have two columns and two rows. The
#' first is the `".param."` column with the labels `"rows"` and `"columns"`; the
#' second column, `"value"`, contains the row and column counts.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @section Examples:
#' 
#' Get the dimensions of the `game_revenue` dataset that is included in the
#' **pointblank** package.
#' 
#' ```{r}
#' tt_tbl_dims(tbl = game_revenue)
#' ```
#' 
#' This output table is useful when a table validation depends on its
#' dimensions. Here, we check that `game_revenue` has at least `1500` rows.
#' 
#' ```{r}
#' tt_tbl_dims(tbl = game_revenue) %>%
#'   dplyr::filter(.param. == "rows") %>%
#'   test_col_vals_gt(
#'     columns = vars(value),
#'     value = 1500
#'   )
#' ```
#' 
#' We can check `small_table` to ensure that number of columns is less than
#' `10`.
#' 
#' ```{r}
#' tt_tbl_dims(tbl = small_table) %>%
#'   dplyr::filter(.param. == "columns") %>%
#'   test_col_vals_lt(
#'     columns = vars(value),
#'     value = 10
#'   )
#' ```
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-3
#' 
#' @export
tt_tbl_dims <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  n_cols <- get_table_total_columns(data = tbl)
  n_rows <- get_table_total_rows(data = tbl)
  
  tbl_dims_tbl <-
    dplyr::tibble(
      .param. = c("rows", "columns"),
      value = as.integer(c(n_rows, n_cols))
    )
  
  attr(tbl_dims_tbl, which = "tt_type") <- "tbl_dims"
  
  tbl_dims_tbl
}

#' Table Transformer: get a table's column names
#' 
#' @description
#' 
#' With any table object, you can produce a summary table that contains table's
#' column names. The output summary table will have two columns and as many rows
#' as there are columns in the input table. The first column is the `".param."`
#' column, which is an integer-based column containing the indices of the
#' columns from the input table. The second column, `"value"`, contains the
#' column names from the input table.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @section Examples:
#' 
#' Get the column names of the `game_revenue` dataset that is included in the
#' **pointblank** package.
#' 
#' ```{r}
#' tt_tbl_colnames(tbl = game_revenue)
#' ```
#' 
#' This output table is useful when you want to validate the column names of the
#' table. Here, we check that `game_revenue` table, included in the
#' **pointblank** package, has certain column names present with
#' [test_col_vals_make_subset()].
#' 
#' ```{r}
#' tt_tbl_colnames(tbl = game_revenue) %>%
#'   test_col_vals_make_subset(
#'     columns = vars(value),
#'     set = c("acquisition", "country")
#'   )
#' ```
#' 
#' We can check to see whether the column names in the `specifications` table
#' are all less than `15` characters in length. For this, we would use the
#' combination of `tt_tbl_colnames()`, then [tt_string_info()], and finally
#' [test_col_vals_lt()] to perform the test.
#' 
#' ```{r}
#' specifications %>%
#'   tt_tbl_colnames() %>%
#'   tt_string_info() %>%
#'   test_col_vals_lt(
#'     columns = vars(value),
#'     value = 15
#'   )
#' ```
#' 
#' This returned `FALSE` and this is because the column name
#' `credit_card_numbers` is 16 characters long.
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-4
#' 
#' @export
tt_tbl_colnames <- function(tbl) {
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  tbl_colnames <- get_table_column_names(data = tbl)
  
  tbl_colnames_tbl <-
    dplyr::tibble(
      .param. = seq_along(tbl_colnames),
      value = tbl_colnames
    )
  
  attr(tbl_colnames_tbl, which = "tt_type") <- "tbl_colnames"
  
  tbl_colnames_tbl
}

#' Table Transformer: shift the times of a table
#' 
#' @description
#' 
#' With any table object containing date or date-time columns, these values can
#' be precisely shifted with `tt_time_shift()` and specification of the time
#' shift. We can either provide a string with the time shift components and the
#' shift direction (like `"-4y 10d"`) or a `difftime` object (which can be
#' created via **lubridate** expressions or by using the [base::difftime()]
#' function).
#' 
#' @details
#' 
#' The `time_shift` specification cannot have a higher time granularity than the
#' least granular time column in the input table. Put in simpler terms, if there
#' are any date-based based columns (or just a single date-based column) then
#' the time shifting can only be in terms of years, months, and days. Using a
#' `time_shift` specification of `"20d 6H"` in the presence of any dates will
#' result in a truncation to `"20d"`. Similarly, a `difftime` object will be
#' altered in the same circumstances, however, the object will resolved to an
#' exact number of days through rounding.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#'   
#' @param time_shift Either a character-based representation that specifies the
#'   time difference by which all time values in time-based columns will be
#'   shifted, or, a `difftime` object. The character string is constructed in
#'   the format `"0y 0m 0d 0H 0M 0S"` and individual time components can be
#'   omitted (i.e., `"1y 5d"` is a valid specification of shifting time values
#'   ahead one year and five days). Adding a `"-"` at the beginning of the
#'   string (e.g., `"-2y"`) will shift time values back.
#' 
#' @return A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
#'   depending on what was provided as `tbl`.
#' 
#' @section Examples:
#' 
#' Let's use the `game_revenue` dataset, included in the **pointblank** package,
#' as the input table for the first demo. It has entries in the first 21 days of
#' 2015 and we'll move all of the date and date-time values to the beginning of
#' 2021 with the `tt_time_shift()` function and the `"6y"` `time_shift`
#' specification.
#' 
#' ```{r}
#' tt_time_shift(
#'   tbl = game_revenue,
#'   time_shift = "6y"
#' )
#' ```
#' 
#' Keeping only the `date_time` and `a`-`f` columns of `small_table`, also
#' included in the package, shift the times back 2 days and 12 hours with the
#' `"-2d 12H"` specification.
#' 
#' ```{r}
#' small_table %>%
#'   dplyr::select(-date) %>%
#'   tt_time_shift("-2d 12H")
#' ```
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-5
#' 
#' @export
tt_time_shift <- function(
    tbl,
    time_shift = "0y 0m 0d 0H 0M 0S"
) {
  
  # nocov start
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    
    stop(
      "The `tt_time_shift()` function requires the lubridate package:\n",
      "* It can be installed with `install.packages(\"lubridate\")`.",
      call. = FALSE
    )
  }
  
  # nocov end
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)

  tbl_info <- get_tbl_information(tbl = tbl)
  r_col_types <- tbl_info$r_col_types
  col_names <- tbl_info$col_names
  
  time_columns <- col_names[r_col_types %in% c("POSIXct", "Date")]
  
  if (length(time_columns) < 1) {
    return(tbl)
  }

  if (inherits(time_shift, "difftime")) {
    
    # If there is a date-based column in the table, we need to ensure
    # that the `difftime` object is rounded to a value of day units
    if ("Date" %in% r_col_types) {
      
      n_days <-
        as.integer(round(as.numeric(time_shift, units = "days"), digits = 0))
      
      tbl <- 
        tbl %>%
        dplyr::mutate(
          dplyr::across(
            .cols = time_columns,
            .fns = ~ lubridate::days(n_days) + .
          )
        )
      
    } else {
      
      tbl <- 
        tbl %>%
        dplyr::mutate(
          dplyr::across(
            .cols = time_columns,
            .fns = ~ time_shift + .
          )
        )
    }
    
  } else {
      
    if (grepl("^-", time_shift)) {
      direction_val <- -1L
      time_shift <- gsub("^-", "", time_shift)
    } else {
      direction_val <- 1L
    }
    
    difference_vec <- unlist(strsplit(time_shift, split = " "))
    
    for (i in seq_along(difference_vec)) {
      
      if (grepl("[0-9]+?(y|m|d|H|M|S)$", difference_vec[i])) {
        
        time_basis <- gsub("[0-9]+?(y|m|d|H|M|S)", "\\1", difference_vec[i])
        time_value <- 
          as.integer(gsub("([0-9]+?)(y|m|d|H|M|S)", "\\1", difference_vec[i]))
        
        # If the time value is zero, then proceed to the next iteration
        if (time_value == 0) next
        
        # Don't shift times by hours, minutes, or seconds if there are any
        # time columns that are date-based (this will either fail or yield
        # undesirable time values in date-based columns)
        if (time_basis %in% c("H", "M", "S") && "Date" %in% r_col_types) next
        
        fn_time <-
          switch(
            time_basis,
            y = lubridate::years,
            m = lubridate::dmonths,
            d = lubridate::days,
            H = lubridate::hours,
            M = lubridate::minutes,
            S = lubridate::seconds
          )
        
        # Apply the time change for the particular time basis to all columns
        tbl <- 
          tbl %>%
          dplyr::mutate(
            dplyr::across(
              .cols = time_columns,
              .fns = ~ fn_time(time_value * direction_val) + .)
          )
      }
    }
  }
  
  tbl
}

#' Table Transformer: slice a table with a slice point on a time column
#' 
#' @description
#' 
#' With any table object containing date, date-time columns, or a mixture
#' thereof, any one of those columns can be used to effectively slice the data
#' table in two with a `slice_point`: and you get to choose which of those
#' slices you want to keep. The slice point can be defined in several ways. One
#' method involves using a decimal value between `0` and `1`, which defines the
#' slice point as the time instant somewhere between the earliest time value (at
#' `0`) and the latest time value (at `1`). Another way of defining the slice
#' point is by supplying a time value, and the following input types are
#' accepted: (1) an ISO 8601 formatted time string (as a date or a date-time),
#' (2) a `POSIXct` time, or (3) a `Date` object.
#' 
#' @details
#' 
#' There is the option to `arrange` the table by the date or date-time values in
#' the `time_column`. This ordering is always done in an ascending manner. Any
#' `NA`/`NULL` values in the `time_column` will result in the corresponding rows
#' can being removed (no matter which slice is retained).
#'  
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#'   
#' @param time_column The time-based column that will be used as a basis for the
#'   slicing. If no time column is provided then the first one found will be
#'   used.
#'   
#' @param slice_point The location on the `time_column` where the slicing will
#'   occur. This can either be a decimal value from `0` to `1`, an ISO 8601
#'   formatted time string (as a date or a date-time), a `POSIXct` time, or a
#'   `Date` object.
#'   
#' @param keep Which slice should be kept? The `"left"` side (the default)
#'   contains data rows that are earlier than the `slice_point` and the
#'   `"right"` side will have rows that are later.
#'   
#' @param arrange Should the slice be arranged by the `time_column`? This may be
#'   useful if the input `tbl` isn't ordered by the `time_column`. By default,
#'   this is `FALSE` and the original ordering is retained.
#' 
#' @return A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
#'   depending on what was provided as `tbl`.
#' 
#' @section Examples:
#' 
#' Let's use the `game_revenue` dataset, included in the **pointblank** package,
#' as the input table for the first demo. It has entries in the first 21 days of
#' 2015 and we'll elect to get all of the records where the `time` values are
#' strictly for the first 15 days of 2015. The `keep` argument has a default of
#' `"left"` so all rows where the `time` column is less than
#' `"2015-01-16 00:00:00"` will be kept.
#' 
#' ```{r}
#' tt_time_slice(
#'   tbl = game_revenue,
#'   time_column = "time",
#'   slice_point = "2015-01-16"
#' )
#' ```
#' 
#' Omit the first 25% of records from `small_table`, also included in the
#' package, with a fractional `slice_point` of `0.25` on the basis of a timeline
#' that begins at `2016-01-04 11:00:00` and ends at `2016-01-30 11:23:00`.
#' 
#' ```{r}
#' small_table %>%
#'   tt_time_slice(
#'     slice_point = 0.25,
#'     keep = "right"
#'   )
#' ```
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-6
#' 
#' @export
tt_time_slice <- function(
    tbl,
    time_column = NULL,
    slice_point = 0,
    keep = c("left", "right"),
    arrange = FALSE
) {
  
  # nocov start
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    
    stop(
      "The `tt_time_shift()` function requires the lubridate package:\n",
      "* It can be installed with `install.packages(\"lubridate\")`.",
      call. = FALSE
    )
  }
  
  # nocov end
  
  keep <- match.arg(keep)
  
  # Determine whether the `tbl` object is acceptable here
  check_is_a_table_object(tbl = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  r_col_types <- tbl_info$r_col_types
  col_names <- tbl_info$col_names
  
  all_time_columns <- col_names[r_col_types %in% c("POSIXct", "Date")]
  
  if (length(all_time_columns) < 1) {
    return(tbl)
  }
  
  if (is.null(time_column)) {
    time_column <- all_time_columns[1]
  }
  
  col_sym <- rlang::sym(time_column)
  
  time_bounds <-
    tbl %>%
    dplyr::select(!!col_sym) %>%
    dplyr::summarize_all(
      .funs = list(
        ~ min(., na.rm = TRUE),
        ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::collect() %>%
    as.list()
  
  if (is.numeric(slice_point)) {
    
    if (slice_point < 0 || slice_point > 1) {
      stop(
        "When provided as a number, `slice_point` must be between 0 and 1",
        call. = FALSE
      )
    }
    
    time_range_s <-
      as.numeric(
        difftime(
          time1 = time_bounds$max,
          time2 = time_bounds$min,
          units = "secs"
        )
      )
    
    time_slice_instant <-
      time_bounds$min +
      lubridate::seconds(time_range_s * slice_point)
    
  } else if (inherits(slice_point, "character")) {
    
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", slice_point[1])) {
      
      time_slice_instant <-
        lubridate::ymd_hms(paste(slice_point[1], "00:00:00"))
      
    } else if (
      grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}(T| )[0-9]{2}:[0-9]{2}:[0-9]{2}$",
        slice_point[1]
        )) {
      
      time_slice_instant <- lubridate::ymd_hms(slice_point[1])
      
    } else {
      
      stop(
        "The `slice_point` must be a date or date-time",
        call. = FALSE
        )
    }
    
  } else if (inherits(slice_point, "POSIXct")) {
    
    time_slice_instant <- slice_point
    
  } else if (inherits(slice_point, "Date")) {
    
    time_slice_instant <- slice_point
  }
  
  # Optionally arrange rows by the time column
  if (arrange) {
    tbl <- dplyr::arrange(tbl, !!col_sym)
  }
  
  # Perform the filtering of data either on the left or right of
  # the `time_slice_instant`
  if (keep == "left") {
    tbl <- dplyr::filter(tbl, !!col_sym < time_slice_instant)
  } else {
    tbl <- dplyr::filter(tbl, !!col_sym >= time_slice_instant)
  }
  
  tbl
}

#' Get a parameter value from a summary table
#' 
#' @description
#' 
#' The `get_tt_param()` function can help you to obtain a single parameter value
#' from a summary table generated by the `tt_*()` functions
#' [tt_summary_stats()], [tt_string_info()], [tt_tbl_dims()], or
#' [tt_tbl_colnames()]. The following parameters are to be used depending on the
#' input `tbl`:
#'
#' - from [tt_summary_stats()]: `"min"`, `"p05"`, `"q_1"`, `"med"`, `"q_3"`,
#' `"p95"`, `"max"`, `"iqr"`, `"range"`
#' - from [tt_string_info()]: `"length_mean"`, `"length_min"`, `"length_max"`
#' - from [tt_tbl_dims()]: `"rows"`, `"columns"`
#' - from [tt_tbl_colnames()]: any integer present in the `.param.` column
#' 
#' The [tt_summary_stats()] and [tt_string_info()] functions will generate
#' summary tables with columns that mirror the numeric and character columns
#' in their input tables, respectively. For that reason, a column name must be
#' supplied to the `column` argument in `get_tt_param()`.
#' 
#' @param tbl A summary table generated by either of the [tt_summary_stats()],
#'   [tt_string_info()], [tt_tbl_dims()], or [tt_tbl_colnames()] functions.
#'   
#' @param param The parameter name associated to the value that is to be gotten.
#'   These parameter names are always available in the first column (`.param.`)
#'   of a summary table obtained by [tt_summary_stats()], [tt_string_info()],
#'   [tt_tbl_dims()], or [tt_tbl_colnames()].
#'   
#' @param column The column in the summary table for which the data value should
#'   be obtained. This must be supplied for summary tables generated by
#'   [tt_summary_stats()] and [tt_string_info()] (the [tt_tbl_dims()] and
#'   [tt_tbl_colnames()] functions will always generate a two-column summary
#'   table).
#'   
#' @return A scalar value.
#'   
#' @section Examples:
#' 
#' Get summary statistics for the first quarter of the `game_revenue` dataset
#' that's included in the **pointblank** package.
#' 
#' ```{r}
#' stats_tbl <- 
#'   game_revenue %>%
#'   tt_time_slice(slice_point = 0.25) %>%
#'   tt_summary_stats()
#'   
#' stats_tbl
#' ```
#' 
#' Sometimes you need a single value from the table generated by the
#' [tt_summary_stats()] function. For that, we can use the `get_tt_param()`
#' function. So if we wanted to test whether the maximum session duration during
#' the rest of the time period (the remaining 0.75) is never higher than that of
#' the first quarter of the year, we can supply a value from `stats_tbl` to
#' [test_col_vals_lte()]:
#' 
#' ```{r}
#' game_revenue %>%
#'   tt_time_slice(
#'     slice_point = 0.25,
#'     keep = "right"
#'   ) %>%
#'   test_col_vals_lte(
#'     columns = vars(session_duration), 
#'     value = get_tt_param(
#'       tbl = stats_tbl,
#'       param = "max",
#'       column = "session_duration"
#'     )
#'   )
#' ```
#' 
#' @family Table Transformers
#' @section Function ID:
#' 12-7
#' 
#' @export
get_tt_param <- function(
    tbl,
    param,
    column = NULL
) {
  
  # Stop function if the `tt_type` attribute isn't present
  # in the table object
  tt_type <- attr(tbl, which = "tt_type", exact = TRUE)
  
  # Stop function if `param` isn't a vector of length 1
  if (length(param) != 1) {
    
    stop(
      "The value for `param` must be a vector of length 1.",
      call. = FALSE
    )
  }
  
  if (is.null(tt_type)) {
    
    stop(
      "The summary table provided in `tbl` wasn't produced in pointblank:\n",
      "* use either `tt_summary_stats()`, `tt_string_info()`, ",
      "`tt_tbl_dims()`, or `tt_tbl_colnames()` to create a summary table ",
      "based on an input table.",
      call. = FALSE
    )
  }
  
  if (tt_type %in% c("summary_stats", "string_info")) {
    
    # Stop function if `column` is not provided
    if (is.null(column)) {
      
      stop(
        "When getting a value from a ", gsub("_", " ", tt_type), " table, ",
        "a `column` name must be provided.",
        call. = FALSE
      )
    }
    
    # Stop function if `column` isn't a character vector of length 1
    if (!is.character(column) && length(column) != 1) {
      
      stop(
        "The value for `column` must be a character vector of length 1.",
        call. = FALSE
      )
    }
    
    tt_tbl_colnames <- base::setdiff(names(tbl), ".param.") 
    
    # Stop function if `column` is not one of the summary table's columns
    if (!(column %in% tt_tbl_colnames)) {
      
      stop(
        "The provided `column` must match a column name in the summary table.",
        call. = FALSE
      )
    }
    
    if (tt_type == "summary_stats") {
      
      summary_stats_params <-
        c("min", "p05", "q_1", "med", "q_3", "p95", "max", "iqr", "range")
      
      if (!(param %in% summary_stats_params)) {
        
        stop(
          "The `param` value must be a param name for a summary stats table.",
          call. = FALSE
        )
      }
      
    } else {
      
      string_length_params <- c("length_mean", "length_min", "length_max")
      
      if (!(param %in% string_length_params)) {
        
        stop(
          "The `param` value must be a param name for a string lengths table.",
          call. = FALSE
        )
      }
    }
    
    # Obtain the value from the `tbl` through a `select()`, `filter()`, `pull()`
    param_value <-
      tbl %>%
      dplyr::select(.param., .env$column) %>%
      dplyr::filter(.param. == .env$param) %>%
      dplyr::pull(.env$column)
    
  } else if (tt_type == "tbl_dims") {
    
    if (!(param %in% c("rows", "columns"))) {
      
      stop(
        "The `param` value must be a param name for a dimensions table.",
        call. = FALSE
      )
    }
    
    # Obtain the value from the `tbl` through a `filter()` and `pull()`
    param_value <-
      tbl %>%
      dplyr::filter(.param. == .env$param) %>%
      dplyr::pull(value)
  
  } else if (tt_type == "tbl_colnames") {
    
    # Stop function if the `param` value provided isn't in the range of
    # column indices present in `tbl`
    if (!(param %in% tbl$.param.)) {
      
      stop(
        "The column index given as `param` isn't present in the summary table",
        call. = FALSE
      )
    }
    
    # Obtain the value from the `tbl` through a `filter()` and `pull()`
    param_value <-
      tbl %>%
      dplyr::filter(.param. == as.integer(.env$param)) %>%
      dplyr::pull(value)
  }
  
  param_value
}
