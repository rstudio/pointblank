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
#' With any table object, you can produce a summary table that is scoped to
#' the numeric column values. The table produced will have a leading column
#' called `"::stat::"` with labels for each of the nine rows, each corresponding
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
#' @family Table Transformers
#' @section Function ID:
#' 14-1
#' 
#' @export
tt_summary_stats <- function(tbl) {
  
  if (!is_a_table_object(tbl)) {
    stop("The object supplied is not a table", call. = FALSE)
  }

  n_cols <- get_table_total_columns(data = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  col_names <- tbl_info$col_names
  r_col_types <- tbl_info$r_col_types
  
  summary_stats_tbl <- 
    dplyr::tibble(
      `::stat::` = c(
        "min", "p05", "q_1", "med", "q_3",
        "p95", "max", "iqr", "range"
      )
    )
  
  for (i in seq_len(n_cols)) {
    
    if (r_col_types[i] %in% c("integer", "numeric")) {
      
      data_col <- dplyr::select(tbl, col_names[i])
      
      suppressWarnings({
        if (inherits(tbl, "data.frame")) {
          stats_list <- get_df_column_qtile_stats(data_column = data_col)
        } else if (inherits(tbl, "tbl_dbi")) {
          stats_list <- get_dbi_column_qtile_stats(data_column = data_col)
        } else if (inherits(tbl, "tbl_spark")) {
          stats_list <- get_spark_column_qtile_stats(data_column = data_col)
        }
      })
      
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
  
  summary_stats_tbl
}

#' Table Transformer: obtain an information table for string columns
#' 
#' @description
#' With any table object, you can produce an information table that is scoped to
#' string-based columns. The table produced will have a leading column called
#' `"::param::"` with labels for each of the three rows, each corresponding to
#' the following pieces of information pertaining to string length:
#'
#' 1. Mean String Length (`"length_mean"`)
#' 2. Minimum String Length
#' (`"length_min"`)
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
#' @family Table Transformers
#' @section Function ID:
#' 14-2
#' 
#' @export
tt_string_info <- function(tbl) {
  
  if (!is_a_table_object(tbl)) {
    stop("The object supplied is not a table", call. = FALSE)
  }
  
  n_cols <- get_table_total_columns(data = tbl)
  
  tbl_info <- get_tbl_information(tbl = tbl)
  col_names <- tbl_info$col_names
  r_col_types <- tbl_info$r_col_types
  
  string_info_tbl <- 
    dplyr::tibble(
      `::param::` = c("length_mean", "length_min", "length_max")
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

  string_info_tbl
}

#' Table Transformer: get the dimensions of a table
#' 
#' @description
#' With any table object, you can produce an information that contains nothing
#' more than the table's dimensions: the number of rows and the number of
#' columns.
#'
#' The table produced will have two columns and two rows. The first is the
#' `"param"` column with the labels `"rows"` and `"columns"`; the second column,
#' `"value"`, contains the row and column counts.
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' 
#' @return A `tibble` object.
#' 
#' @family Table Transformers
#' @section Function ID:
#' 14-3
#' 
#' @export
tt_tbl_dims <- function(tbl) {
  
  if (!is_a_table_object(tbl)) {
    stop("The object supplied is not a table", call. = FALSE)
  }
  
  n_cols <- get_table_total_columns(data = tbl)
  n_rows <- get_table_total_rows(data = tbl)
  
  dplyr::tibble(
    param = c("rows", "columns"),
    value = as.integer(c(n_rows, n_cols))
  )
}

#' Table Transformer: shift the times of a table
#' 
#' @description
#' With any table object containing date or date-time columns, these values can
#' be precisely shifted with `tt_time_shift()` and specification of the time
#' shift. We can either provide a string with the time shift components and the
#' shift direction or a `difftime` object (which can be created via
#' **lubridate** expressions or by using the [base::difftime()] function).
#' 
#' @param tbl A table object to be used as input for the transformation. This
#'   can be a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#' @param time_shift Either a character string that specifies the time
#'   difference by which all time values in time-based columns will be shifted,
#'   or, a `difftime` object. The character string is constructed in the format
#'   "0y 0m 0d 0H 0M 0S" and individual time components can be omitted (i.e.,
#'   "1y 5d" is a valid specification of shifting time values ahead one year and
#'   five days). Adding a `"-"` at the beginning of the string (e.g., "-2y")
#'   will shift time values back.
#' 
#' @return A `tibble` object.
#' 
#' @family Table Transformers
#' @section Function ID:
#' 14-4
#' 
#' @export
tt_time_shift <- function(tbl,
                          time_shift = "0y 0m 0d 0H 0M 0S") {
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    
    stop(
      "The `tt_time_shift()` function requires the lubridate package:\n",
      "* It can be installed with `install.packages(\"lubridate\")`.",
      call. = FALSE
    )
  }
  
  if (!is_a_table_object(tbl)) {
    stop("The object supplied is not a table", call. = FALSE)
  }

  
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
