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
#' 1. Mean String Length (`"length_mean"`) 2. Minimum String Length
#' (`"length_min"`) 3. Maximum String Length (`"length_max"`)
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
