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


#' Sunder the data, splitting it into 'pass' and 'fail' pieces
#'
#' @description
#' Validation of the data is one thing but, sometimes, you want to use the best
#' part of the input dataset for something else. The `get_sundered_data()`
#' function works with an agent object that has intel (i.e., post
#' `interrogate()`) and gets either the 'pass' data piece (rows with no failing
#' test units across all row-based validation functions), or, the 'fail' data
#' piece (rows with at least one failing test unit across the same series of
#' validations). As a final option, we can have emit all the data with a new
#' column (called `.pb_combined`) which labels each row as passing or failing
#' across validation steps. These labels are `"pass"` and `"fail"` by default
#' but their values can be easily customized.
#' 
#' @details
#' There are some caveats to sundering. The validation steps considered for this
#' splitting has to be of the row-based variety (e.g., the `col_vals_*()`
#' functions or [conjointly()], but not `rows_distinct()`). Furthermore,
#' validation steps that experienced evaluation issues during interrogation are
#' not considered, and, validation steps where `active = FALSE` will be
#' disregarded. The collection of validation steps that fulfill the above
#' requirements for sundering are termed in-consideration validation steps.
#'
#' If using any `preconditions` for validation steps, we must ensure that all
#' in-consideration validation steps use the same specified `preconditions`
#' function. Put another way, we cannot split the target table using a
#' collection of in-consideration validation steps that use different forms of
#' the input table.
#'
#' @param agent An agent object of class `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were actually
#'   carried out.
#' @param type The desired piece of data resulting from the splitting. Options
#'   for returning a single table are `"pass"` (the default) and `"fail"`. Each
#'   of these options return a single table with, in the `"pass"` case, only the
#'   rows that passed across all validation steps (i.e., had no failing test
#'   units in any part of a row for any validation step), or, the complementary
#'   set of rows in the `"fail"` case. Providing `NULL` returns both of the
#'   split data tables in a list (with the names of `"pass"` and `"fail"`). The
#'   option `"combined"` applies a categorical (pass/fail) label (settable in
#'   the `pass_fail` argument) in a new `.pb_combined` flag column. For this
#'   case the ordering of rows is fully retained from the input table.
#' @param pass_fail A vector for encoding the flag column with 'pass' and 'fail'
#'   values when `type = "combined"`. The default is `c("pass", "fail")` but
#'   other options could be `c(TRUE, FALSE)`, `c(1, 0)`, or `c(1L, 0L)`.
#' @param id_cols An optional specification of one or more identifying columns.
#'   When taken together, we can count on this single column or grouping of
#'   columns to distinguish rows. If the table undergoing validation is not a
#'   data frame or tibble, then columns need to be specified for `id_cols`.
#' @return A list of table objects if `type` is `NULL`, or, a single table if a
#'   `type` is given.
#' 
#' @section Demos:
#' 
#' Create a series of two validation steps focused on testing row values for
#' part of the `small_table` object. Then, `interrogate()`.
#' 
#' ```{r}
#' agent <-
#'   create_agent(
#'     tbl = small_table %>%
#'       dplyr::select(a:f),
#'     label = "`get_sundered_data()`"
#'   ) %>%
#'   col_vals_gt(vars(d), value = 1000) %>%
#'   col_vals_between(
#'     vars(c),
#'     left = vars(a), right = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   interrogate()
#' ```
#' 
#' Get the sundered data piece that contains only rows that passed both
#' validation steps (the default piece). This yields 5 of 13 total rows.
#' 
#' ```{r}
#' agent %>% get_sundered_data()
#' ```
#' 
#' Get the complementary data piece: all of those rows that failed either of the
#' two validation steps. This yields 8 of 13 total rows.
#' 
#' ```{r}
#' agent %>% get_sundered_data(type = "fail")
#' ```
#'   
#' We can get all of the input data returned with a flag column (called
#' `.pb_combined`). This is done by using `type = "combined"` and that rightmost
#' column will contain `"pass"` and `"fail"` values.
#' 
#' ```{r}
#' agent %>% get_sundered_data(type = "combined")
#' ```
#' 
#' We can change the `"pass"` or `"fail"` text values to another type of coding
#' with the `pass_fail` argument. One possibility is `TRUE`/`FALSE`.
#' 
#' ```{r}
#' agent %>%
#'   get_sundered_data(
#'     type = "combined",
#'     pass_fail = c(TRUE, FALSE)
#'   )
#' ```
#'
#' ...and using `0` and `1` might be worthwhile in some situations.
#' 
#' ```{r}
#' agent %>%
#'   get_sundered_data(
#'     type = "combined",
#'     pass_fail = 0:1
#'   )
#' ```
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-3
#' 
#' @export
get_sundered_data <- function(
    agent,
    type = c("pass", "fail", "combined"),
    pass_fail = c("pass", "fail"),
    id_cols = NULL
) {

  # Match to one of the three choices (`pass`, `fail`, `combined`)
  # while still allowing for the NULL optiona
  if (!is.null(type)) {
    type <- match.arg(type)
  }
  
  # Stop function if the agent hasn't
  # yet performed an interrogation
  if (!inherits(agent, "has_intel")) {
    
    stop(
      "The `agent` has not yet performed an interrogation.",
      call. = FALSE
    )
  }
  
  input_tbl <- agent$tbl
  tbl_src <- agent$tbl_src

  if (!(tbl_src %in% c("tbl_df", "data.frame")) && is.null(id_cols)) {
    
    stop(
      "This table needs to have `id_cols` specified, otherwise ",
      "sundering cannot be done",
      call. = FALSE
    )
  }
  
  # Get the row count of the input table
  row_count_input_tbl <- 
    input_tbl %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  # Keep only the validation steps that:
  # - did not result in evaluation errors
  # - are row-based (not including `rows_distinct()`)
  # - are `active`
  validation_set_prefiltered <- 
    agent$validation_set %>%
    dplyr::filter(eval_error == FALSE) %>%
    dplyr::filter(
      assertion_type %in%
        base::setdiff(row_based_validation_fns_vec(), "rows_distinct")
    ) %>%
    dplyr::filter(active == TRUE)
  
  # Get a character vector of preconditions
  preconditions_vec <- 
    vapply(
      validation_set_prefiltered[["preconditions"]],
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        paste(as.character(x), collapse = "")
      }
    )
  
  if (!all(preconditions_vec == preconditions_vec[1])) {
    
    stop(
      "Using `get_sundered_data()` requires that either:\n",
      "* No `preconditions` are used, or\n",
      "* All specified `preconditions` are the same",
      call. = FALSE
    )
  }
  
  # Obtain the validation steps that are to be used for sundering
  validation_steps_i <- 
    validation_set_prefiltered %>%
    dplyr::pull(i)
    
  if (length(validation_steps_i) == 0) {
    
    if (!is.null(type) && type == "pass") {
      return(input_tbl)
    }
    
    if (!is.null(type) && type == "fail") {
      return(input_tbl[0, ])
    }
    
    if (is.null(type)) {
      return(
        list(
          pass = input_tbl,
          fail = input_tbl[0, ]
        )
      )
    }
  }
  
  # Get the stored `tbl_check` objects for `validation_steps_i`
  tbl_check_obj <-
    agent$validation_set %>%
    dplyr::filter(i %in% validation_steps_i) %>%
    dplyr::pull(tbl_checked)

  for (i in seq(tbl_check_obj)) {
    
    if (i == min(seq(tbl_check_obj))) {
      
      new_col_i <- paste0("pb_is_good_", i)
      
      tbl_check_join <- 
        tbl_check_obj[[i]][[1]] %>%
        dplyr::rename(!!new_col_i := pb_is_good_)
      
      if (agent$tbl_src %in% c("tbl_df", "data.frame")) {
        
        tbl_check_join <- 
          tbl_check_join %>%
          tibble::rowid_to_column(var = "__pb_rowid__")
      }
    }
    
    new_col_ii <- paste0("pb_is_good_", i + 1)
    
    if (length(seq(tbl_check_obj)) == 1) break
    
    tbl_check_join_r <- tbl_check_obj[[i + 1]][[1]]
    
    if (!(agent$tbl_src %in% c("tbl_df", "data.frame"))) {
      
      by_cols <- id_cols
      
      tbl_check_join <- 
        tbl_check_join %>%
        dplyr::select(
          dplyr::one_of(by_cols), dplyr::starts_with("pb_is_good_")
        ) %>%
        dplyr::left_join(
          tbl_check_join_r %>%
            dplyr::rename(!!new_col_ii := pb_is_good_) %>%
            dplyr::select(
              dplyr::one_of(by_cols), dplyr::starts_with("pb_is_good_")
            ),
          by = by_cols
        ) %>%
        dplyr::left_join(
          tbl_check_join %>% 
            dplyr::select(-dplyr::starts_with("pb_is_good_")),
          by = by_cols
        )
      
    } else if (agent$tbl_src %in% c("tbl_df", "data.frame")) {
      
      tbl_check_join_r <- 
        tbl_check_join_r %>%
        tibble::rowid_to_column(var = "__pb_rowid__")
      
      by_cols <- c("__pb_rowid__", agent$col_names)
      
      tbl_check_join <- 
        tbl_check_join %>%
        dplyr::left_join(
          tbl_check_join_r %>%
            dplyr::rename(!!new_col_ii := pb_is_good_),
          by = by_cols
        )
    }

    if (i == (max(seq(tbl_check_obj)) - 1)) break
  }

  columns_str_vec <- paste0("pb_is_good_", seq(tbl_check_obj))
  columns_str_add <- paste0("pb_is_good_", seq(tbl_check_obj), collapse = " + ")
  validation_n <- length(seq(tbl_check_obj))
  
  tbl_check_join <- 
    tbl_check_join %>%
    dplyr::mutate(pb_is_good_ = !!rlang::parse_expr(columns_str_add)) %>%
    dplyr::select(-dplyr::one_of(columns_str_vec)) %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      pb_is_good_ == validation_n ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::select(-dplyr::starts_with("__pb_rowid__"))

  if (!is.null(type) && type == "pass") {
    
    sundered_tbl_pass <- 
      tbl_check_join %>%
      dplyr::filter(pb_is_good_ == 1) %>%
      dplyr::select(-pb_is_good_)
    
    return(sundered_tbl_pass)
  }
  
  if (!is.null(type) && type == "fail") {
    
    sundered_tbl_fail <- 
      tbl_check_join %>%
      dplyr::filter(pb_is_good_ == 0) %>%
      dplyr::select(-pb_is_good_)
    
    return(sundered_tbl_fail)
  }
  
  if (!is.null(type) && type == "combined") {
    
    sundered_tbl_combined <- 
      tbl_check_join %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        pb_is_good_ ~ pass_fail[1],
        !pb_is_good_ ~ pass_fail[2],
        TRUE ~ pass_fail[1]
      )) %>%
      dplyr::rename(`.pb_combined` = pb_is_good_)
    
    return(sundered_tbl_combined)
  }
  
  if (is.null(type)) {
    
    sundered_tbl_list <-
      list(
        pass = tbl_check_join %>%
          dplyr::filter(pb_is_good_ == 1) %>%
          dplyr::select(-pb_is_good_),
        fail = tbl_check_join %>%
          dplyr::filter(pb_is_good_ == 0) %>%
          dplyr::select(-pb_is_good_)
      )
    
    return(sundered_tbl_list)
  }
}
