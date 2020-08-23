#' Sunder the data, splitting it into 'pass' and 'fail' pieces
#'
#' Validation of the data is one thing but, sometimes, you want to use the best
#' part of the input dataset for something else. The `get_sundered_data()`
#' function works with an agent object that has intel (i.e., post
#' `interrogate()`) and gets either the 'pass' data piece (rows with no failing
#' units across all row-based validation functions), or, the 'fail' data
#' piece (rows with at least one failing unit across the same series of
#' validations). There are some caveats, only those validation steps with
#' no `preconditions` are considered. And, the validation steps used for this
#' must, again, be of the row-based variety (e.g., the `col_vals_*()` functions,
#' and [conjointly()]).
#'
#' @param agent An agent object of class `ptblank_agent`. It should have had
#'   [interrogate()] called on it, such that the validation steps were actually
#'   carried out.
#' @param type The desired piece of data resulting from the splitting. Options
#'   are `"pass"` (the default) and `"fail"`.
#' @param id_cols An optional specification of one or more identifying columns.
#'   When taken together, we can count on this single column or grouping of
#'   columns to distinguish rows.
#' 
#' @return A list of table objects if `type` is `NULL`, or, a table object piece
#'   if a `type` is given.
#' 
#' @examples
#' # Create a series of three validation
#' # steps focus on test row values for
#' # the `small_table` tibble object;
#' # `interrogate()` immediately
#' agent <-
#'   create_agent(tbl = small_table) %>%
#'   col_vals_gt(vars(d), 100) %>%
#'   col_vals_equal(
#'     vars(d), vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_between(
#'     vars(c), left = vars(a), right = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   interrogate()
#' 
#' # Get the sundered data piece that
#' # contains only rows that passed all
#' # validation steps (the default piece)
#' agent %>% get_sundered_data()
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 5-4
#' 
#' @export
get_sundered_data <- function(agent,
                              type = "pass",
                              id_cols = NULL) {

  # Stop function if the agent hasn't
  # yet performed an interrogation
  if (!inherits(agent, "has_intel")) {
    stop("The `agent` has not yet performed an interrogation.", call. = FALSE)
  }
  
  input_tbl <- agent$tbl
  tbl_src <- agent$tbl_src
  
  if (!(tbl_src %in% c("tbl_df", "data.frame")) && is.null(id_cols)) {
    stop("This table needs to have `id_cols` specified, otherwise sundering cannot be done",
         call. = FALSE)
  }
  
  # Get the row count of the input table
  row_count_input_tbl <- 
    input_tbl %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  # Get the row count of the input table after using
  # `dplyr::distinct()`
  row_count_input_tbl_distinct <- 
    input_tbl %>%
    dplyr::distinct() %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  # Get the validation steps that are row-based, and,
  # did not result in evaluation errors
  validation_steps_i <- 
    agent$validation_set %>%
    dplyr::filter(eval_error == FALSE) %>%
    dplyr::filter(assertion_type %in% base::setdiff(row_based_step_fns_vector(), "rows_distinct")) %>%
    dplyr::filter(vapply(preconditions, FUN = is.null, FUN.VALUE = logical(1))) %>%
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
        dplyr::select(dplyr::one_of(by_cols), dplyr::starts_with("pb_is_good_")) %>%
        dplyr::left_join(
          tbl_check_join_r %>%
            dplyr::rename(!!new_col_ii := pb_is_good_) %>%
            dplyr::select(dplyr::one_of(by_cols), dplyr::starts_with("pb_is_good_")),
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
