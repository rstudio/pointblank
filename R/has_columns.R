#------------------------------------------------------------------------------#
# 
#                 _         _    _      _                _    
#                (_)       | |  | |    | |              | |   
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |                                                        
#  |_|                                                        
#  
#  This file is part of the 'rstudio/pointblank' project.
#  
#  Copyright (c) 2017-2023 pointblank authors
#  
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
# 
#------------------------------------------------------------------------------#


#' Determine if one or more columns exist in a table
#' 
#' @description
#' 
#' This utility function can help you easily determine whether a column of a
#' specified name is present in a table object. This function works well enough
#' on a table object but it can also be used as part of a formula in any
#' validation function's `active` argument. Using `active = ~ . %>%
#' has_columns(column_1)` means that the validation step will be inactive if
#' the target table doesn't contain a column named `column_1`. We can also use
#' multiple columns in `c()`, so having `active = ~ . %>%
#' has_columns(c(column_1, column_2))` in a validation step will make it
#' inactive at [interrogate()] time unless the columns `column_1` and `column_2`
#' are both present.
#' 
#' @param x *A data table*
#' 
#'   `obj:<tbl_*>` // **required**
#' 
#'   The input table. This can be a data frame, tibble, a `tbl_dbi` object, or a
#'   `tbl_spark` object.
#' 
#' @param columns *The target columns*
#'   
#'   `<tidy-select>` // *required*
#' 
#'   One or more columns or column-selecting expressions. Each element is
#'   checked for a match in the table `x`.
#'   
#' @return A length-1 logical vector.
#' 
#' @section Examples:
#' 
#' The `small_table` dataset in the package has the columns `date_time`, `date`,
#' and the `a` through `f` columns.
#' 
#' ```{r}
#' small_table
#' ```
#' 
#' With `has_columns()` we can check for column existence by using it directly
#' on the table.
#' 
#' ```r
#' small_table %>% has_columns(columns = date)
#' ```
#' 
#' ```
#' ## [1] TRUE
#' ```
#' 
#' Multiple column names can be supplied. The following is `TRUE` because both
#' columns are present in `small_table`.
#' 
#' ```r
#' small_table %>% has_columns(columns = c(a, b))
#' ```
#' 
#' ```
#' ## [1] TRUE
#' ```
#' 
#' It's possible to use a tidyselect helper as well:
#' 
#' ```r
#' small_table %>% has_columns(columns = c(a, starts_with("b")))
#' ```
#' 
#' ```
#' ## [1] TRUE
#' ```
#' 
#' Because column `h` isn't present, this returns `FALSE` (all specified columns
#' need to be present to obtain `TRUE`).
#' 
#' ```r
#' small_table %>% has_columns(columns = c(a, h))
#' ```
#' 
#' ```
#' ## [1] FALSE
#' ```
#' 
#' The same holds in the case of tidyselect helpers. Because no columns start
#' with `"h"`, including `starts_with("h")` returns `FALSE` for the entire
#' check.
#' 
#' ```r
#' small_table %>% has_columns(columns = starts_with("h"))
#' small_table %>% has_columns(columns = c(a, starts_with("h")))
#' ```
#' 
#' ```
#' ## [1] FALSE
#' ## [1] FALSE
#' ```
#' 
#' The `has_columns()` function can be useful in expressions that involve the
#' target table, especially if it is uncertain that the table will contain a
#' column that's involved in a validation.
#' 
#' In the following agent-based validation, the first two steps will be 'active'
#' because all columns checked for in the expressions are present. The third
#' step becomes inactive because column `j` isn't there (without the `active`
#' statement there we would get an evaluation failure in the agent report).
#' 
#' ```r
#' agent <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table"
#'   ) %>%
#'   col_vals_gt(
#'     columns = c, value = vars(a),
#'     active = ~ . %>% has_columns(c(a, c))
#'   ) %>%
#'   col_vals_lt(
#'     columns = h, value = vars(d),
#'     preconditions = ~ . %>% dplyr::mutate(h = d - a),
#'     active = ~ . %>% has_columns(c(a, d))
#'   ) %>%
#'   col_is_character(
#'     columns = j,
#'     active = ~ . %>% has_columns(j)
#'   ) %>%
#'   interrogate()
#' ```
#' 
#' Through the agent's x-list, we can verify that no evaluation error (any
#' evaluation at all, really) had occurred. The third value, representative of
#' the third validation step, is actually `NA` instead of `FALSE` because the
#' step became inactive.
#' 
#' ```r
#' x_list <- get_agent_x_list(agent = agent)
#' 
#' x_list$eval_warning
#' ```
#' 
#' ```
#' ## [1] FALSE FALSE    NA
#' ```
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-2
#'
#' @export
has_columns <- function(
    x,
    columns
) {

  if (!is_a_table_object(x)) {
    stop(
      "The input for `has_columns()` should be a table object of any of ",
      "these types:\n",
      "* `data.frame` or `tbl_df` (tibble)\n",
      "* a `tbl_dbi` object (generated with `db_tbl()` or `dplyr::tbl()`)\n",
      "* a `tbl_spark` object (using the sparklyr package)",
      call. = FALSE
    )
  }
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  if (rlang::quo_is_missing(columns)) {
    rlang::abort("Must supply a value for `columns`")
  }
  
  # Split into quos if multi-length c()/vars() expr
  if (rlang::quo_is_call(columns, c("c", "vars"))) {
    columns_env <- rlang::quo_get_env(columns)
    column_quos <- lapply(rlang::call_args(columns), function(x) {
      rlang::new_quosure(x, env = columns_env)
    })
  } else {
    column_quos <- list(columns)
  }
  
  .call = rlang::current_env()
  has_column <- function(col_expr) {
    columns <- tryCatch(
      expr = resolve_columns(x = x, var_expr = col_expr,
                             allow_empty = FALSE, call = .call),
      error = function(cnd) cnd
    )
    ## Check for error from {tidyselect}
    if (rlang::is_error(columns)) {
      cnd <- columns
      # Rethrow error if genuine evaluation error
      if (inherits(cnd, "resolve_eval_err")) {
        rlang::cnd_signal(cnd)
      } 
      # FALSE if "column not found" or "0 columns" error
      return(FALSE)
    }
    ## TRUE if selection successful
    return(TRUE)
  }
  
  all(vapply(column_quos, has_column, logical(1), USE.NAMES = FALSE))
  
}
