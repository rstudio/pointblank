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


#' Determine if one or more columns exist in a table
#' 
#' @description
#' 
#' This utility function can help you easily determine whether a column of a
#' specified name is present in a table object. This function works well enough
#' on a table object but it can also be used as part of a formula in any
#' validation function's `active` argument. Using `active = ~ . %>%
#' has_columns("column_1")` means that the validation step will be inactive if
#' the target table doesn't contain a column named `column_1`. We can also use
#' multiple columns in `vars()` so having `active = ~ . %>%
#' has_columns(vars(column_1, column_2))` in a validation step will make it
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
#'   `vector<character>|vars(<columns>)`` // **required**
#' 
#'   One or more column names that are to be checked for existence in the table
#'   `x`.
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
#' on the table. A column name can be verified as present by using it in double
#' quotes.
#' 
#' ```r
#' small_table %>% has_columns(columns = "date")
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
#' small_table %>% has_columns(columns = c("a", "b"))
#' ```
#' 
#' ```
#' ## [1] TRUE
#' ```
#' 
#' It's possible to supply column names in `vars()` as well:
#' 
#' ```r
#' small_table %>% has_columns(columns = vars(a, b))
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
#' small_table %>% has_columns(columns = vars(a, h))
#' ```
#' 
#' ```
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
#'     columns = vars(c), value = vars(a),
#'     active = ~ . %>% has_columns(vars(a, c))
#'   ) %>%
#'   col_vals_lt(
#'     columns = vars(h), value = vars(d),
#'     preconditions = ~ . %>% dplyr::mutate(h = d - a),
#'     active = ~ . %>% has_columns(vars(a, d))
#'   ) %>%
#'   col_is_character(
#'     columns = vars(j),
#'     active = ~ . %>% has_columns("j")
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
  
  # Normalize the `columns` expression
  if (inherits(columns, "quosures")) {
    
    columns <- 
      vapply(
        columns,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) as.character(rlang::get_expr(x))
      )
  }
  
  all(columns %in% rlang::names2(x))
}
