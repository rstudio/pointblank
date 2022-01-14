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
#' @param x The table object.
#' @param columns One or more column names that are to be checked for existence
#' in the table `x`.
#'   
#' @return A length-1 logical vector.
#' 
#' @examples 
#' # The `small_table` dataset in the
#' # package has the columns `date_time`,
#' # `date`, and the `a` through `f`
#' # columns
#' small_table
#' 
#' # With `has_columns()` we can check for
#' # column existence by using it directly
#' # on the table; a column name can be
#' # verified as present by using it in
#' # double quotes
#' small_table %>% has_columns("date")
#' 
#' # Multiple column names can be supplied;
#' # this is `TRUE` because both columns are
#' # present in `small_table`
#' small_table %>% has_columns(c("a", "b"))
#' 
#' # It's possible to supply column names
#' # in `vars()` as well
#' small_table %>% has_columns(vars(a, b))
#' 
#' # Because column `h` isn't present, this
#' # returns `FALSE` (all specified columns
#' # need to be present to obtain `TRUE`)
#' small_table %>% has_columns(vars(a, h))
#' 
#' # The `has_columns()` function can be
#' # useful in expressions that involve the
#' # target table, especially if it is
#' # uncertain that the table will contain
#' # a column that's involved in a validation
#' 
#' # In the following agent-based validation,
#' # the first two steps will be 'active'
#' # because all columns checked for in the
#' # expressions are present; the third step
#' # is inactive because column `j` isn't
#' # there (without the `active` statement we
#' # would get an evaluation failure in the
#' # agent report)
#' agent <- 
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table"
#'   ) %>%
#'   col_vals_gt(
#'     vars(c), value = vars(a),
#'     active = ~ . %>% has_columns(vars(a, c))
#'   ) %>%
#'   col_vals_lt(
#'     vars(h), value = vars(d),
#'     preconditions = ~ . %>% dplyr::mutate(h = d - a),
#'     active = ~ . %>% has_columns(vars(a, d))
#'   ) %>%
#'   col_is_character(
#'     vars(j),
#'     active = ~ . %>% has_columns("j")
#'   ) %>%
#'   interrogate() 
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-2
#'
#' @export
has_columns <- function(x, columns) {

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
