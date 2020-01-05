#' Are numerical column data less than or equal to a specific value?
#'
#' The `col_vals_lte()` validation step function checks whether column values
#' (in any number of specified `columns`) are *less than or equal to* a
#' specified `value` (the exact comparison used in this function is
#' `col_val <= value`). This function can be used directly on a data table or
#' with an *agent* object (technically, a `ptblank_agent` object). Each
#' validation step will operate over the number of test units that is equal to
#' the number of rows in the table (after any `preconditions` have been
#' applied).
#' 
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names
#' in quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
#' 
#' This validation step function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of
#' `na_pass = FALSE` means that any `NA`s encountered will accumulate failing
#' test units. 
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation step function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction test
#' units failed, or, an absolute value), often using the `warn_at` argument.
#' This is especially true when `x` is a table object because, otherwise,
#' nothing happens. For the `col_vals_*()`-type functions, using 
#' `action_levels(warn_at = 0.25)` or `action_levels(stop_at = 0.25)` are good
#' choices depending on the situation (the first produces a warning when a
#' quarter of the total test units fails, the other `stop()`s at the same
#' threshold level).
#' 
#' Having table `preconditions` means **pointblank** will mutate the table just
#' before interrogation. It's isolated to the validation steps produced by this
#' validation step function. Using **dplyr** code is suggested here since the
#' statements can be translated to SQL if necessary. The code is to be supplied
#' as a one-sided **R** formula (using a leading `~`). In the formula
#' representation, the obligatory `tbl` variable will serve as the input
#' data table to be transformed (e.g.,
#' `~ tbl %>% dplyr::mutate(col_a = col_b + 10)`. A series of expressions can be
#' used by enclosing the set of statements with `{ }` but note that the `tbl`
#' variable must be ultimately returned.
#' 
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#' 
#' @inheritParams col_vals_gt
#' @param value A numeric value used for this test. Any column values `<= value`
#'   are considered passing.
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' library(dplyr)
#' 
#' # Create a simple data frame with
#' # a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 4, 1, 2),
#'     b = c(3, 2, 5, 6)
#'   )
#' 
#' # Validate that the sum of
#' # values across columns `a`
#' # and `b` are always less
#' # than or equal to 10
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_lte(vars(a_b), 10,
#'     preconditions = ~ {
#'       tbl %>% dplyr::mutate(a_b = a + b)
#'     }
#'   ) %>%
#'   interrogate()
#' 
#' # Determine if this column
#' # validation has passed by using
#' # `all_passed()`
#' all_passed(agent)
#' 
#' @seealso The analogous function with a right-open bound: [col_vals_lt()].
#' 
#' @import rlang
#' @export
col_vals_lte <- function(x,
                         columns,
                         value,
                         na_pass = FALSE,
                         preconditions = NULL,
                         actions = NULL,
                         brief = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x) %>%
      col_vals_lte(
        columns = columns,
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        brief = brief,
        actions = prime_actions(actions)
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_lte",
        preconditions = preconditions,
        column = columns,
        value = value
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_lte",
        column = column,
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }

  agent
}
