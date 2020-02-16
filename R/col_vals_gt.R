#' Are column data greater than a specified value?
#'
#' The `col_vals_gt()` validation step function checks whether column values (in
#' any number of specified `columns`) are *greater than* a specified `value`
#' (the exact comparison used in this function is `col_val > value`). The
#' `value` can be specified as a single, literal value or as a column name given
#' in `vars()`. This function can be used directly on a data table or with an
#' *agent* object (technically, a `ptblank_agent` object). Each validation step
#' will operate over the number of test units that is equal to the number of
#' rows in the table (after any `preconditions` have been applied).
#'
#' If providing multiple column names to `columns`, the result will be an
#' expansion of validation steps to that number of column names (e.g.,
#' `vars(col_a, col_b)` will result in the entry of two validation steps). Aside
#' from column names in quotes and in `vars()`, **tidyselect** helper functions
#' are available for specifying columns. They are: `starts_with()`,
#' `ends_with()`, `contains()`, `matches()`, and `everything()`.
#' 
#' This validation step function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of
#' `na_pass = FALSE` means that any `NA`s encountered will accumulate failing
#' test units. 
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
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#'
#' @param x A data frame, tibble, or an agent object of class `ptblank_agent`.
#' @param columns The column (or a set of columns, provided as a character
#'   vector) to which this validation should be applied.
#' @param value A numeric value used for this test. Any column values `>value`
#'   are considered passing.
#' @param na_pass Should any encountered `NA` values be allowed to pass a
#'   validation unit? This is by default `FALSE`. Set to `TRUE` to give `NA`s
#'   a pass.
#' @param preconditions expressions used for mutating the input table before
#'   proceeding with the validation. This is ideally as a one-sided R formula
#'   using a leading `~`. In the formula representation, the `tbl` serves as the
#'   input data table to be transformed (e.g.,
#'   `~ tbl %>% dplyr::mutate(col = col + 10)`. A series of expressions can be
#'   used by enclosing the set of statements with `{ }` but note that the `tbl`
#'   object must be ultimately returned.
#' @param actions A list containing threshold levels so that the validation step
#'   can react accordingly when exceeding the set levels. This is to be created
#'   with the [action_levels()] helper function.
#' @param brief An optional, text-based description for the validation step.
#' @param active A logical value indicating whether the validation step should
#'   be active. If the step function is working with an agent, `FALSE` will make
#'   the validation step inactive (still reporting its presence and keeping
#'   indexes for the steps unchanged). If the step function will be operating
#'   directly on data, then any step with `active = FALSE` will simply pass the
#'   data through with no validation whatsoever. The default for this is `TRUE`.
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' library(dplyr)
#' 
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- tibble(a = c(5, 7, 8, 5))
#' 
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), 4) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-6
#' 
#' @seealso The analogous function with a left-closed bound: [col_vals_gte()].
#' 
#' @import rlang
#' @export
col_vals_gt <- function(x,
                        columns,
                        value,
                        na_pass = FALSE,
                        preconditions = NULL,
                        actions = NULL,
                        brief = NULL,
                        active = TRUE) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)

  if (is_a_table_object(x)) {

    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      col_vals_gt(
        columns = columns,
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_gt",
        preconditions = preconditions,
        column = columns,
        values = value
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_gt",
        column = column,
        values = value,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief,
        active = active
      )
  }

  agent
}
