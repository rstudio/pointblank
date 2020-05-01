#' Are column data not between two specified values?
#' 
#' The `col_vals_not_between()` validation step function checks whether column
#' values (in any number of specified `columns`) *do not* fall within a range.
#' The range specified with three arguments: `left`, `right`, and `inclusive`.
#' The `left` and `right` values specify the lower and upper bounds. The bounds
#' can be specified as single, literal values or as column names given in
#' `vars()`. The `inclusive` argument, as a vector of two logical values
#' relating to `left` and `right`, states whether each bound is inclusive or
#' not. The default is `c(TRUE, TRUE)`, where both endpoints are inclusive
#' (i.e., `[left, right]`). For partially-unbounded versions of this function,
#' we can use the [col_vals_lt()], [col_vals_lte()], [col_vals_gt()], or
#' [col_vals_gte()] validation step functions. This function can be used
#' directly on a data table or with an *agent* object (technically, a
#' `ptblank_agent` object). Each validation step will operate over the number of
#' test units that is equal to the number of rows in the table (after any
#' `preconditions` have been applied).
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
#' @inheritParams col_vals_gt
#' @param left,right The lower and uppers bounds for the range. The validation
#'   Any values `>= left` and `<= right` will be considered as failing.
#' @inheritParams col_vals_between
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- 
#'   dplyr::tibble(a = c(5.6, 7.8, 3.4))
#' 
#' # Validate that none of the values 
#' # in column `a` are between 9 and 10,
#' # or, between 0 and 2
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_not_between(vars(a), 9, 10) %>%
#'   col_vals_not_between(vars(a), 0, 2) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed by
#' # using `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-8
#' 
#' @seealso The analogue to this function: [col_vals_between()].
#' 
#' @name col_vals_not_between
NULL

#' @rdname col_vals_not_between
#' @import rlang
#' @export
col_vals_not_between <- function(x,
                                 columns,
                                 left,
                                 right,
                                 inclusive = c(TRUE, TRUE),
                                 na_pass = FALSE,
                                 preconditions = NULL,
                                 actions = NULL,
                                 brief = NULL,
                                 active = TRUE) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  left <- stats::setNames(left, inclusive[1])
  right <- stats::setNames(right, inclusive[2])
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      col_vals_not_between(
        columns = columns,
        left = left,
        right = right,
        inclusive = inclusive,
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
    brief <- generate_autobriefs(agent, columns, preconditions, values = c(left, right), "col_vals_not_between")
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_not_between",
        column = columns[i],
        values = c(left, right),
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_vals_not_between
#' @import rlang
#' @export
expect_col_vals_not_between <- function(object,
                                        columns,
                                        left,
                                        right,
                                        inclusive = c(TRUE, TRUE),
                                        na_pass = FALSE,
                                        preconditions = NULL,
                                        threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_vals_not_between(
      columns = {{ columns }},
      left = {{ left }}, 
      right = {{ right }},
      inclusive = inclusive,
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>% .$validation_set
  
  x <- vs$notify %>% all()
  f_failed <- vs$f_failed
  
  # TODO: express warnings and errors here
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  columns <- prep_column_text(columns) %>% tidy_gsub("~", "")
  
  # TODO: format message in the case of multiple columns passed in
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue("Some values in {columns} are between {left} and {right}.")
  )
  
  act$val <- object
  
  invisible(act$val)
}
