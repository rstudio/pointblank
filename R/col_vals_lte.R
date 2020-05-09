#' Are column data less than or equal to a specified value?
#'
#' The `col_vals_lte()` validation step function and the `expect_col_vals_lte()`
#' expectation function both check whether column values in a table are *less
#' than or equal to* a specified `value` (the exact comparison used in this
#' function is `col_val <= value`). The `value` can be specified as a single,
#' literal value or as a column name given in `vars()`. The validation step
#' function can be used directly on a data table or with an *agent* object
#' (technically, a `ptblank_agent` object) whereas the expectation function can
#' only be used with a data table. The types of data tables that can be used
#' include data frames, tibbles, and even database tables of `tbl_dbi` class.
#' Each validation step or expectation will operate over the number of test
#' units that is equal to the number of rows in the table (after any
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
#' before interrogation. Such a table mutation is isolated in scope to the
#' validation step(s) produced by the validation step function call. Using
#' **dplyr** code is suggested here since the statements can be translated to
#' SQL if necessary. The code is most easily supplied as a one-sided **R**
#' formula (using a leading `~`). In the formula representation, the `.` serves
#' as the input data table to be transformed (e.g., 
#' `~ . %>% dplyr::mutate(col_a = col_b + 10)`). Alternatively, a function could
#' instead be supplied (e.g., 
#' `function(x) dplyr::mutate(x, col_a = col_b + 10)`).
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
#' @param value A numeric value used for this test. Any column values `<= value`
#'   are considered passing.
#'   
#' @return For the validation step function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   
#' @examples
#' # Create a simple table with a
#' # column of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 4, 1, 2),
#'     b = c(3, 2, 5, 6)
#'   )
#' 
#' # Validate that the sum of
#' # values across columns `a`
#' # and `b` are always less
#' # than or equal to 10
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_lte(vars(a_b), 10,
#'     preconditions = ~
#'       . %>% dplyr::mutate(a_b = a + b)
#'   ) %>%
#'   interrogate()
#' 
#' # Determine if this column
#' # validation has passed by using
#' # `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-2
#' 
#' @seealso The analogous function with a right-open bound: [col_vals_lt()].
#' 
#' @name col_vals_lte
NULL

#' @rdname col_vals_lte
#' @import rlang
#' @export
col_vals_lte <- function(x,
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
      col_vals_lte(
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
    brief <- generate_autobriefs(agent, columns, preconditions, values = value, "col_vals_lte")
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_lte",
        column = columns[i],
        values = value,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_vals_lte
#' @import rlang
#' @export
expect_col_vals_lte <- function(object,
                                columns,
                                value,
                                na_pass = FALSE,
                                preconditions = NULL,
                                threshold = 1) {
  
  expectation_type <- "expect_col_vals_lte"
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_vals_lte(
      columns = {{ columns }},
      value = {{ value }}, 
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>% .$validation_set
  
  x <- vs$notify %>% all()
  
  threshold_type <- get_threshold_type(threshold = threshold)
  
  if (threshold_type == "proportional") {
    failed_amount <- vs$f_failed
  } else {
    failed_amount <- vs$n_failed
  }
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  column_text <- prep_column_text(vs$column[[1]])
  operator <- "<="
  values_text <- prep_values_text(values = vs$values, limit = 3, lang = "en")
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(failure_message_gluestring(expectation_type))
  )
  
  act$val <- object
  
  invisible(act$val)
}
