#' Are column data not equal to a specified value?
#' 
#' The `col_vals_not_equal()` validation function, the
#' `expect_col_vals_not_equal()` expectation function, and the
#' `test_col_vals_not_equal()` test function all check whether column values in
#' a table *are not* equal to a specified `value`. The validation step function
#' can be used directly on a data table or with an *agent* object (technically,
#' a `ptblank_agent` object) whereas the expectation and test functions can only
#' be used with a data table. The types of data tables that can be used include
#' data frames, tibbles, database tables (`tbl_dbi`), and Spark DataFrames
#' (`tbl_spark`). Each validation step or expectation will operate over the
#' number of test units that is equal to the number of rows in the table (after
#' any `preconditions` have been applied).
#'
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names in
#' quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
#'
#' This validation function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of `na_pass =
#' FALSE` means that any `NA`s encountered will accumulate failing test units.
#' 
#' Having table `preconditions` means **pointblank** will mutate the table just
#' before interrogation. Such a table mutation is isolated in scope to the
#' validation step(s) produced by the validation function call. Using
#' **dplyr** code is suggested here since the statements can be translated to
#' SQL if necessary. The code is most easily supplied as a one-sided **R**
#' formula (using a leading `~`). In the formula representation, the `.` serves
#' as the input data table to be transformed (e.g., 
#' `~ . %>% dplyr::mutate(col_a = col_b + 10)`). Alternatively, a function could
#' instead be supplied (e.g., 
#' `function(x) dplyr::mutate(x, col_a = col_b + 10)`).
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction of
#' test units failed, or, an absolute value), often using the `warn_at`
#' argument. This is especially true when `x` is a table object because,
#' otherwise, nothing happens. For the `col_vals_*()`-type functions, using 
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
#' @param value a numeric value used to test for non-equality.
#' 
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#'
#' @examples
#' # For all of the examples here, we'll
#' # use a simple table with three numeric
#' # columns (`a`, `b`, and `c`) and three
#' # character columns (`d`, `e`, and `f`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 5, 5, 5, 5, 5),
#'     b = c(1, 1, 1, 2, 2, 2),
#'     c = c(1, 1, 1, 2, 2, 2),
#'     d = LETTERS[c(1:3, 5:7)],
#'     e = LETTERS[c(1:6)],
#'     f = LETTERS[c(1:6)]
#'   )
#'   
#' tbl
#' 
#' # A: Using an `agent` with validation
#' #    functions and then `interrogate()` 
#' 
#' # Validate that values in column `a`
#' # are all *not* equal to the value
#' # of `6`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_not_equal(vars(a), 6) %>%
#'   interrogate()
#' 
#' # Determine if this validation
#' # had no failing test units (there
#' # are 6 test units, one for each row)
#' all_passed(agent)
#' 
#' # Calling `agent` in the console
#' # prints the agent's report; but we
#' # can get a `gt_tbl` object directly
#' # with `get_agent_report(agent)`
#' 
#' # B: Using the validation function
#' #    directly on the data (no `agent`)
#' 
#' # This way of using validation functions
#' # acts as a data filter: data is passed
#' # through but should `stop()` if there
#' # is a single test unit failing; the
#' # behavior of side effects can be
#' # customized with the `actions` option
#' tbl %>% 
#'   col_vals_not_equal(vars(a), 6) %>%
#'   dplyr::pull(a)
#'   
#' # C: Using the expectation function
#' 
#' # With the `expect_*()` form, we would
#' # typically perform one validation at a
#' # time; this is primarily used in
#' # testthat tests
#' expect_col_vals_not_equal(tbl, vars(a), 6)
#' 
#' # D: Using the test function
#' 
#' # With the `test_*()` form, we should
#' # get a single logical value returned
#' # to us
#' test_col_vals_not_equal(tbl, vars(a), 6)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-4
#' 
#' @seealso The analogue to this function: [col_vals_equal()].
#' 
#' @name col_vals_not_equal
NULL

#' @rdname col_vals_not_equal
#' @import rlang
#' @export
col_vals_not_equal <- function(x,
                               columns,
                               value,
                               na_pass = FALSE,
                               preconditions = NULL,
                               actions = NULL,
                               step_id = NULL,
                               brief = NULL,
                               active = TRUE) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      col_vals_not_equal(
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
    brief <- generate_autobriefs(agent, columns, preconditions, values = value, "col_vals_not_equal")
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns, agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_not_equal",
        column = columns[i],
        values = value,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = covert_actions(actions, agent),
        step_id = step_id[i],
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_vals_not_equal
#' @import rlang
#' @export
expect_col_vals_not_equal <- function(object,
                                      columns,
                                      value,
                                      na_pass = FALSE,
                                      preconditions = NULL,
                                      threshold = 1) {
  
  fn_name <- "expect_col_vals_not_equal"
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_vals_not_equal(
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
  operator <- "!="
  values_text <- prep_values_text(values = vs$values, limit = 3, lang = "en")
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(failure_message_gluestring(fn_name = fn_name, lang = "en"))
  )
  
  act$val <- object
  
  invisible(act$val)
}

#' @rdname col_vals_not_equal
#' @import rlang
#' @export
test_col_vals_not_equal <- function(object,
                                    columns,
                                    value,
                                    na_pass = FALSE,
                                    preconditions = NULL,
                                    threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_vals_not_equal(
      columns = {{ columns }},
      value = {{ value }}, 
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>% .$validation_set
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  all(!vs$notify)
}
