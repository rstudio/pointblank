#' Are column data part of a specified set of values?
#'
#' The `col_vals_in_set()` validation step function checks whether column values
#' (in any number of specified `columns`) are part of a specified `set` of
#' values. This function can be used directly on a data table or with an *agent*
#' object (technically, a `ptblank_agent` object). Each validation step will
#' operate over the number of test units that is equal to the number of rows in
#' the table (after any `preconditions` have been applied).
#' 
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names
#' in quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
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
#' @param set A vector of numeric or string-based elements, where column values
#'   found within this `set` will be considered as passing.
#'   
#' @return For the validation step function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   
#' @examples
#' # Create a simple table with 2
#' # columns: one with numerical
#' # values, the other with strings
#' tbl <-
#'   dplyr::tibble(
#'     a = c(1, 2, 3, 4),
#'     b = rep(c("one", "two"), 2)
#'   )
#' 
#' # Validate that all numerical values
#' # in column `a` belong to a numerical
#' # set, and, create an analogous 
#' # validation check for column `b` with
#' # a set of string values 
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_in_set(vars(a), 1:4) %>%
#'   col_vals_in_set(vars(b), c("one", "two")) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-9
#' 
#' @seealso The analogue to this function: [col_vals_not_in_set()].
#' 
#' @name col_vals_in_set
NULL

#' @rdname col_vals_in_set
#' @import rlang
#' @export
col_vals_in_set <- function(x,
                            columns,
                            set,
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
      col_vals_in_set(
        columns = columns,
        set = set,
        preconditions = preconditions,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x

  if (is.null(brief)) {
    brief <- generate_autobriefs(agent, columns, preconditions, values = set, "col_vals_in_set")
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_in_set",
        column = columns[i],
        values = set,
        preconditions = preconditions,
        actions = actions,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_vals_in_set
#' @import rlang
#' @export
expect_col_vals_in_set <- function(object,
                                   columns,
                                   set,
                                   preconditions = NULL,
                                   threshold = 1) {
  
  expectation_type <- "expect_col_vals_in_set"
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_vals_in_set(
      columns = {{ columns }},
      set = {{ set }}, 
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
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(failure_message_gluestring)
  )
  
  act$val <- object
  
  invisible(act$val)
}
