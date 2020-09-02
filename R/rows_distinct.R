#' Are row data distinct?
#'
#' The `rows_distinct()` validation function, the `expect_rows_distinct()`
#' expectation function, and the `test_rows_distinct()` test function all check
#' whether row values (optionally constrained to a selection of specified
#' `columns`) are, when taken as a complete unit, distinct from all other units
#' in the table. The validation function can be used directly on a data table or
#' with an *agent* object (technically, a `ptblank_agent` object) whereas the
#' expectation and test functions can only be used with a data table. The types
#' of data tables that can be used include data frames, tibbles, database tables
#' (`tbl_dbi`), and Spark DataFrames (`tbl_spark`). As a validation step or as
#' an expectation, this will operate over the number of test units that is equal
#' to the number of rows in the table (after any `preconditions` have been
#' applied).
#'
#' We can specify the constraining column names in quotes, in `vars()`, and with
#' the following **tidyselect** helper functions: `starts_with()`,
#' `ends_with()`, `contains()`, `matches()`, and `everything()`.
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
#'   
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#'   
#' @examples
#' # Create a simple table with three
#' # columns of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 8, 3),
#'     c = c(1, 1, 1, 3, 3, 3)
#'   )
#' 
#' # Validate that when considering only
#' # data in columns `a` and `b`, there
#' # are no duplicate rows (i.e., all
#' # rows are distinct)
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   rows_distinct(vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @family validation functions
#' @section Function ID:
#' 2-15
#' 
#' @name rows_distinct
NULL

#' @rdname rows_distinct
#' @import rlang
#' @export
rows_distinct <- function(x,
                          columns = NULL,
                          preconditions = NULL,
                          actions = NULL,
                          step_id = NULL,
                          label = NULL,
                          brief = NULL,
                          active = TRUE) {

  # Capture the `columns` expression
  #columns <- rlang::enquo(columns)
  
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

  # Resolve the columns based on the expression
  if (!is.null(columns)) {
    columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  }
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x, label = "::QUIET::") %>%
      rows_distinct(
        columns = columns,
        preconditions = preconditions,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (length(columns) > 0) {
    columns <- paste(columns, collapse = ", ")
  } else if (length(columns) == 1) {
    columns <- columns
  } else {
    columns <- NULL
  }
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "rows_distinct",
        column = columns
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # TODO: check `step_id` value(s) against previous recorded IDs

  # Add a validation step
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "rows_distinct",
      column = list(ifelse(is.null(columns), NA_character_, columns)),
      values = NULL,
      preconditions = preconditions,
      actions = covert_actions(actions, agent),
      step_id = step_id,
      label = label,
      brief = brief,
      active = active
    )

  agent
}

#' @rdname rows_distinct
#' @import rlang
#' @export
expect_rows_distinct <- function(object,
                                 columns = NULL,
                                 preconditions = NULL,
                                 threshold = 1) {
  
  fn_name <- "expect_rows_distinct"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    rows_distinct(
      columns = {{ columns }},
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
    failure_message = glue::glue(failure_message_gluestring(fn_name = fn_name, lang = "en"))
  )
  
  act$val <- object
  
  invisible(act$val)
}

#' @rdname rows_distinct
#' @import rlang
#' @export
test_rows_distinct <- function(object,
                               columns = NULL,
                               preconditions = NULL,
                               threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    rows_distinct(
      columns = {{ columns }},
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

#' Verify that row data are not duplicated (deprecated)
#'
#' @inheritParams col_vals_gt
#' @param x An agent object of class `ptblank_agent`.
#'   
#' @return A `ptblank_agent` object.
#'
#' @export
rows_not_duplicated <- function(x,
                                columns = NULL,
                                preconditions = NULL,
                                brief = NULL,
                                actions = NULL,
                                active = TRUE) {
  
  # nocov start

  warning("The `rows_not_duplicated()` function is deprecated and will soon be removed\n",
          " * Use the `rows_distinct()` function instead",
          call. = FALSE)
  
  rows_distinct(
    x = x,
    columns = {{ columns }},
    preconditions = preconditions,
    actions = actions,
    brief = brief,
    active = active
  )

  # nocov end
}
