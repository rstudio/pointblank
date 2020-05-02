#' Do the columns contain logical values?
#'
#' The `col_is_logical()` validation step function checks whether one or more
#' columns is of the logical (`TRUE`/`FALSE`) type. Like many of the
#' `col_is_*()`-type functions in **pointblank**, the only requirement is a
#' specification of the column names. This function can be used directly on a
#' data table or with an *agent* object (technically, a `ptblank_agent` object).
#' Each validation step will operate over a single test unit, which is whether
#' the column is an logical-type column or not.
#' 
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names
#' in quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation step function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction test
#' units failed, or, an absolute value), often using the `warn_at` argument.
#' This is especially true when `x` is a table object because, otherwise,
#' nothing happens. For the `col_is_*()`-type functions, using 
#' `action_levels(warn_at = 1)` or `action_levels(stop_at = 1)` are good choices
#' depending on the situation (the first produces a warning, the other
#' `stop()`s).
#' 
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#'
#' @inheritParams col_vals_gt
#' 
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple table with a
#' # column of `logical` values
#' tbl <- 
#'   dplyr::tibble(a = c(TRUE, FALSE))
#' 
#' # Validate that column `a` in the
#' # table is classed as `logical`
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_is_logical(vars(a)) %>%
#'   interrogate()
#' 
#' # Determine if this column
#' # validation has passed by using
#' # `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-19
#' 
#' @name col_is_logical
NULL

#' @rdname col_is_logical
#' @import rlang
#' @export
col_is_logical <- function(x,
                           columns,
                           actions = NULL,
                           brief = NULL,
                           active = TRUE) {
  
  preconditions <- NULL
  values <- NULL
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      col_is_logical(
        columns = columns,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x

  if (is.null(brief)) {
    brief <- generate_autobriefs(agent, columns, preconditions, values, "col_is_logical")
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_is_logical",
        column = columns[i],
        preconditions = NULL,
        actions = actions,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_is_logical
#' @import rlang
#' @export
expect_col_is_logical <- function(object,
                                  columns,
                                  threshold = 1) {
  
  expectation_type <- "expect_col_is_logical"
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_is_logical(
      columns = {{ columns }},
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
