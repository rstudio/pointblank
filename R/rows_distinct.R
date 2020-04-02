#' Verify that row data are distinct
#'
#' The `rows_distinct()` validation step function checks whether row values
#' (optionally constrained to a selection of specified `columns`) are, when
#' taken as a complete unit, distinct from all other units in the table. This
#' function can be used directly on a data table or with an *agent* object
#' (technically, a `ptblank_agent` object). This validation step will operate
#' over the number of test units that is equal to the number of rows in the
#' table (after any `preconditions` have been applied).
#' 
#' We can specify the constraining column names in quotes, in `vars()`, and with
#' the following **tidyselect** helper functions: `starts_with()`,
#' `ends_with()`, `contains()`, `matches()`, and `everything()`.
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
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
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
#' @family Validation Step Functions
#' @section Function ID:
#' 2-15
#' 
#' @import rlang
#' @export

rows_distinct <- function(x,
                          columns = NULL,
                          preconditions = NULL,
                          actions = NULL,
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
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      rows_distinct(
        columns = columns,
        preconditions = preconditions,
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

  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "rows_distinct",
      column = list(ifelse(is.null(columns), NA_character_, columns)),
      values = NULL,
      preconditions = preconditions,
      actions = actions,
      brief = brief,
      active = active
    )

  agent
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

