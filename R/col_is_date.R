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


#' Do the columns contain R `Date` objects?
#'
#' @description
#' The `col_is_date()` validation function, the `expect_col_is_date()`
#' expectation function, and the `test_col_is_date()` test function all check
#' whether one or more columns in a table is of the **R** `Date` type. Like many
#' of the `col_is_*()`-type functions in **pointblank**, the only requirement is
#' a specification of the column names. The validation function can be used
#' directly on a data table or with an *agent* object (technically, a
#' `ptblank_agent` object) whereas the expectation and test functions can only
#' be used with a data table. The types of data tables that can be used include
#' data frames, tibbles, database tables (`tbl_dbi`), and Spark DataFrames
#' (`tbl_spark`). Each validation step or expectation will operate over a single
#' test unit, which is whether the column is a `Date`-type column or not.
#' 
#' @section Column Names:
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names in
#' quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
#' 
#' @section Actions:
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation function, takes a specially-crafted list object
#' that is best produced by the [action_levels()] function. Read that function's
#' documentation for the lowdown on how to create reactions to above-threshold
#' failure levels in validation. The basic gist is that you'll want at least a
#' single threshold level (specified as either the fraction of test units
#' failed, or, an absolute value), often using the `warn_at` argument. This is
#' especially true when `x` is a table object because, otherwise, nothing
#' happens. For the `col_is_*()`-type functions, using `action_levels(warn_at =
#' 1)` or `action_levels(stop_at = 1)` are good choices depending on the
#' situation (the first produces a warning, the other will `stop()`).
#' 
#' @section Briefs:
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#'
#' @section YAML:
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). When
#' `col_is_date()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `col_is_date()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' R statement:
#' 
#' ```r
#' agent %>% 
#'   col_is_date(
#'     vars(a),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "The `col_is_date()` step.",
#'     active = FALSE
#'   )
#' ```
#' 
#' YAML representation:
#' 
#' ```yaml
#' steps:
#' - col_is_date:
#'     columns: vars(a)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `col_is_date()` step.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns`
#' argument requires a value. Arguments with default values won't be written to
#' YAML when using [yaml_write()] (though it is acceptable to include them with
#' their default when generating the YAML by other means). It is also possible
#' to preview the transformation of an agent to YAML without any writing to disk
#' by using the [yaml_agent_string()] function.
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
#' @section Demos:
#' 
#' The `small_table` dataset in the package has a `date` column. The following
#' examples will validate that that column is of the `Date` class.
#' 
#' ```{r}
#' small_table
#' ```
#' 
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#' 
#' Validate that the column `date` has the `Date` class.
#' 
#' ```r
#' agent <-
#'   create_agent(small_table) %>%
#'   col_is_date(vars(date)) %>%
#'   interrogate()
#' ```
#'   
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report, showing the single entry
#' that corresponds to the validation step demonstrated here.
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_col_is_date_1.png")`
#' }
#' }
#' 
#' ## B: Using the validation function directly on the data (no `agent`)
#' 
#' This way of using validation functions acts as a data filter. Data is passed
#' through but should `stop()` if there is a single test unit failing. The
#' behavior of side effects can be customized with the `actions` option.
#' 
#' ```{r}
#' small_table %>%
#'   col_is_date(vars(date)) %>%
#'   dplyr::slice(1:5)
#' ```
#'
#' ## C: Using the expectation function
#' 
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in testthat tests.
#' 
#' ```r
#' expect_col_is_date(small_table, vars(date))
#' ```
#' 
#' ## D: Using the test function
#' 
#' With the `test_*()` form, we should get a single logical value returned to
#' us.
#' 
#' ```{r}
#' small_table %>% test_col_is_date(vars(date))
#' ```
#' 
#' @family validation functions
#' @section Function ID:
#' 2-26
#' 
#' @name col_is_date
NULL

#' @rdname col_is_date
#' @import rlang
#' @export
col_is_date <- function(
    x,
    columns,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {
  
  preconditions <- NULL
  values <- NULL
  
  # Get `columns` as a label
  columns_expr <- 
    rlang::as_label(rlang::quo(!!enquo(columns))) %>%
    gsub("^\"|\"$", "", .)
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (is_a_table_object(x)) {
    
    secret_agent <- 
      create_agent(x, label = "::QUIET::") %>%
      col_is_date(
        columns = columns,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    brief <- 
      generate_autobriefs(
        agent, columns, preconditions, values, "col_is_date"
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns, agent)
  
  # Get the next step number for the `validation_set` tibble
  i_o <- get_next_validation_set_row(agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_is_date",
        i_o = i_o,
        columns_expr = columns_expr,
        column = columns[i],
        preconditions = NULL,
        actions = covert_actions(actions, agent),
        step_id = step_id[i],
        label = label,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_is_date
#' @import rlang
#' @export
expect_col_is_date <- function(
    object,
    columns,
    threshold = 1
) {
  
  fn_name <- "expect_col_is_date"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_is_date(
      columns = {{ columns }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  x <- vs$notify
  
  threshold_type <- get_threshold_type(threshold = threshold)
  
  if (threshold_type == "proportional") {
    failed_amount <- vs$f_failed
  } else {
    failed_amount <- vs$n_failed
  }
  
  # If several validations were performed serially (due to supplying
  # multiple columns)
  if (length(x) > 1 && any(x)) {
    
    # Get the index (step) of the first failure instance
    fail_idx <- which(x)[1]
    
    # Get the correct, single `failed_amount` for the first
    # failure instance
    failed_amount <- failed_amount[fail_idx]
    
    # Redefine `x` as a single TRUE value
    x <- TRUE
    
  } else {
    x <- any(x)
    fail_idx <- 1
  }
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  column_text <- prep_column_text(vs$column[[fail_idx]])
  col_type <- "Date"
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(
      failure_message_gluestring(
        fn_name = fn_name, lang = "en"
      )
    )
  )
  
  act$val <- object
  
  invisible(act$val)
}

#' @rdname col_is_date
#' @import rlang
#' @export
test_col_is_date <- function(
    object,
    columns,
    threshold = 1
) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_is_date(
      columns = {{ columns }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  all(!vs$notify)
}
