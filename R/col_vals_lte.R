#------------------------------------------------------------------------------#
#
#                 _         _    _      _                _
#                (_)       | |  | |    | |              | |
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   <
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |
#  |_|
#
#  This file is part of the 'rstudio/pointblank' project.
#
#  Copyright (c) 2017-2024 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


#' Are column data less than or equal to a fixed value or data in another
#' column?
#'
#' @description
#'
#' The `col_vals_lte()` validation function, the `expect_col_vals_lte()`
#' expectation function, and the `test_col_vals_lte()` test function all check
#' whether column values in a table are *less than or equal to* a specified
#' `value` (the exact comparison used in this function is `col_val <= value`).
#' The `value` can be specified as a single, literal value or as a column name
#' given in `vars()`. The validation function can be used directly on a data
#' table or with an *agent* object (technically, a `ptblank_agent` object)
#' whereas the expectation and test functions can only be used with a data
#' table. Each validation step or expectation will operate over the number of
#' test units that is equal to the number of rows in the table (after any
#' `preconditions` have been applied).
#'
#' @inheritParams col_vals_gt
#'
#' @param value *Value for comparison*
#'
#'   `<value expression>` // **required**
#'
#'   A value used for this comparison. This can be a single value or a
#'   compatible column given in `vars()`. Any column values less than or equal
#'   to what is specified here will pass validation.
#'
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#'
#' @section Supported Input Tables:
#'
#' The types of data tables that are officially supported are:
#'
#'  - data frames (`data.frame`) and tibbles (`tbl_df`)
#'  - Spark DataFrames (`tbl_spark`)
#'  - the following database tables (`tbl_dbi`):
#'    - *PostgreSQL* tables (using the `RPostgres::Postgres()` as driver)
#'    - *MySQL* tables (with `RMySQL::MySQL()`)
#'    - *Microsoft SQL Server* tables (via **odbc**)
#'    - *BigQuery* tables (using `bigrquery::bigquery()`)
#'    - *DuckDB* tables (through `duckdb::duckdb()`)
#'    - *SQLite* (with `RSQLite::SQLite()`)
#'
#' Other database tables may work to varying degrees but they haven't been
#' formally tested (so be mindful of this when using unsupported backends with
#' **pointblank**).
#'
#' @section Column Names:
#'
#' `columns` may be a single column (as symbol `a` or string `"a"`) or a vector
#' of columns (`c(a, b, c)` or `c("a", "b", "c")`). `{tidyselect}` helpers
#' are also supported, such as `contains("date")` and `where(is.double)`. If
#' passing an *external vector* of columns, it should be wrapped in `all_of()`.
#'
#' When multiple columns are selected by `columns`, the result will be an
#' expansion of validation steps to that number of columns (e.g.,
#' `c(col_a, col_b)` will result in the entry of two validation steps).
#'
#' Previously, columns could be specified in `vars()`. This continues to work,
#' but `c()` offers the same capability and supersedes `vars()` in `columns`.
#'
#' @section Missing Values:
#'
#' This validation function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of `na_pass =
#' FALSE` means that any `NA`s encountered will accumulate failing test units.
#'
#' @section Preconditions:
#'
#' Providing expressions as `preconditions` means **pointblank** will preprocess
#' the target table during interrogation as a preparatory step. It might happen
#' that a particular validation requires a calculated column, some filtering of
#' rows, or the addition of columns via a join, etc. Especially for an
#' *agent*-based report this can be advantageous since we can develop a large
#' validation plan with a single target table and make minor adjustments to it,
#' as needed, along the way.
#'
#' The table mutation is totally isolated in scope to the validation step(s)
#' where `preconditions` is used. Using **dplyr** code is suggested here since
#' the statements can be translated to SQL if necessary (i.e., if the target
#' table resides in a database). The code is most easily supplied as a one-sided
#' **R** formula (using a leading `~`). In the formula representation, the `.`
#' serves as the input data table to be transformed (e.g., `~ . %>%
#' dplyr::mutate(col_b = col_a + 10)`). Alternatively, a function could instead
#' be supplied (e.g., `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#'
#' @section Segments:
#'
#' By using the `segments` argument, it's possible to define a particular
#' validation with segments (or row slices) of the target table. An optional
#' expression or set of expressions that serve to segment the target table by
#' column values. Each expression can be given in one of two ways: (1) as column
#' names, or (2) as a two-sided formula where the LHS holds a column name and
#' the RHS contains the column values to segment on.
#'
#' As an example of the first type of expression that can be used,
#' `vars(a_column)` will segment the target table in however many unique values
#' are present in the column called `a_column`. This is great if every unique
#' value in a particular column (like different locations, or different dates)
#' requires it's own repeating validation.
#'
#' With a formula, we can be more selective with which column values should be
#' used for segmentation. Using `a_column ~ c("group_1", "group_2")` will
#' attempt to obtain two segments where one is a slice of data where the value
#' `"group_1"` exists in the column named `"a_column"`, and, the other is a
#' slice where `"group_2"` exists in the same column. Each group of rows
#' resolved from the formula will result in a separate validation step.
#'
#' If there are multiple `columns` specified then the potential number of
#' validation steps will be `m` columns multiplied by `n` segments resolved.
#'
#' Segmentation will always occur after `preconditions` (i.e., statements that
#' mutate the target table), if any, are applied. With this type of one-two
#' combo, it's possible to generate labels for segmentation using an expression
#' for `preconditions` and refer to those labels in `segments` without having to
#' generate a separate version of the target table.
#'
#' @section Actions:
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
#' @section Labels:
#'
#' `label` may be a single string or a character vector that matches the number
#' of expanded steps. `label` also supports `{glue}` syntax and exposes the
#' following dynamic variables contextualized to the current step:
#'
#' - `"{.step}"`: The validation step name
#' - `"{.col}"`: The current column name
#' - `"{.seg_col}"`: The current segment's column name
#' - `"{.seg_val}"`: The current segment's value/group
#'
#' The glue context also supports ordinary expressions for further flexibility
#' (e.g., `"{toupper(.step)}"`) as long as they return a length-1 string.
#'
#' @section Briefs:
#'
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#'
#' @section YAML:
#'
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). When
#' `col_vals_lte()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `col_vals_lte()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#'
#' R statement:
#'
#' ```r
#' agent %>%
#'   col_vals_lte(
#'     columns = a,
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "The `col_vals_lte()` step.",
#'     active = FALSE
#'   )
#' ```
#'
#' YAML representation:
#'
#' ```yaml
#' steps:
#' - col_vals_lte:
#'     columns: c(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `col_vals_lte()` step.
#'     active: false
#' ```
#'
#' In practice, both of these will often be shorter as only the `columns` and
#' `value` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'
#' @section Examples:
#'
#' For all of the examples here, we'll use a simple table with three numeric
#' columns (`a`, `b`, and `c`) and three character columns (`d`, `e`, and `f`).
#'
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'       a = c(5, 5, 5, 5, 5, 5),
#'       b = c(1, 1, 1, 2, 2, 2),
#'       c = c(1, 1, 1, 2, 3, 4),
#'       d = LETTERS[a],
#'       e = LETTERS[b],
#'       f = LETTERS[c]
#'   )
#'
#' tbl
#' ```
#'
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#'
#' Validate that values in column `c` are all less than or equal to the value of
#' `4`. We'll determine if this validation has any failing test units (there are
#' 6 test units, one for each row).
#'
#' ```r
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_lte(columns = c, value = 4) %>%
#'   interrogate()
#' ```
#'
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report, showing the single entry
#' that corresponds to the validation step demonstrated here.
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_col_vals_lte_1.png")`
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
#' tbl %>%
#'   col_vals_lte(columns = c, value = 4) %>%
#'   dplyr::pull(c)
#' ```
#'
#' ## C: Using the expectation function
#'
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in **testthat** tests.
#'
#' ```r
#' expect_col_vals_lte(tbl, columns = c, value = 4)
#' ```
#'
#' ## D: Using the test function
#'
#' With the `test_*()` form, we should get a single logical value returned to
#' us.
#'
#' ```{r}
#' test_col_vals_lte(tbl, columns = c, value = 4)
#' ```
#'
#' @family validation functions
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
col_vals_lte <- function(
    x,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    segments = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {

  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  # Get `columns` as a label
  columns_expr <- as_columns_expr(columns)

  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)

  # Resolve segments into list
  segments_list <-
    resolve_segments(
      x = x,
      seg_expr = segments,
      preconditions = preconditions
    )

  if (is_a_table_object(x)) {

    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      col_vals_lte(
        columns = tidyselect::all_of(columns),
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        segments = segments,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()

    return(x)
  }

  agent <- x

  brief <- resolve_brief(
    brief = brief, agent = agent,
    columns = columns, segments_list = segments_list,
    preconditions = preconditions, values = value,
    assertion_type = "col_vals_lte"
  )

  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns, agent)

  # Get the next step number for the `validation_set` tibble
  i_o <- get_next_validation_set_row(agent)

  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)

  # Add one or more validation steps based on the
  # length of the `columns` variable
  label <- resolve_label(label, columns, segments_list)
  for (i in seq_along(columns)) {
    for (j in seq_along(segments_list)) {

      seg_col <- names(segments_list[j])
      seg_val <- unname(unlist(segments_list[j]))

      agent <-
        create_validation_step(
          agent = agent,
          assertion_type = "col_vals_lte",
          i_o = i_o,
          columns_expr = columns_expr,
          column = columns[i],
          values = value,
          na_pass = na_pass,
          preconditions = preconditions,
          seg_expr = segments,
          seg_col = seg_col,
          seg_val = seg_val,
          actions = covert_actions(actions, agent),
          step_id = step_id[i],
          label = label[[i, j]],
          brief = brief[[i, j]],
          active = active
        )
    }
  }

  agent
}

#' @rdname col_vals_lte
#' @import rlang
#' @export
expect_col_vals_lte <- function(
    object,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {

  fn_name <- "expect_col_vals_lte"

  vs <-
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_lte(
      columns = {{ columns }},
      value = {{ value }},
      na_pass = na_pass,
      preconditions = {{ preconditions }},
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
  operator <- "<="
  values_text <-
    prep_values_text(values = vs$values[[fail_idx]], limit = 3, lang = "en")

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

#' @rdname col_vals_lte
#' @import rlang
#' @export
test_col_vals_lte <- function(
    object,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {

  vs <-
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_lte(
      columns = {{ columns }},
      value = {{ value }},
      na_pass = na_pass,
      preconditions = {{ preconditions }},
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
