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


#' Does the column count match that of a different table?
#'
#' @description
#'
#' The `col_count_match()` validation function, the `expect_col_count_match()`
#' expectation function, and the `test_col_count_match()` test function all
#' check whether the column count in the target table matches that of a
#' comparison table. The validation function can be used directly on a data
#' table or with an *agent* object (technically, a `ptblank_agent` object)
#' whereas the expectation and test functions can only be used with a data
#' table. As a validation step or as an expectation, there is a single test unit
#' that hinges on whether the column counts for the two tables are the same
#' (after any `preconditions` have been applied).
#'
#' @inheritParams col_vals_gt
#'
#' @param count *The count comparison*
#'
#'   `scalar<numeric|integer>|obj:<tbl_*>` // **required**
#'
#'   Either a literal value for the number of columns, or, a table to compare
#'   against the target table in terms of column count values. If supplying a
#'   comparison table, it can either be a table object such as a data frame, a
#'   tibble, a `tbl_dbi` object, or a `tbl_spark` object. Alternatively, a
#'   table-prep formula (`~ <tbl reading code>`) or a function (
#'   `function() <tbl reading code>`) can be used to lazily read in the
#'   comparison table at interrogation time.
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
#' @section Preconditions:
#'
#' Providing expressions as `preconditions` means **pointblank** will preprocess
#' the target table during interrogation as a preparatory step. It might happen
#' that this particular validation requires some operation on the target table
#' before the column count comparison takes place. Using `preconditions` can be
#' useful at times since since we can develop a large validation plan with a
#' single target table and make minor adjustments to it, as needed, along the
#' way.
#'
#' The table mutation is totally isolated in scope to the validation step(s)
#' where `preconditions` is used. Using **dplyr** code is suggested here since
#' the statements can be translated to SQL if necessary (i.e., if the target
#' table resides in a database). The code is most easily supplied as a one-sided
#' **R** formula (using a leading `~`). In the formula representation, the `.`
#' serves as the input data table to be transformed. Alternatively, a function
#' could instead be supplied.
#'
#' @section Actions:
#'
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation function, takes a specially-crafted list object
#' that is best produced by the [action_levels()] function. Read that function's
#' documentation for the lowdown on how to create reactions to above-threshold
#' failure levels in validation. The basic gist is that you'll want at least a
#' single threshold level (specified as either the fraction of test units
#' failed, or, an absolute value), often using the `warn_at` argument. Using
#' `action_levels(warn_at = 1)` or `action_levels(stop_at = 1)` are good choices
#' depending on the situation (the first produces a warning, the other
#' `stop()`s).
#'
#' @section Labels:
#'
#' `label` may be a single string or a character vector that matches the number
#' of expanded steps. `label` also supports `{glue}` syntax and exposes the
#' following dynamic variables contextualized to the current step:
#'
#' - `"{.step}"`: The validation step name
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
#' `col_count_match()` is represented in YAML (under the top-level `steps` key
#' as a list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `col_count_match()` as
#' a validation step is expressed in R code and in the corresponding YAML
#' representation.
#'
#' R statement:
#'
#' ```r
#' agent %>%
#'   col_count_match(
#'     count = ~ file_tbl(
#'       file = from_github(
#'         file = "sj_all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       ),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "The `col_count_match()` step.",
#'     active = FALSE
#'   )
#' ```
#'
#' YAML representation:
#'
#' ```yaml
#' steps:
#' - col_count_match:
#'     count: ~ file_tbl(
#'       file = from_github(
#'         file = "sj_all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       )
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `col_count_match()` step.
#'     active: false
#' ```
#'
#' In practice, both of these will often be shorter. Arguments with default
#' values won't be written to YAML when using [yaml_write()] (though it is
#' acceptable to include them with their default when generating the YAML by
#' other means). It is also possible to preview the transformation of an agent
#' to YAML without any writing to disk by using the [yaml_agent_string()]
#' function.
#'
#' @section Examples:
#'
#' Create a simple table with three columns and three rows of values:
#'
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6),
#'     b = c(7, 1, 0),
#'     c = c(1, 1, 1)
#'   )
#'
#' tbl
#' ```
#'
#' Create a second table which is quite different but has the same number of
#' columns as `tbl`.
#'
#' ```{r}
#' tbl_2 <-
#'   dplyr::tibble(
#'     e = c("a", NA, "a", "c"),
#'     f = c(2.6, 1.2, 0, NA),
#'     g = c("f", "g", "h", "i")
#'   )
#'
#' tbl_2
#' ```
#'
#' We'll use these tables with the different function variants.
#'
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#'
#' Validate that the count of columns in the target table (`tbl`) matches that
#' of the comparison table (`tbl_2`).
#'
#' ```r
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_count_match(count = tbl_2) %>%
#'   interrogate()
#' ```
#'
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report, showing the single entry
#' that corresponds to the validation step demonstrated here.
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_col_count_match_1.png")`
#' }
#' }
#'
#' ## B: Using the validation function directly on the data (no `agent`)
#'
#' This way of using validation functions acts as a data filter: data is passed
#' through but should `stop()` if there is a single test unit failing. The
#' behavior of side effects can be customized with the `actions` option.
#'
#' ```{r}
#' tbl %>% col_count_match(count = tbl_2)
#' ```
#'
#' ## C: Using the expectation function
#'
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in **testthat** tests.
#'
#' ```r
#' expect_col_count_match(tbl, count = tbl_2)
#' ```
#'
#' ## D: Using the test function
#'
#' With the `test_*()` form, we should get a single logical value returned to
#' us.
#'
#' ```{r}
#' tbl %>% test_col_count_match(count = 3)
#' ```
#'
#' @family validation functions
#' @section Function ID:
#' 2-32
#'
#' @name col_count_match
NULL

#' @rdname col_count_match
#' @import rlang
#' @export
col_count_match <- function(
    x,
    count,
    preconditions = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {

  if (is_a_table_object(x)) {

    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      col_count_match(
        count = count,
        preconditions = preconditions,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()

    return(x)
  }

  agent <- x

  brief <- resolve_briefs(
    brief = brief, agent = agent,
    preconditions = preconditions, values = count,
    assertion_type = "col_count_match"
  )

  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)

  # Get the next step number for the `validation_set` tibble
  i_o <- get_next_validation_set_row(agent)

  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)

  # Add the validation step
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_count_match",
      i_o = i_o,
      columns_expr = NA_character_,
      column = NA_character_,
      values = count,
      preconditions = preconditions,
      actions = covert_actions(actions, agent),
      step_id = step_id,
      label = label,
      brief = brief,
      active = active
    )

  agent
}

#' @rdname col_count_match
#' @import rlang
#' @export
expect_col_count_match <- function(
    object,
    count,
    preconditions = NULL,
    threshold = 1
) {

  fn_name <- "expect_col_count_match"

  vs <-
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_count_match(
      count = {{ count }},
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set

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
    failure_message = glue::glue(
      failure_message_gluestring(
        fn_name = fn_name, lang = "en"
      )
    )
  )

  act$val <- object

  invisible(act$val)
}

#' @rdname col_count_match
#' @import rlang
#' @export
test_col_count_match <- function(
    object,
    count,
    preconditions = NULL,
    threshold = 1
) {

  vs <-
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_count_match(
      count = {{ count }},
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
