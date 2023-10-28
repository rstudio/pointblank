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
#  Copyright (c) 2017-2023 pointblank authors
#  
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
# 
#------------------------------------------------------------------------------#


#' Perform multiple rowwise validations for joint validity
#'
#' @description
#' 
#' The `conjointly()` validation function, the `expect_conjointly()` expectation
#' function, and the `test_conjointly()` test function all check whether test
#' units at each index (typically each row) all pass multiple validations. We
#' can use validation functions that validate row units (the `col_vals_*()`
#' series), check for column existence ([col_exists()]), or validate column type
#' (the `col_is_*()` series). Because of the imposed constraint on the allowed
#' validation functions, the ensemble of test units are either comprised rows of
#' the table (after any common `preconditions` have been applied) or are single
#' test units (for those functions that validate columns).
#' 
#' Each of the functions used in a `conjointly()` validation step (composed
#' using multiple validation function calls) ultimately perform a rowwise test
#' of whether all sub-validations reported a *pass* for the same test units. In
#' practice, an example of a joint validation is testing whether values for
#' column `a` are greater than a specific value while adjacent values in column
#' `b` lie within a specified range. The validation functions to be part of the
#' conjoint validation are to be supplied as one-sided **R** formulas (using a
#' leading `~`, and having a `.` stand in as the data object). The validation
#' function can be used directly on a data table or with an *agent* object
#' (technically, a `ptblank_agent` object) whereas the expectation and test
#' functions can only be used with a data table.
#' 
#' @inheritParams col_vals_gt
#' 
#' @param ... *Validation expressions*
#' 
#'   `<validation expressions>` // **required** (or, use `.list`)
#' 
#'   A collection one-sided formulas that consist of validation functions that
#'   validate row units (the `col_vals_*()` series), column existence
#'   ([col_exists()]), or column type (the `col_is_*()` series). An example of
#'   this is `~ col_vals_gte(., vars(a), 5.5), ~ col_vals_not_null(., vars(b)`).
#' 
#' @param .list *Alternative to `...`*
#' 
#'   `<list of multiple expressions>` // **required** (or, use `...`)
#' 
#'   Allows for the use of a list as an input alternative to `...`.
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
#' If providing multiple column names in any of the supplied validation steps,
#' the result will be an expansion of sub-validation steps to that number of
#' column names. Aside from column names in quotes and in `vars()`,
#' **tidyselect** helper functions are available for specifying columns. They
#' are: `starts_with()`, `ends_with()`, `contains()`, `matches()`, and
#' `everything()`.
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
#' `conjointly()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `conjointly()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' R statement:
#' 
#' ```r
#' agent %>% 
#'   conjointly(
#'     ~ col_vals_lt(., columns = vars(a), value = 8),
#'     ~ col_vals_gt(., columns = vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., columns = vars(b)),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "The `conjointly()` step.",
#'     active = FALSE
#'   )
#' ```
#' 
#' YAML representation:
#' 
#' ```yaml
#' steps:
#' - conjointly:
#'     fns:
#'     - ~col_vals_lt(., columns = vars(a), value = 8)
#'     - ~col_vals_gt(., columns = vars(c), value = vars(a))
#'     - ~col_vals_not_null(., columns = vars(b))
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `conjointly()` step.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the expressions for
#' validation steps are necessary. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'
#' @section Examples:
#' 
#' For all examples here, we'll use a simple table with three numeric columns
#' (`a`, `b`, and `c`). This is a very basic table but it'll be more useful when
#' explaining things later.
#' 
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 2, 6),
#'     b = c(3, 4, 6),
#'     c = c(9, 8, 7)
#'   )
#'   
#' tbl
#' ```
#'   
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#' 
#' Validate a number of things on a row-by-row basis using validation functions
#' of the `col_vals*` type (all have the same number of test units): (1) values
#' in `a` are less than `8`, (2) values in `c` are greater than the adjacent
#' values in `a`, and (3) there aren't any NA values in `b`. We'll determine if
#' this validation has any failing test units (there are 3 test units, one for
#' each row).
#' 
#' ```r
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   conjointly(
#'     ~ col_vals_lt(., columns = vars(a), value = 8),
#'     ~ col_vals_gt(., columns = vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., columns = vars(b))
#'     ) %>%
#'   interrogate()
#' ```
#' 
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report, showing the single entry
#' that corresponds to the validation step demonstrated here.
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_conjointly_1.png")`
#' }
#' }
#' 
#' What's going on? Think of there being three parallel validations, each
#' producing a column of `TRUE` or `FALSE` values (`pass` or `fail`) and line
#' them up side-by-side, any rows with any `FALSE` values results in a conjoint
#' `fail` test unit.
#' 
#' ## B: Using the validation function directly on the data (no `agent`)
#' 
#' This way of using validation functions acts as a data filter. Data is passed
#' through but should `stop()` if there is a single test unit failing. The
#' behavior of side effects can be customized with the `actions` option.
#' 
#' ```{r}
#' tbl %>%
#'   conjointly(
#'     ~ col_vals_lt(., columns = vars(a), value = 8),
#'     ~ col_vals_gt(., columns = vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., columns = vars(b))
#'   )
#' ```
#'
#' ## C: Using the expectation function
#' 
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in **testthat** tests.
#' 
#' ```r
#' expect_conjointly(
#'   tbl,
#'   ~ col_vals_lt(., columns = vars(a), value = 8),
#'   ~ col_vals_gt(., columns = vars(c), value = vars(a)),
#'   ~ col_vals_not_null(., columns = vars(b))
#' )
#' ```
#' 
#' ## D: Using the test function
#' 
#' With the `test_*()` form, we should get a single logical value returned to
#' us.
#' 
#' ```{r}
#' tbl %>%
#'   test_conjointly(
#'     ~ col_vals_lt(., columns = vars(a), value = 8),
#'     ~ col_vals_gt(., columns = vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., columns = vars(b))
#'   )
#' ```
#' 
#' @family validation functions
#' @section Function ID:
#' 2-34
#'
#' @name conjointly
NULL

#' @rdname conjointly
#' @import rlang
#' @export
conjointly <- function(
    x,
    ...,
    .list = list2(...),
    preconditions = NULL,
    segments = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {

  # Obtain all of the group's elements
  list_elements <- .list
  
  dots_attrs <- list_elements[rlang::names2(list_elements) != ""]
  
  validation_formulas <-
    list_elements[
      vapply(
        list_elements,
        function(x) rlang::is_formula(x),
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]
  
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
      conjointly(
        .list = .list,
        preconditions = preconditions,
        segments = segments,
        actions = prime_actions(actions),
        label = label,
        brief = brief,
        active = active
      ) %>%
      interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "conjointly",
        preconditions = preconditions,
        values = validation_formulas
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)
  
  # Get the next step number for the `validation_set` tibble
  i_o <- get_next_validation_set_row(agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)

  # Add one or more validation steps based on the
  # length of `segments_list`
  label <- resolve_label(label, segments = segments_list)
  for (i in seq_along(segments_list)) {
    
    seg_col <- names(segments_list[i])
    seg_val <- unname(unlist(segments_list[i]))
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "conjointly",
        i_o = i_o,
        columns_expr = NULL,
        column = NULL,
        values = validation_formulas,
        na_pass = NULL,
        preconditions = preconditions,
        seg_expr = segments,
        seg_col = seg_col,
        seg_val = seg_val,
        actions = covert_actions(actions, agent),
        step_id = step_id,
        label = label[[i]],
        brief = brief,
        active = active
      )
  }
  
  agent
}

#' @rdname conjointly
#' @import rlang
#' @export
expect_conjointly <- function(
    object,
    ...,
    .list = list2(...),
    preconditions = NULL,
    threshold = 1
) {
  
  fn_name <- "expect_conjointly"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    conjointly(
      .list = .list,
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
  
  # TODO: express warnings and errors here
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  values_text <- prep_values_text(values = vs$values, limit = 3, lang = "en")
  
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

#' @rdname conjointly
#' @import rlang
#' @export
test_conjointly <- function(
    object,
    ...,
    .list = list2(...),
    preconditions = NULL,
    threshold = 1
) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    conjointly(
      .list = .list,
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
