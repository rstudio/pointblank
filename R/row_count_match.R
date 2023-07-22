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


#' Does the row count match that of a different table?
#'
#' @description
#' 
#' The `row_count_match()` validation function, the `expect_row_count_match()`
#' expectation function, and the `test_row_count_match()` test function all
#' check whether the row count in the target table matches that of a comparison
#' table. The validation function can be used directly on a data table or with
#' an *agent* object (technically, a `ptblank_agent` object) whereas the
#' expectation and test functions can only be used with a data table. As a
#' validation step or as an expectation, there is a single test unit that hinges
#' on whether the row counts for the two tables are the same (after any
#' `preconditions` have been applied).
#' 
#' @inheritParams col_vals_gt
#' @param count Either a literal value for the number of rows, or, a table to
#'   compare against the target table in terms of row count values. If supplying
#'   a comparison table, it can either be a table object such as a data frame, a
#'   tibble, a `tbl_dbi` object, or a `tbl_spark` object. Alternatively, a
#'   table-prep formula (`~ <table reading code>`) or a function
#'   (`function() <table reading code>`) can be used to lazily read in the
#'   comparison table at interrogation time.
#' @param tbl_compare The `tbl_compare` argument is deprecated. Instead, use
#'   `count`.
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
#' before the row count comparison takes place. Using `preconditions` can be
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
#' Segmentation will always occur after `preconditions` (i.e., statements that
#' mutate the target table), if any, are applied. With this type of one-two
#' combo, it's possible to generate labels for segmentation using an expression
#' for `preconditions` and refer to those labels in `segments` without having to
#' generate a separate version of the target table.
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
#' `row_count_match()` is represented in YAML (under the top-level `steps` key
#' as a list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `row_count_match()` as
#' a validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' R statement:
#' 
#' ```r
#' agent %>% 
#'   row_count_match(
#'     count = ~ file_tbl(
#'       file = from_github(
#'         file = "sj_all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       ),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "The `row_count_match()` step.",
#'     active = FALSE
#'   )
#' ```
#' 
#' YAML representation:
#' 
#' ```yaml
#' steps:
#' - row_count_match:
#'     count: ~ file_tbl(
#'       file = from_github(
#'         file = "sj_all_revenue_large.rds",
#'         repo = "rich-iannone/intendo",
#'         subdir = "data-large"
#'         )
#'       )
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `row_count_match()` step.
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
#' Create a simple table with three columns and four rows of values.
#' 
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5),
#'     b = c(7, 1, 0, 0),
#'     c = c(1, 1, 1, 3)
#'   )
#' 
#' tbl
#' ```
#'
#' Create a second table which is quite different but has the same number of
#' rows as `tbl`.
#' 
#' ```{r}
#' tbl_2 <-
#'   dplyr::tibble(
#'     e = c("a", NA, "a", "c"),
#'     f = c(2.6, 1.2, 0, NA)
#'   )
#' 
#' tbl_2
#' ```
#' 
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#' 
#' Validate that the count of rows in the target table (`tbl`) matches that of
#' the comparison table (`tbl_2`).
#' 
#' ```r
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   row_count_match(count = tbl_2) %>%
#'   interrogate()
#' ```
#' 
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report, showing the single entry
#' that corresponds to the validation step demonstrated here.
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_row_count_match_1.png")`
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
#' tbl %>% row_count_match(count = tbl_2)
#' ```
#' 
#' ## C: Using the expectation function
#' 
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in **testthat** tests.
#' 
#' ```r
#' expect_row_count_match(tbl, count = tbl_2)
#' ```
#' 
#' ## D: Using the test function
#' 
#' With the `test_*()` form, we should get a single logical value returned to
#' us.
#' 
#' ```{r}
#' tbl %>% test_row_count_match(count = 4)
#' ```
#' 
#' @family validation functions
#' @section Function ID:
#' 2-31
#' 
#' @name row_count_match
NULL

#' @rdname row_count_match
#' @import rlang
#' @export
row_count_match <- function(
    x,
    count,
    preconditions = NULL,
    segments = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE,
    tbl_compare = NULL
) {
  
  # The `tbl_compare` argument is undergoing soft deprecation so if it is
  # not missing, issue a warning and migrate the supplied value over to
  # the `count` argument
  if (!is.null(tbl_compare)) {
    
    count <- tbl_compare
    
    warning(
      "Use `count` to specify a comparison table (`tbl_compare` is now ",
      "undergoing deprecation):\n",
      " * The `count` argument can accept a literal numeric value in addition ",
      "to a comparison table",
      call. = FALSE
    )
  }
  
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
      row_count_match(
        count = count,
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
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "row_count_match",
        values = count
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
  # length of `segments`
  for (i in seq_along(segments_list)) {
    
    seg_col <- names(segments_list[i])
    seg_val <- unname(unlist(segments_list[i]))
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "row_count_match",
        i_o = i_o,
        columns_expr = NA_character_,
        column = NA_character_,
        values = count,
        preconditions = preconditions,
        seg_expr = segments,
        seg_col = seg_col,
        seg_val = seg_val,
        actions = covert_actions(actions, agent),
        step_id = step_id,
        label = label,
        brief = brief,
        active = active
      )
  }
  
  agent
}

#' @rdname row_count_match
#' @import rlang
#' @export
expect_row_count_match <- function(
    object,
    count,
    preconditions = NULL,
    threshold = 1,
    tbl_compare = NULL
) {
  
  # The `tbl_compare` argument is undergoing soft deprecation so if it is
  # not missing, issue a warning and migrate the supplied value over to
  # the `count` argument
  if (!is.null(tbl_compare)) {
    
    count <- tbl_compare
    
    warning(
      "Use `count` to specify a comparison table (`tbl_compare` is now ",
      "undergoing deprecation):\n",
      " * The `count` argument can accept a literal numeric value in addition ",
      "to a comparison table",
      call. = FALSE
    )
  }
  
  fn_name <- "expect_row_count_match"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    row_count_match(
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

#' @rdname row_count_match
#' @import rlang
#' @export
test_row_count_match <- function(
    object,
    count,
    preconditions = NULL,
    threshold = 1,
    tbl_compare = NULL
) {
  
  # The `tbl_compare` argument is undergoing soft deprecation so if it is
  # not missing, issue a warning and migrate the supplied value over to
  # the `count` argument
  if (!is.null(tbl_compare)) {
    
    count <- tbl_compare
    
    warning(
      "Use `count` to specify a comparison table (`tbl_compare` is now ",
      "undergoing deprecation):\n",
      " * The `count` argument can accept a literal numeric value in addition ",
      "to a comparison table",
      call. = FALSE
    )
  }
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    row_count_match(
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
