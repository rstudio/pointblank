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

#' Run several tests and a final validation in a serial manner
#'
#' @description 
#' The `serially()` validation function allows for a series of tests to run in
#' sequence before either culminating in a final validation step or simply
#' exiting the series. This construction allows for pre-testing that may make
#' sense before a validation step. For example, there may be situations where
#' it's vital to check a column type before performing a validation on the same
#' column (since having the wrong type can result in an evaluation error for the
#' subsequent validation). Another serial workflow might entail having a bundle
#' of checks in a prescribed order and, if all pass, then the goal of this
#' testing has been achieved (e.g., checking if a table matches another through
#' a series of increasingly specific tests).
#' 
#' A series as specified inside `serially()` is composed with a listing of
#' calls, and we would draw upon test functions (**T**) to describe tests and
#' optionally provide a finalizing call with a validation function (**V**).
#' The following constraints apply:
#' 
#' - there must be at least one test function in the series (**T** -> **V** is
#' good, **V** is *not*)
#' - there can only be one validation function call, **V**; it's optional but,
#' if included, it must be placed at the end (**T** -> **T** -> **V** is good,
#' these sequences are bad: (1) **T** -> **V** -> **T**, (2) **T** -> **T** -> 
#' **V** -> **V**)
#' - a validation function call (**V**), if included, mustn't itself yield
#' multiple validation steps (this may happen when providing multiple `columns`
#' or any `segments`)
#' 
#' Here's an example of how to arrange expressions:
#' 
#' ```
#' ~ test_col_exists(., columns = vars(count)),
#' ~ test_col_is_numeric(., columns = vars(count)),
#' ~ col_vals_gt(., columns = vars(count), value = 2)
#' ```
#' 
#' This series concentrates on the column called `count` and first checks
#' whether the column exists, then checks if that column is numeric, and then
#' finally validates whether all values in the column are greater than `2`.
#' 
#' Note that in the above listing of calls, the `.` stands in for the target
#' table and is always necessary here. Also important is that all `test_*()`
#' functions have a `threshold` argument that is set to `1` by default. Should
#' you need to bump up the threshold value it can be changed to a different
#' integer value (as an absolute threshold of failing test units) or a
#' decimal value between `0` and `1` (serving as a fractional threshold of
#' failing test units).
#' 
#' @section Supported Input Tables:
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
#' If providing multiple column names in any of the supplied validation steps,
#' the result will be an expansion of sub-validation steps to that number of
#' column names. Aside from column names in quotes and in `vars()`,
#' **tidyselect** helper functions are available for specifying columns. They
#' are: `starts_with()`, `ends_with()`, `contains()`, `matches()`, and
#' `everything()`.
#' 
#' @section Preconditions:
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
#' @section Actions:
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
#' `serially()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `serially()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' R statement:
#' 
#' ```r
#' agent %>% 
#'   serially(
#'     ~ col_vals_lt(., columns = vars(a), value = 8),
#'     ~ col_vals_gt(., columns = vars(c), value = vars(a)),
#'     ~ col_vals_not_null(., columns = vars(b)),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "The `serially()` step.",
#'     active = FALSE
#'   )
#' ```
#' 
#' YAML representation:
#' 
#' ```yaml
#' steps:
#' - serially:
#'     fns:
#'     - ~col_vals_lt(., columns = vars(a), value = 8)
#'     - ~col_vals_gt(., columns = vars(c), value = vars(a))
#'     - ~col_vals_not_null(., vars(b))
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `serially()` step.
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
#' @inheritParams col_vals_gt
#' @param ... A collection one-sided formulas that consist of `test_*()`
#'   function calls (e.g., [test_col_vals_between()], etc.) arranged in sequence
#'   of intended interrogation order. Typically, validations up until the final
#'   one would have some `threshold` value set (default is `1`) for short
#'   circuiting within the series. A finishing validation function call (e.g.,
#'   [col_vals_increasing()], etc.) can optionally be inserted at the end of the
#'   series, serving as a validation step that only undergoes interrogation if
#'   the prior tests adequately pass. An example of this is
#'   `~ test_column_exists(., vars(a)), ~ col_vals_not_null(., vars(a))`).
#' @param .list Allows for the use of a list as an input alternative to `...`.
#' 
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
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
#'     b = c(6, 4, 9),
#'     c = c(1, 2, 3)
#'   )
#'   
#' tbl
#' ```
#'   
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#' 
#' The `serially()` function can be set up to perform a series of tests and then
#' perform a validation (only if all tests pass). Here, we are going to (1) test
#' whether columns `a` and `b` are numeric, (2) check that both don't have any
#' `NA` values, and (3) perform a finalizing validation that checks whether
#' values in `b` are greater than values in `a`. We'll determine if this
#' validation has any failing test units (there are 4 tests and a final
#' validation).
#' 
#' ```r
#' agent_1 <-
#'   create_agent(tbl = tbl) %>%
#'   serially(
#'     ~ test_col_is_numeric(., columns = vars(a, b)),
#'     ~ test_col_vals_not_null(., columns = vars(a, b)),
#'     ~ col_vals_gt(., columns = vars(b), value = vars(a))
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
#' `r pb_get_image_tag(file = "man_serially_1.png")`
#' }
#' }
#' 
#' What's going on? All four of the tests passed and so the final validation
#' occurred. There were no failing test units in that either!
#'
#' The final validation is optional and so here is a variation where only the
#' serial tests are performed.
#' 
#' ```r
#' agent_2 <-
#'   create_agent(tbl = tbl) %>%
#'   serially(
#'     ~ test_col_is_numeric(., columns = vars(a, b)),
#'     ~ test_col_vals_not_null(., columns = vars(a, b))
#'   ) %>%
#'   interrogate()
#' ```
#' 
#' Everything is good here too:
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_serially_2.png")`
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
#'   serially(
#'     ~ test_col_is_numeric(., columns = vars(a, b)),
#'     ~ test_col_vals_not_null(., columns = vars(a, b)),
#'     ~ col_vals_gt(., columns = vars(b), value = vars(a))
#'   )
#' ```
#'
#' ## C: Using the expectation function
#' 
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in **testthat** tests.
#' 
#' ```r
#' expect_serially(
#'   tbl,
#'   ~ test_col_is_numeric(., columns = vars(a, b)),
#'   ~ test_col_vals_not_null(., columns = vars(a, b)),
#'   ~ col_vals_gt(., columns = vars(b), value = vars(a))
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
#'   test_serially(
#'     ~ test_col_is_numeric(., columns = vars(a, b)),
#'     ~ test_col_vals_not_null(., columns = vars(a, b)),
#'     ~ col_vals_gt(., columns = vars(b), value = vars(a))
#'   )
#' ```
#'
#' @family validation functions
#' @section Function ID:
#' 2-35
#' 
#' @name serially
NULL

#' @rdname serially
#' @import rlang
#' 
#' @export
serially <- function(
    x,
    ...,
    .list = list2(...),
    preconditions = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {
  
  segments <- NULL
  
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
  
  assertion_types <-
    vapply(
      validation_formulas,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        x %>%
          rlang::f_rhs() %>%
          as.character() %>%
          .[[1]]
      }
    )
  
  # Check that the vector of `assertion_types` uses valid
  # validation function names (including the `test_*()` variants)
  in_set_of_fns <-
    all(
      assertion_types %in% 
        c(
          all_validations_fns_vec(),
          paste0("test_", all_validations_fns_vec())
        )
    )
  
  if (!in_set_of_fns) {
    
    stop(
      "All `serially()` steps must use validation or test function calls.",
      call. = FALSE
    )
  }
  
  # There must be at least one `test_*()` step; if not, stop the function
  has_a_test <- 
    any(assertion_types %in% paste0("test_", all_validations_fns_vec()))
  
  if (!has_a_test) {
    
    stop(
      "There must be at least one `test_*()` function call in `serially()`.",
      call. = FALSE
    )
  }
  
  # Check whether the series has any validation calls
  any_validations <-
    any(assertion_types %in% all_validations_fns_vec())
  
  # If there are any validations we must ensure a few things
  # [1] there isn't more than one call
  # [2] the validation call must be the final call
  # [3] the finalizing validation mustn't itself yield multiple steps
  if (any_validations) {
    
    # Check [1]: more than one validation function call
    has_multiple_validations <-
      sum(assertion_types %in% all_validations_fns_vec()) > 1
    
    if (has_multiple_validations) {
      
      stop(
        "There cannot be multiple validation function calls in `serially()`",
        call. = FALSE
      )
    }
    
    # Check [2]: validation function call must be final call
    validation_is_final_call <-
      which(assertion_types %in% all_validations_fns_vec()) ==
      length(assertion_types)
    
    if (!validation_is_final_call) {
      
      stop(
        "The validation function call must be the final one in `serially()`",
        call. = FALSE
      )
    }
    
    # Check [3]: the validation function call cannot yield multiple steps
    validation_step_call_args <-
      validation_formulas[length(validation_formulas)][[1]] %>%
      as.call() %>%
      rlang::call_args()
    
    # Check the first argument
    if (!as.character(validation_step_call_args[[1]]) == ".") {
      
      stop(
        "The first argument to a validation function call must be \".\"",
        call. = FALSE
      )
    }
    
    # Check whether the validation function is of type that has an
    # expandable `columns` argument
    has_expandable_cols_arg <-
      assertion_types[length(assertion_types)] %in%
      base::setdiff(
        all_validations_fns_vec(),
        c(
          "rows_distinct", "rows_complete",
          "col_vals_expr", "col_schema_match",
          "conjointly"
        )
      )
    
    if (has_expandable_cols_arg) {
      
      has_multiple_cols <- 
        rlang::as_label(validation_step_call_args[[2]]) %>%
        gsub("^\"|\"$", "", .) %>%
        grepl(",", x = .)
      
      if (has_multiple_cols) {
        
        stop(
          "The finalizing validation function call must only operate on a ",
          "single column",
          call. = FALSE
        )
      }
    }
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
      serially(
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
    
    validation_n <- length(validation_formulas)
    
    assertion_types <-
      vapply(
        validation_formulas,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          x %>%
            rlang::f_rhs() %>%
            as.character() %>%
            .[[1]]
        }
      )
    
    # Initialize the `serially_validation_set` tibble
    serially_validation_set <- dplyr::tibble()
    
    has_final_validation <-
      assertion_types[length(assertion_types)] %in% all_validations_fns_vec()
    
    # Get the total number of `test_*()` calls supplied
    test_call_n <- 
      if (has_final_validation) validation_n - 1 else validation_n
    
    #
    # Determine the total number of test steps
    #
    
    # Create a `double_agent` that will be used just for determining
    # the number of test steps
    double_agent <- create_agent(tbl = dplyr::tibble(), label = "::QUIET::")
    
    for (k in seq_len(test_call_n)) {
      
      double_agent <-
        eval(
          expr = parse(
            text =
              validation_formulas[[k]] %>%
              rlang::f_rhs() %>%
              rlang::expr_deparse() %>%
              tidy_gsub("(.", "(double_agent", fixed = TRUE) %>%
              tidy_gsub("^test_", "") %>%
              tidy_gsub("threshold\\s+?=\\s.*$", ")") %>%
              tidy_gsub(",\\s+?\\)$", ")")
            
          ),
          envir = NULL
        )
    }
    
    test_step_n <- nrow(double_agent$validation_set)
    
    if (has_final_validation) {
      
      final_validation_type <- assertion_types[length(assertion_types)]
      
      double_agent <- create_agent(tbl = dplyr::tibble(), label = "::QUIET::")
      
      double_agent <-
        eval(
          expr = parse(
            text =
              validation_formulas[[length(validation_formulas)]] %>%
              rlang::f_rhs() %>%
              rlang::expr_deparse() %>%
              tidy_gsub("(.", "(double_agent", fixed = TRUE) %>%
              tidy_gsub("^test_", "") %>%
              tidy_gsub("threshold\\s+?=\\s.*$", ")") %>%
              tidy_gsub(",\\s+?\\)$", ")")
            
          ),
          envir = NULL
        )
      
      final_validation_values <- double_agent$validation_set$values
      final_validation_column <- double_agent$validation_set$column
      
    } else {
      
      final_validation_type <- NA_character_
      final_validation_values <- list(NULL)
      final_validation_column <- list(NULL)
    }
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "serially",
        preconditions = preconditions,
        values = list(
          validation_formulas = validation_formulas,
          total_test_calls = test_call_n,
          total_test_steps = test_step_n,
          has_final_validation = has_final_validation,
          final_validation_type = final_validation_type,
          final_validation_column = final_validation_column,
          final_validation_values = final_validation_values
        )
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
  for (i in seq_along(segments_list)) {
    
    seg_col <- names(segments_list[i])
    seg_val <- unname(unlist(segments_list[i]))
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "serially",
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
        label = label,
        brief = brief,
        active = active
      )
  }
  
  agent
}

#' @rdname serially
#' @import rlang
#' @export
expect_serially <- function(
    object,
    ...,
    .list = list2(...),
    preconditions = NULL,
    threshold = 1
) {
  
  fn_name <- "expect_serially"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    serially(
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

#' @rdname serially
#' @import rlang
#' @export
test_serially <- function(
    object,
    ...,
    .list = list2(...),
    preconditions = NULL,
    threshold = 1
) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    serially(
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
