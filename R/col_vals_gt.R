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


#' Are column data greater than a fixed value or data in another column?
#'
#' @description
#' The `col_vals_gt()` validation function, the `expect_col_vals_gt()`
#' expectation function, and the `test_col_vals_gt()` test function all check
#' whether column values in a table are *greater than* a specified `value` (the
#' exact comparison used in this function is `col_val > value`). The `value` can
#' be specified as a single, literal value or as a column name given in
#' `vars()`. The validation function can be used directly on a data table or
#' with an *agent* object (technically, a `ptblank_agent` object) whereas the
#' expectation and test functions can only be used with a data table. The types
#' of data tables that can be used include data frames, tibbles, database tables
#' (`tbl_dbi`), and Spark DataFrames (`tbl_spark`). Each validation step or
#' expectation will operate over the number of test units that is equal to the
#' number of rows in the table (after any `preconditions` have been applied).
#'
#' @section Column Names:
#' If providing multiple column names to `columns`, the result will be an
#' expansion of validation steps to that number of column names (e.g.,
#' `vars(col_a, col_b)` will result in the entry of two validation steps). Aside
#' from column names in quotes and in `vars()`, **tidyselect** helper functions
#' are available for specifying columns. They are: `starts_with()`,
#' `ends_with()`, `contains()`, `matches()`, and `everything()`.
#'
#' @section Missing Values:
#' This validation function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of `na_pass =
#' FALSE` means that any `NA`s encountered will accumulate failing test units.
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
#' @section Segments:
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
#' `col_vals_gt()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `col_vals_gt()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' R statement:
#' 
#' ```r
#' agent %>% 
#'   col_vals_gt(
#'     columns = vars(a),
#'     value = 1,
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "The `col_vals_gt()` step.",
#'     active = FALSE
#'   )
#' ```
#' 
#' YAML representation:
#' 
#' ```yaml
#' steps:
#' - col_vals_gt:
#'     columns: vars(a)
#'     value: 1.0
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `col_vals_gt()` step.
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
#' @param x A data frame, tibble (`tbl_df` or `tbl_dbi`), Spark DataFrame
#'   (`tbl_spark`), or, an *agent* object of class `ptblank_agent` that is
#'   created with [create_agent()].
#' @param object A data frame, tibble (`tbl_df` or `tbl_dbi`), or Spark
#'   DataFrame (`tbl_spark`) that serves as the target table for the expectation
#'   function or the test function.
#' @param columns The column (or a set of columns, provided as a character
#'   vector) to which this validation should be applied.
#' @param value A value used for this comparison. This can be a single value or
#'   a compatible column given in `vars()`. Any column values greater than what
#'   is specified here will pass validation.
#' @param na_pass Should any encountered `NA` values be considered as passing
#'   test units? This is by default `FALSE`. Set to `TRUE` to give `NA`s a pass.
#' @param preconditions An optional expression for mutating the input table
#'   before proceeding with the validation. This can either be provided as a
#'   one-sided R formula using a leading `~` (e.g.,
#'   `~ . %>% dplyr::mutate(col = col + 10)` or as a function (e.g.,
#'   `function(x) dplyr::mutate(x, col = col + 10)`. See the *Preconditions*
#'   section for more information.
#' @param segments An optional expression or set of expressions (held in a list)
#'   that serve to segment the target table by column values. Each expression
#'   can be given in one of two ways: (1) as column names, or (2) as a two-sided
#'   formula where the LHS holds a column name and the RHS contains the column
#'   values to segment on. See the *Segments* section for more details on this.
#' @param actions A list containing threshold levels so that the validation step
#'   can react accordingly when exceeding the set levels. This is to be created
#'   with the [action_levels()] helper function.
#' @param step_id One or more optional identifiers for the single or multiple
#'   validation steps generated from calling a validation function. The use of
#'   step IDs serves to distinguish validation steps from each other and provide
#'   an opportunity for supplying a more meaningful label compared to the step
#'   index. By default this is `NULL`, and **pointblank** will automatically
#'   generate the step ID value (based on the step index) in this case. One or
#'   more values can be provided, and the exact number of ID values should (1)
#'   match the number of validation steps that the validation function call will
#'   produce (influenced by the number of `columns` provided), (2) be an ID
#'   string not used in any previous validation step, and (3) be a vector with
#'   unique values.
#' @param threshold A simple failure threshold value for use with the
#'   expectation (`expect_`) and the test (`test_`) function variants. By
#'   default, this is set to `1` meaning that any single unit of failure in data
#'   validation results in an overall test failure. Whole numbers beyond `1`
#'   indicate that any failing units up to that absolute threshold value will
#'   result in a succeeding **testthat** test or evaluate to `TRUE`. Likewise,
#'   fractional values (between `0` and `1`) act as a proportional failure
#'   threshold, where `0.15` means that 15 percent of failing test units results
#'   in an overall test failure.
#' @param label An optional label for the validation step. This label appears in
#'   the *agent* report and for the best appearance it should be kept short.
#' @param brief An optional, text-based description for the validation step. If
#'   nothing is provided here then an *autobrief* is generated by the *agent*,
#'   using the language provided in [create_agent()]'s `lang` argument (which
#'   defaults to `"en"` or English). The *autobrief* incorporates details of the
#'   validation step so it's often the preferred option in most cases (where a
#'   `label` might be better suited to succinctly describe the validation).
#' @param active A logical value indicating whether the validation step should
#'   be active. If the validation function is working with an *agent*, `FALSE`
#'   will make the validation step inactive (still reporting its presence and
#'   keeping indexes for the steps unchanged). If the validation function will
#'   be operating directly on data (no *agent* involvement), then any step with
#'   `active = FALSE` will simply pass the data through with no validation
#'   whatsoever. Aside from a logical vector, a one-sided R formula using a
#'   leading `~` can be used with `.` (serving as the input data table) to
#'   evaluate to a single logical value. With this approach, the **pointblank**
#'   function [has_columns()] can be used to determine whether to make a
#'   validation step active on the basis of one or more columns existing in the
#'   table (e.g., `~ . %>% has_columns(vars(d, e))`). The default for `active`
#'   is `TRUE`.
#'   
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an *agent*
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#'   
#' @section Demos:
#' 
#' For all of the examples here, we'll use a simple table with three numeric
#' columns (`a`, `b`, and `c`) and three character columns (`d`, `e`, and `f`).
#' 
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 5, 5, 5, 5, 5),
#'     b = c(1, 1, 1, 2, 2, 2),
#'     c = c(1, 1, 1, 2, 3, 4),
#'     d = LETTERS[a],
#'     e = LETTERS[b],
#'     f = LETTERS[c]
#'   )
#'   
#' tbl
#' ```
#' 
#' ## A: Using an `agent` with validation functions and then `interrogate()`
#' 
#' Validate that values in column `a` are all greater than the value of `4`.
#' We'll determine if this validation had any failing test units (there are 6
#' test units, one for each row).
#' 
#' ```r
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(columns = vars(a), value = 4) %>%
#'   interrogate()
#' ```
#' 
#' Printing the `agent` in the console shows the validation report in the
#' Viewer. Here is an excerpt of validation report, showing the single entry
#' that corresponds to the validation step demonstrated here.
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_col_vals_gt_1.png")`
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
#' tbl %>% col_vals_gt(columns = vars(a), value = 4)
#' ```
#'   
#' ## C: Using the expectation function
#' 
#' With the `expect_*()` form, we would typically perform one validation at a
#' time. This is primarily used in **testthat** tests.
#' 
#' ```r
#' expect_col_vals_gt(tbl, columns = vars(a), value = 4)
#' ```
#' 
#' ## D: Using the test function
#' 
#' With the `test_*()` form, we should get a single logical value returned to
#' us.
#' 
#' ```{r}
#' test_col_vals_gt(tbl, columns = vars(a), value = 4)
#' ```
#' 
#' @family validation functions
#' @section Function ID:
#' 2-6
#' 
#' @seealso The analogous function with a left-closed bound: [col_vals_gte()].
#' 
#' @name col_vals_gt
NULL

#' @rdname col_vals_gt
#' @import rlang
#' @export
col_vals_gt <- function(
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

  # Get `columns` as a label
  columns_expr <- 
    rlang::as_label(rlang::quo(!!enquo(columns))) %>%
    gsub("^\"|\"$", "", .)
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
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
      col_vals_gt(
        columns = columns,
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
  
  if (is.null(brief)) {
    
    brief <- 
      generate_autobriefs(
        agent = agent,
        columns = columns,
        preconditions = preconditions,
        values = value,
        assertion_type = "col_vals_gt"
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
  for (i in seq_along(columns)) {
    for (j in seq_along(segments_list)) {
      
      seg_col <- names(segments_list[j])
      seg_val <- unname(unlist(segments_list[j]))
      
      agent <-
        create_validation_step(
          agent = agent,
          assertion_type = "col_vals_gt",
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
          label = label,
          brief = brief[i],
          active = active
        )
    }
  }

  agent
}

#' @rdname col_vals_gt
#' @import rlang
#' @export
expect_col_vals_gt <- function(
    object,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {
  
  fn_name <- "expect_col_vals_gt"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_gt(
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
  operator <- ">"
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

#' @rdname col_vals_gt
#' @import rlang
#' @export
test_col_vals_gt <- function(
    object,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_gt(
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
