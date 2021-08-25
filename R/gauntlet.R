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

#' Run the gauntlet with a series of validation functions
#'
#' @description 
#' The `gauntlet()` validation function runs validation step in sequence,
#' providing an opportunity to exit the validation pathway at any step where
#' test units fail beyond a set *stop* threshold.
#' 
#' In practice, an example of where a `gauntlet()` ensemble validation is useful
#' is when subsequent steps would fail if prior steps didn't pass to a known
#' degree.
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
#' the target table table during interrogation as a preparatory step. It might
#' happen that a particular validation requires a calculated column, some
#' filtering of rows, or the addition of columns via a join, etc. Especially for
#' an *agent*-based report this can be advantageous since we can develop a large
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
#' `gauntlet()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `gauntlet()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' ```
#' # R statement
#' agent %>% 
#'   gauntlet(
#'     ~ col_vals_lt(., vars(a), 8),
#'     ~ col_vals_gt(., vars(c), vars(a)),
#'     ~ col_vals_not_null(., vars(b)),
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     segments = b ~ c("group_1", "group_2"),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "The `gauntlet()` step.",
#'     active = FALSE
#'   )
#' 
#' # YAML representation
#' steps:
#' - gauntlet:
#'     fns:
#'     - ~col_vals_lt(., vars(a), 8)
#'     - ~col_vals_gt(., vars(c), vars(a))
#'     - ~col_vals_not_null(., vars(b))
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     segments: b ~ c("group_1", "group_2")
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `gauntlet()` step.
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
#' @param ... a collection one-sided formulas that consist of `test_*()`
#'   function calls (e.g., [test_col_vals_between()], etc.) arranged in sequence
#'   of intended interrogation order. Typically, validations up until
#'   the final one would have some `threshold` value set (default is `1`) for
#'   short circuiting within the gauntlet. A finishing validation function
#'   call (e.g., [col_vals_increasing()], etc.) can optionally be inserted at
#'   the end of the gauntlet, serving as the main validation step that only
#'   undergoes interrogation if prior steps adequately pass. An example of this
#'   is `~ test_column_exists(., vars(a)), ~ col_vals_not_null(., vars(a))`).
#' @param .list Allows for the use of a list as an input alternative to `...`.
#'
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#'
#'
#' @family validation functions
#' @section Function ID:
#' 2-21
#' 
#' @export
gauntlet <- function(x,
                     ...,
                     .list = list2(...),
                     preconditions = NULL,
                     segments = NULL,
                     actions = NULL,
                     step_id = NULL,
                     label = NULL,
                     brief = NULL,
                     active = TRUE) {
  
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
      gauntlet(
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
        assertion_type = "gauntlet",
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
  for (i in seq_along(segments_list)) {
    
    seg_col <- names(segments_list[i])
    seg_val <- unname(unlist(segments_list[i]))
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "gauntlet",
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

