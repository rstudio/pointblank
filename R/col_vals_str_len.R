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
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


#' Are string lengths in column data within a specified range?
#'
#' @description
#'
#' The `col_vals_str_len()` validation function, the
#' `expect_col_vals_str_len()` expectation function, and the
#' `test_col_vals_str_len()` test function all check whether string lengths
#' of column values in a table fall within a specified range defined by `min`
#' and `max`. The validation function can be used directly on a data table or
#' with an *agent* object (technically, a `ptblank_agent` object) whereas the
#' expectation and test functions can only be used with a data table. Each
#' validation step or expectation will operate over the number of test units
#' that is equal to the number of rows in the table (after any `preconditions`
#' have been applied).
#'
#' @inheritParams col_vals_gt
#'
#' @param min *Minimum string length*
#'
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#'
#'   The minimum acceptable string length (inclusive). If `NULL`, no lower
#'   bound is applied. At least one of `min` or `max` must be provided.
#'
#' @param max *Maximum string length*
#'
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#'
#'   The maximum acceptable string length (inclusive). If `NULL`, no upper
#'   bound is applied. At least one of `min` or `max` must be provided.
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
#' Create a simple table with a character column.
#'
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'     id = c("AB", "CDE", "FGHI", "JK"),
#'     value = c(1, 2, 3, 4)
#'   )
#'
#' tbl
#' ```
#'
#' Validate that string lengths in column `id` are between 2 and 4 characters.
#'
#' ```r
#' agent <-
#'   create_agent(tbl = tbl) |>
#'   col_vals_str_len(columns = id, min = 2, max = 4) |>
#'   interrogate()
#' ```
#'
#' Determine if this validation step passed by using `all_passed()`.
#'
#' ```r
#' all_passed(agent)
#' ```
#'
#' ```
#' ## [1] TRUE
#' ```
#'
#' @family Validation Functions
#' @section Function ID:
#' 2-18
#'
#' @name col_vals_str_len
NULL

#' @rdname col_vals_str_len
#' @import rlang
#' @export
col_vals_str_len <- function(
    x,
    columns,
    min = NULL,
    max = NULL,
    na_pass = FALSE,
    preconditions = NULL,
    segments = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {

  if (is.null(min) && is.null(max)) {
    stop("At least one of `min` or `max` must be provided.", call. = FALSE)
  }

  columns <- rlang::enquo(columns)
  columns_expr <- as_columns_expr(columns)

  columns <- resolve_columns(x = x, var_expr = columns, preconditions)

  segments_list <-
    resolve_segments(
      x = x,
      seg_expr = segments,
      preconditions = preconditions
    )

  if (is_a_table_object(x)) {

    secret_agent <-
      create_agent(x, label = "::QUIET::") |>
      col_vals_str_len(
        columns = tidyselect::all_of(columns),
        min = min,
        max = max,
        na_pass = na_pass,
        preconditions = preconditions,
        segments = segments,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) |>
      interrogate()

    return(x)
  }

  agent <- x

  str_len_values <- list(min = min, max = max)

  brief <- resolve_brief(
    brief = brief, agent = agent,
    columns = columns, segments_list = segments_list,
    preconditions = preconditions, values = str_len_values,
    assertion_type = "col_vals_str_len"
  )

  step_id <- normalize_step_id(step_id, columns, agent)

  i_o <- get_next_validation_set_row(agent)

  check_step_id_duplicates(step_id, agent)

  label <- resolve_label(label, columns, segments_list)
  for (i in seq_along(columns)) {
    for (j in seq_along(segments_list)) {

      seg_col <- names(segments_list[j])
      seg_val <- unname(unlist(segments_list[j]))

      agent <-
        create_validation_step(
          agent = agent,
          assertion_type = "col_vals_str_len",
          i_o = i_o,
          columns_expr = columns_expr,
          column = columns[i],
          values = str_len_values,
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

#' @rdname col_vals_str_len
#' @import rlang
#' @export
expect_col_vals_str_len <- function(
    object,
    columns,
    min = NULL,
    max = NULL,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {

  fn_name <- "expect_col_vals_str_len"

  vs <-
    create_agent(tbl = object, label = "::QUIET::") |>
    col_vals_str_len(
      columns = {{ columns }},
      min = min,
      max = max,
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) |>
    interrogate() |>
    (\(x) x$validation_set)()

  x <- vs$notify

  threshold_type <- get_threshold_type(threshold = threshold)

  if (threshold_type == "proportional") {
    failed_amount <- vs$f_failed
  } else {
    failed_amount <- vs$n_failed
  }

  if (length(x) > 1 && any(x)) {

    fail_idx <- which(x)[1]

    failed_amount <- failed_amount[fail_idx]

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
  str_len_vals <- vs$values[[fail_idx]]
  value_1 <- if (!is.null(str_len_vals$min)) str_len_vals$min else "
"
  value_2 <- if (!is.null(str_len_vals$max)) str_len_vals$max else "
"

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

#' @rdname col_vals_str_len
#' @import rlang
#' @export
test_col_vals_str_len <- function(
    object,
    columns,
    min = NULL,
    max = NULL,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {

  vs <-
    create_agent(tbl = object, label = "::QUIET::") |>
    col_vals_str_len(
      columns = {{ columns }},
      min = min,
      max = max,
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) |>
    interrogate() |>
    (\(x) x$validation_set)()

  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }

  all(!vs$notify)
}
