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


#' Set action levels: failure thresholds and functions to invoke
#'
#' @description
#'
#' The `action_levels()` function works with the `actions` argument that is
#' present in the [create_agent()] function and in every validation step
#' function (which also has an `actions` argument). With it, we can provide
#' threshold *failure* values for any combination of `warn`, `stop`, or `notify`
#' failure states.
#'
#' We can react to any entering of a state by supplying corresponding functions
#' to the `fns` argument. They will undergo evaluation at the time when the
#' matching state is entered. If provided to [create_agent()] then the policies
#' will be applied to every validation step, acting as a default for the
#' validation as a whole.
#'
#' Calls of `action_levels()` could also be applied directly to any validation
#' step and this will act as an override if set also in [create_agent()]. Usage
#' of `action_levels()` is required to have any useful side effects (i.e.,
#' warnings, throwing errors) in the case of validation functions operating
#' directly on data (e.g., `mtcars %>% col_vals_lt("mpg", 35)`). There are two
#' helper functions that are convenient when using validation functions directly
#' on data (the `agent`-less workflow): `warn_on_fail()` and `stop_on_fail()`.
#' These helpers either warn or stop (default failure threshold for each is set
#' to `1`), and, they do so with informative warning or error messages. The
#' `stop_on_fail()` helper is applied by default when using validation functions
#' directly on data (more information on this is provided in *Details*).
#'
#' @details
#'
#' The output of the `action_levels()` call in `actions` will be interpreted
#' slightly differently if using an *agent* or using validation functions
#' directly on a data table. For convenience, when working directly on data, any
#' values supplied to `warn_at` or `stop_at` will be automatically given a stock
#' `warning()` or `stop()` function. For example using
#' `small_table %>% col_is_integer("date")` will provide a detailed stop message
#' by default, indicating the reason for the failure. If you were to supply the
#' `fns` for `stop` or `warn` manually then the stock functions would be
#' overridden. Furthermore, if `actions` is NULL in this workflow (the default),
#' **pointblank** will use a `stop_at` value of `1` (providing a detailed,
#' context-specific error message if there are any *failing* units). We can
#' absolutely suppress this automatic stopping behavior at each validation
#' step by setting `active = FALSE`. In this interactive data case, there is no
#' stock function given for `notify_at`. The `notify` failure state is less
#' commonly used in this workflow as it is in the *agent*-based one.
#'
#' When using an *agent*, we often opt to not use any functions in `fns` as the
#' `warn`, `stop`, and `notify` failure states will be reported on when using
#' `create_agent_report()` (and, usually that's sufficient). Instead, using the
#' `end_fns` argument is a better choice since that scheme provides useful data
#' on the entire interrogation, allowing for finer control on side effects and
#' reducing potential for duplicating any side effects.
#'
#' @param warn_at *Threshold value for the 'warn' failure state*
#'
#'   `scalar<integer|numeric>(val>=0)` // *default:* `NULL` (`optional`)
#'
#'   Either the threshold number or the threshold fraction of *failing* test
#'   units that result in entering the `warn` failure state.
#'
#' @param stop_at *Threshold value for the 'stop' failure state*
#'
#'   `scalar<integer|numeric>(val>=0)` // *default:* `NULL` (`optional`)
#'
#'   Either the threshold number or the threshold fraction of *failing* test
#'   units that result in entering the `stop` failure state.
#'
#' @param notify_at *Threshold value for the 'notify' failure state*
#'
#'   `scalar<integer|numeric>(val>=0)` // *default:* `NULL` (`optional`)
#'
#'   Either the threshold number or the threshold fraction of *failing* test
#'   units that result in entering the `notify` failure state.
#'
#' @param fns *Functions to execute when entering failure states*
#'
#'   `list` // *default:* `NULL` (`optional`)
#'
#'   A named list of functions that is to be paired with the appropriate failure
#'   states. The syntax for this list involves using failure state names from
#'   the set of `warn`, `stop`, and `notify`. The functions corresponding to the
#'   failure states are provided as formulas (e.g.,
#'   `list(warn = ~ warning("Too many failures."))`. A series of expressions for
#'   each named state can be used by enclosing the set of statements with `{ }`.
#'
#' @return An `action_levels` object.
#'
#' @section Defining threshold values:
#'
#' Any threshold values supplied for the `warn_at`, `stop_at`, or `notify_at`
#' arguments correspond to the `warn`, `stop`, and `notify` failure states,
#' respectively. A threshold value can either relates to an absolute number of
#' test units or a fraction-of-total test units that are *failing*. Exceeding
#' the threshold means entering one or more of the `warn`, `stop`, or `notify`
#' failure states.
#'
#' If a threshold value is a decimal value between `0` and `1` then it's a
#' proportional failure threshold (e.g., `0.15` indicates that if 15 percent of
#' the test units are found to be *failing*, then the designated failure state
#' is entered). Absolute values starting from `1` can be used instead, and this
#' constitutes an absolute failure threshold (e.g., `10` means that if 10 of the
#' test units are found to be *failing*, the failure state is entered).
#'
#' @section Examples:
#'
#' For these examples, we will use the included `small_table` dataset.
#'
#' ```{r}
#' small_table
#' ```
#'
#' Create an `action_levels` object with fractional values for the `warn`,
#' `stop`, and `notify` states.
#'
#' ```r
#' al <-
#'   action_levels(
#'     warn_at = 0.2,
#'     stop_at = 0.8,
#'     notify_at = 0.5
#'   )
#' ```
#'
#' A summary of settings for the `al` object is shown by printing it.
#'
#' Create a pointblank agent and apply the `al` object to `actions`. Add two
#' validation steps and interrogate the `small_table`.
#'
#' ```r
#' agent_1 <-
#'   create_agent(
#'     tbl = small_table,
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     columns = a, value = 2
#'   ) %>%
#'   col_vals_lt(
#'     columns = d, value = 20000
#'   ) %>%
#'   interrogate()
#' ```
#'
#' The report from the agent will show that the `warn` state has been entered
#' for the first validation step but not the second one. We can confirm this in
#' the console by inspecting the `warn` component in the agent's x-list.
#'
#' ```r
#' x_list <- get_agent_x_list(agent = agent_1)
#'
#' x_list$warn
#' ```
#'
#' ```
#' ## [1]  TRUE FALSE
#' ```
#'
#' Applying the `action_levels` object to the agent means that all validation
#' steps will inherit these settings but we can override this by applying
#' another such object to the validation step instead (this time using the
#' `warn_on_fail()` shorthand).
#'
#' ```r
#' agent_2 <-
#'   create_agent(
#'     tbl = small_table,
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     columns = a, value = 2,
#'     actions = warn_on_fail(warn_at = 0.5)
#'   ) %>%
#'   col_vals_lt(
#'     columns = d, value = 20000
#'   ) %>%
#'   interrogate()
#' ```
#'
#' In this case, the first validation step has a less stringent failure
#' threshold for the `warn` state and it's high enough that the condition is not
#' entered. This can be confirmed in the console through inspection of the
#' x-list `warn` component.
#'
#' ```r
#' x_list <- get_agent_x_list(agent = agent_2)
#'
#' x_list$warn
#' ```
#'
#' ```
#' ## [1] FALSE FALSE
#' ```
#'
#' In the context of using validation functions directly on data (i.e., no
#' involvement of an agent) we want to trigger warnings and raise errors. The
#' following will yield a warning if it is executed (returning the `small_table`
#' data).
#'
#' ```r
#' small_table %>%
#'   col_vals_gt(
#'     columns = a, value = 2,
#'     actions = warn_on_fail(warn_at = 2)
#'   )
#' ```
#'
#' \preformatted{## # A tibble: 13 × 8
#' ##    date_time           date           a b           c      d e
#' ##    <dttm>              <date>     <int> <chr>   <dbl>  <dbl> <lgl>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE
#' ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-…     7   834. TRUE
#' ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-…     8   108. FALSE
#' ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-…    NA  2230. TRUE
#' ## # … with 1 more variable: f <chr>
#' ## Warning message:
#' ## Exceedance of failed test units where values in `a` should have been >
#' ## `2`.
#' ## The `col_vals_gt()` validation failed beyond the absolute threshold
#' ## level (2).
#' ## * failure level (4) >= failure threshold (2)}
#'
#'
#'
#' With the same pipeline, not supplying anything for `actions` (it's `NULL` by
#' default) will have the same effect as using `stop_on_fail(stop_at = 1)`.
#'
#' ```r
#' small_table %>%
#'   col_vals_gt(columns = a, value = 2)
#' ```
#'
#' ```
#' ## Error: Exceedance of failed test units where values in `a` should have
#' ## been > `2`.
#' ## The `col_vals_gt()` validation failed beyond the absolute threshold
#' ## level (1).
#' ## * failure level (4) >= failure threshold (1)
#' ```
#'
#'
#' Here's the equivalent set of statements:
#'
#' ```r
#' small_table %>%
#'   col_vals_gt(
#'     columns = a, value = 2,
#'     actions = stop_on_fail(stop_at = 1)
#'   )
#' ```
#'
#' ```
#' ## Error: Exceedance of failed test units where values in `a` should have
#' ## been > `2`.
#' ## The `col_vals_gt()` validation failed beyond the absolute threshold
#' ## level (1).
#' ## * failure level (4) >= failure threshold (1)
#' ```
#'
#'
#' This is because the `stop_on_fail()` call is auto-injected in the default
#' case (when operating on data) for your convenience. Behind the scenes a
#' 'secret agent' uses 'covert actions': all so you can type less.
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-5
#'
#' @name action_levels
NULL

#' @rdname action_levels
#' @export
action_levels <- function(
    warn = NULL,
    error = NULL,
    critical = NULL,
    fns = NULL,
    ...,
    warn_at = NULL,
    stop_at = NULL,
    notify_at = NULL
) {

  fns <- normalize_fns_list(fns = fns)

  warn_list <- normalize_fraction_count(warn_at)
  stop_list <- normalize_fraction_count(stop_at)
  notify_list <- normalize_fraction_count(notify_at)

  action_levels <-
    list(
      warn_fraction = warn_list$fraction,
      warn_count = warn_list$count,
      stop_fraction = stop_list$fraction,
      stop_count = stop_list$count,
      notify_fraction = notify_list$fraction,
      notify_count = notify_list$count,
      fns = fns
    )

  # Assign the class attribute value `action_levels` to
  # the `action_levels` object
  attr(action_levels, "class") <- "action_levels"

  action_levels
}

#' @rdname action_levels
#' @export
warn_on_fail <- function(warn_at = 1) {
  action_levels(warn_at = warn_at, fns = list(warn = ~stock_warning(x = x)))
}

#' @rdname action_levels
#' @export
stop_on_fail <- function(stop_at = 1) {
  action_levels(stop_at = stop_at, fns = list(stop = ~stock_stoppage(x = x)))
}

normalize_fns_list <- function(fns) {

  if (is.null(fns) || (is.list(fns) && length(fns) == 0)) {
    return(list(warn = NULL, stop = NULL, notify = NULL))
  }

  are_formulas <-
    fns %>%
    vapply(
      FUN.VALUE = logical(1), USE.NAMES = FALSE,
      FUN = function(x) rlang::is_formula(x)
    )

  if (!all(are_formulas)) {

    stop(
      "All components of the `fns` list must be formulas.",
      call. = FALSE
    )
  }

  if ("" %in% names(fns)) {

    stop("The `fns` list must be fully named.", call. = FALSE)
  }

  if (!all(names(fns) %in% c("warn", "stop", "notify"))) {

    stop(
      "All names in the `fns` list must be one of `warn`, `stop`, or `notify`.",
      call. = FALSE
    )
  }

  fns
}

normalize_fraction_count <- function(x) {

  if (!is.null(x) && !any(c(inherits(x, "numeric"), inherits(x, "integer")))) {

    stop(
      "All values provided to `action_levels()` must be either ",
      "`numeric` or `integer` types.",
      call. = FALSE
    )
  }

  if (!is.null(x) && x <= 0) {

    stop(
      "All values provided to `action_levels()` must be `>=0`.",
      call. = FALSE
    )
  }

  if (!is.null(x)) {
    if (x < 1) {
      fraction <- x
      count <- NULL
    } else if (x >= 1) {
      count <- floor(x) %>% as.numeric()
      fraction <- NULL
    }
  } else {
    fraction <- NULL
    count <- NULL
  }

  list(fraction = fraction, count = count)
}

prime_actions <- function(actions) {

  if (!is.null(actions)) {
    if (is.null(actions$fns$warn)) {
      actions$fns$warn <- ~stock_warning(x = x)
    }
    if (is.null(actions$fns$stop)) {
      actions$fns$stop <- ~stock_stoppage(x = x)
    }
  } else {
    actions <-
      action_levels(
        stop_at = 1,
        fns = list(stop = ~stock_stoppage(x = x))
      )
  }

  actions
}

covert_actions <- function(actions, agent) {

  if (is.null(actions)) {
    actions <- agent$actions
  }

  actions
}

stock_stoppage <- function(x) {

  fn_name <- x$type
  column_text <- prep_column_text(x$column)
  values_text <- prep_values_text(values = x$values, limit = 3, lang = "en")
  operator <- prep_operator_text(fn_name = x$type)

  if (grepl("between", fn_name)) {
    value_1 <- prep_values_text(x$values) %>% tidy_gsub(",.*", "")
    value_2 <- prep_values_text(x$values) %>% tidy_gsub(".*, ", "")
  }

  if (grepl("col_is", fn_name)) {
    col_type <- prep_col_type(fn_name = fn_name)
  }

  if (!is.null(x$actions$stop_count)) {
    threshold <- x$actions$stop_count
    failed_amount <- x$n_failed
    threshold_type <- "absolute"
  } else if (!is.null(x$actions$stop_fraction)) {
    threshold <- x$actions$stop_fraction
    failed_amount <- x$f_failed
    threshold_type <- "proportional"
  }

  failure_message <-
    glue::glue(failure_message_gluestring(fn_name = fn_name, lang = "en"))

  stop(failure_message, call. = FALSE)
}

stock_warning <- function(x) {

  fn_name <- x$type
  column_text <- prep_column_text(x$column)
  values_text <- prep_values_text(values = x$values, limit = 3, lang = "en")
  operator <- prep_operator_text(fn_name = x$type)

  if (grepl("between", fn_name)) {
    value_1 <- prep_values_text(x$values) %>% tidy_gsub(",.*", "")
    value_2 <- prep_values_text(x$values) %>% tidy_gsub(".*, ", "")
  }

  if (grepl("col_is", fn_name)) {
    col_type <- prep_col_type(fn_name = fn_name)
  }

  if (!is.null(x$actions$warn_count)) {
    threshold <- x$actions$warn_count
    failed_amount <- x$n_failed
    threshold_type <- "absolute"
  } else if (!is.null(x$actions$warn_fraction)) {
    threshold <- x$actions$warn_fraction
    failed_amount <- x$f_failed
    threshold_type <- "proportional"
  }

  failure_message <-
    glue::glue(failure_message_gluestring(fn_name = fn_name, lang = "en"))

  warning(failure_message, call. = FALSE)
}
