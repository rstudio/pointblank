#' Set action levels for reacting to exceeding thresholds
#' 
#' This helper function works with the `actions` argument that is present in
#' every validation step function. With it, we can provide threshold *fail*
#' levels for any combination of `warn`, `stop`, or `notify` states. We can
#' react to any entrance of a state by supplying corresponding functions to the
#' `fns` argument. They will undergo evaluation at the time when the matching
#' state is entered.
#' 
#' @param warn_at,stop_at,notify_at The threshold number or fraction of
#'   validation units that can provide a *fail* result before entering the
#'   `warn`, `stop`, or `notify` states.
#' @param fns A named list of functions that can be used with each action type.
#'   The syntax for this list involves using names from the set of `warn`,
#'   `stop`, and `notify`. The functions corresponding to the states are
#'   provided as formulas (e.g., `list(warn = ~ warning("Too many failures."))`.
#'   A series of expressions for each named state can be used by enclosing the
#'   set of statements with `{ }`.
#' 
#' @examples 
#' # Create a simple data frame with
#' # a column of numerical values
#' df <- data.frame(a = c(5, 7, 8, 5))
#' 
#' # Create an `action_levels()` list
#' # with fractional values for the
#' # `warn`, `stop`, and `notify` states
#' al <- 
#'   action_levels(
#'     warn_at = 0.2,
#'     stop_at = 0.8,
#'     notify_at = 0.345
#'   )
#' 
#' # Validate that values in column
#' # `a` are always greater than 7 and
#' # apply the list of action levels
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_gt(vars(a), 7, actions = al) %>%
#'   interrogate()
#'
#' # The report from the agent will
#' # show that the `warn` state has
#' # been entered for the first and
#' # only validation step
#' agent %>%
#'   get_agent_report(display_table = FALSE)
#'   
#' @export
action_levels <- function(warn_at = NULL,
                          stop_at = NULL,
                          notify_at = NULL,
                          fns = NULL) {
  
  fns <- normalize_fns_list(fns = fns)
  
  warn_list <- normalize_fraction_count(warn_at)
  stop_list <- normalize_fraction_count(stop_at)
  notify_list <- normalize_fraction_count(notify_at)
  
  list(
    warn_fraction = warn_list$fraction,
    warn_count = warn_list$count,
    stop_fraction = stop_list$fraction,
    stop_count = stop_list$count,
    notify_fraction = notify_list$fraction,
    notify_count = notify_list$count,
    fns = fns
  )
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
    stop("All components of the `fns` list must be formulas.", call. = FALSE)
  }
  
  if ("" %in% names(fns)) {
    stop("The `fns` list must be fully named.", call. = FALSE)
  }
  
  if (!all(names(fns) %in% c("warn", "stop", "notify"))) {
    stop("All names in the `fns` list must be one of `warn`, `stop`, or `notify`.",
         call. = FALSE)
  }
  
  fns
}

normalize_fraction_count <- function(x) {
  
  if (!is.null(x) && !any(c(inherits(x, "numeric"), inherits(x, "integer")))) {
    stop("All values provided to `action_levels()` must be either `numeric` or `integer` types.",
         call. = FALSE)
  }
  
  if (!is.null(x) && x <= 0) {
    stop("All values provided to `action_levels()` must be `>=0`.", call. = FALSE)
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
      actions$fns$warn <- ~ stock_warning(vl = .vars_list)
    }
    if (is.null(actions$fns$stop)) {
      actions$fns$stop <- ~ stock_stoppage(vl = .vars_list)
    }
  }
  
  actions
}

stock_stoppage <- function(vl) {
  stop("The validation (`", vl$type, "()`) meets or exceeds the stop threshold",
       call. = FALSE)
}

stock_warning <- function(vl) {
  warning("The validation (`", vl$type, "()`) meets or exceeds the warn threshold",
          call. = FALSE)
}
