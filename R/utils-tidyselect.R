resolve_columns <- function(x, var_expr, preconditions = NULL, ...,
                            call = rlang::caller_env()) {
  
  # Materialize table and apply preconditions for tidyselect
  tbl <- apply_preconditions_for_cols(x, preconditions)
  
  # If tbl cannot (yet) materialize, don't attempt tidyselect and return early
  if (is.null(tbl)) {
    return(resolve_columns_notidyselect(var_expr))
  }
  
  # Attempt tidyselect
  out <- tryCatch(
    expr = resolve_columns_internal(tbl, var_expr, ..., call = call),
    error = function(cnd) cnd
  )
  
  if (rlang::is_error(out)) {
    # If error is a genuine evaluation error, throw that error
    if (!is_subscript_error(out)) {
      rlang::cnd_signal(rlang::error_cnd("resolve_eval_err", parent = out))
    }
    # If not in validation-planning context (assert/expect/test), rethrow
    if (is_a_table_object(x) || is_secret_agent(x)) {
      rlang::cnd_signal(out)
    } else {
      # Else (mid-planning): grab columns attempted to subset
      fail <- out$i %||% out$parent$i
      # If failure is due to scoping a bad object in the env, re-throw error
      if (!is.character(fail) && !rlang::is_integerish(fail)) {
        rlang::cnd_signal(out)
      }
      success <- resolve_columns_possible(tbl, var_expr)
      out <- c(success, fail) %||% NA_character_
    }
  }
  
  out
  
}

resolve_columns_notidyselect <- function(var_expr, call) {
  # Error if attempting to tidyselect on a lazy tbl that cannot materialize
  var_str <- rlang::expr_deparse(rlang::quo_get_expr(var_expr))
  if (any(sapply(names(tidyselect::vars_select_helpers), grepl, var_str))) {
    rlang::abort(
      "Cannot use tidyselect helpers for an undefined lazy tbl.",
      call = call
    )
  }
  
  # Force column selection to character vector
  if (rlang::quo_is_symbol(var_expr)) {
    var_expr <- rlang::as_name(var_expr)
  } else if (rlang::quo_is_call(var_expr, c("vars", "c"))) {
    var_expr <- rlang::quo_set_expr(var_expr, vars_to_c(var_expr))
  }
  rlang::eval_tidy(var_expr)
}

# Apply the preconditions function and resolve to data frame for tidyselect
apply_preconditions_for_cols <- function(x, preconditions) {
  # Extract tbl
  tbl <- if (is_ptblank_agent(x)) {
    tryCatch(get_tbl_object(x), error = function(...) NULL)
  } else if (is_a_table_object(x)) {
    x
  }
  # Apply preconditions
  if (!is.null(tbl) && !is.null(preconditions)) {
    tbl <- apply_preconditions(tbl = tbl, preconditions = preconditions)
  }
  tbl
}

# Determines whether the error from a tidyselect expression is from attempting
# to select a non-existing column (i.e., a "subscript error")
is_subscript_error <- function(cnd) {
  is.null(cnd$parent) || inherits(cnd$parent, "vctrs_error_subscript")
}

# If selection gets short-circuited by error, re-run with `strict = FALSE`
# to safely get the possible column selections
resolve_columns_possible <- function(tbl, var_expr) {
  success <- tryCatch(
    names(tidyselect::eval_select(var_expr, tbl,
                                  strict = FALSE, allow_empty = FALSE)),
    error = function(cnd) NULL
  )
  success
}

# Resolve column selections to integer
resolve_columns_internal <- function(tbl, var_expr, ..., call) {
  
  # Return NA if the expr is NULL
  if (rlang::quo_is_null(var_expr)) {
    return(NA_character_)
  }
  
  # Special case `serially()`: just deparse elements and bypass tidyselect
  if (rlang::is_empty(tbl)) {
    var_expr <- rlang::quo_get_expr(var_expr)
    if (rlang::is_symbol(var_expr) || rlang::is_scalar_character(var_expr)) {
      column <- rlang::as_name(var_expr)
    } else {
      cols <- rlang::call_args(var_expr)
      column <- vapply(cols, rlang::as_name, character(1), USE.NAMES = FALSE)
    }
    return(column)
  }
  # Special case `vars()`-expression for backwards compatibility
  if (rlang::quo_is_call(var_expr, "vars")) {
    var_expr <- rlang::quo_set_expr(var_expr, vars_to_c(var_expr))
  }
  
  # Proceed with tidyselect
  column <- tidyselect::eval_select(var_expr, tbl, error_call = call, ...)
  column <- names(column)
  
  if (length(column) < 1) {
    column <- NA_character_
  }
  
  column
}

# Convert to the idiomatic `c()`-expr before passing off to tidyselect
# + ensure that vars() always scopes symbols to data (vars(a) -> c("a"))
vars_to_c <- function(var_expr) {
  var_args <- lapply(rlang::call_args(var_expr), function(var_arg) {
    if (rlang::is_symbol(var_arg)) rlang::as_name(var_arg) else var_arg
  })
  c_expr <- rlang::call2("c", !!!var_args)
  c_expr
}
