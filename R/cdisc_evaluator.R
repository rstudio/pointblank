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

# Evaluate a condition tree against a data frame, returning a logical vector.
# TRUE = row matches the violation condition.
# Returns all-FALSE when the condition tree is empty.
cdisc_evaluate_conditions <- function(df, conditions) {

  if (length(conditions) == 0L) {
    return(rep(FALSE, nrow(df)))
  }

  tryCatch(
    .cdisc_compile(conditions, df),
    error = function(e) {
      rlang::abort(
        paste("Condition evaluation error:", conditionMessage(e)),
        class = "cdisc_evaluation_error"
      )
    }
  )
}

# Recursive condition tree compiler -> logical vector
.cdisc_compile <- function(cond, df) {

  if (!is.null(cond[["all"]])) {
    subs <- lapply(cond[["all"]], .cdisc_compile, df = df)
    return(Reduce(`&`, subs))
  }

  if (!is.null(cond[["any"]])) {
    subs <- lapply(cond[["any"]], .cdisc_compile, df = df)
    return(Reduce(`|`, subs))
  }

  if (!is.null(cond[["not"]])) {
    return(!.cdisc_compile(cond[["not"]], df))
  }

  .cdisc_compile_leaf(cond, df)
}

# Compile a single leaf condition to a logical vector
.cdisc_compile_leaf <- function(cond, df) {

  name <- cond$name
  op <- cond$operator
  value <- cond$value

  if (!name %in% names(df)) {
    rlang::abort(sprintf("Column not found: %s", name))
  }

  col <- df[[name]]

  # Normalize factors to character for reliable comparisons
  if (is.factor(col)) {
    col <- as.character(col)
  }

  result <- switch(
    op,
    "is_null" = is.na(col),
    "is_not_null" = !is.na(col),
    "equal_to" = .cdisc_safe_eq(col, value),
    "not_equal_to" = .cdisc_safe_neq(col, value),
    "greater_than" = col > value,
    "greater_than_or_equal_to" = col >= value,
    "less_than" = col < value,
    "less_than_or_equal_to" = col <= value,
    "contains" = grepl(as.character(value), as.character(col), fixed = TRUE),
    "not_contains" = !grepl(
      as.character(value), as.character(col), fixed = TRUE
    ),
    "starts_with" = startsWith(as.character(col), as.character(value)),
    "ends_with" = endsWith(as.character(col), as.character(value)),
    "is_in" = col %in% unlist(value, use.names = FALSE),
    "not_in" = !(col %in% unlist(value, use.names = FALSE)),
    "matches_regex" = grepl(
      as.character(value), as.character(col), perl = TRUE
    ),
    "equal_to_column" = .cdisc_safe_eq(col, df[[as.character(value)]]),
    "not_equal_to_column" = .cdisc_safe_neq(col, df[[as.character(value)]]),
    rlang::abort(sprintf("Unknown operator: %s", op))
  )

  result
}

# NA-safe equality: NA == x returns NA in R, but we want NA to propagate
# (matching narwhals/Python null semantics). The caller uses which() to
# extract TRUE-only indices, so NA rows are naturally excluded.
.cdisc_safe_eq <- function(a, b) {
  a == b
}

.cdisc_safe_neq <- function(a, b) {
  a != b
}

# ── ISO 8601 date helpers ────────────────────────────────────────────────────

# Allows YYYY, YYYY-MM, YYYY-MM-DD, YYYY-MM-DDTHH:MM, YYYY-MM-DDTHH:MM:SS,
# with optional timezone offset or Z. Partial dates are permitted by SDTM.
.cdisc_iso8601_re <- paste0(
  "^\\d{4}",
  "(-\\d{2}",
  "(-\\d{2}",
  "(T\\d{2}:\\d{2}",
  "(:\\d{2}",
  "(\\.\\d+)?",
  ")?",
  "(Z|[+-]\\d{2}:\\d{2})?",
  ")?",
  ")?",
  ")?$"
)

cdisc_is_iso8601 <- function(value) {
  if (is.na(value) || !nzchar(as.character(value))) {
    return(FALSE)
  }
  grepl(.cdisc_iso8601_re, trimws(as.character(value)), perl = TRUE)
}
