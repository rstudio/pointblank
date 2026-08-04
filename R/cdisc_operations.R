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

# Apply all operations to df, returning an enriched data frame with computed
# columns. Operations are executed sequentially before conditions run.
cdisc_apply_operations <- function(df, operations, ct, datasets,
                                   define_meta = NULL) {
  for (op in operations) {
    operator <- op$operator %||% ""
    handler <- .cdisc_op_registry[[operator]]
    if (is.null(handler)) next
    df <- tryCatch(
      handler(df, op$params %||% list(), ct, datasets, define_meta),
      error = function(e) df
    )
  }
  df
}

# ── Operation handlers ───────────────────────────────────────────────────────

# _pb_<col>_valid: TRUE = value in codelist, NA, or empty string
.cdisc_op_codelist_check <- function(df, params, ct, datasets,
                                     define_meta = NULL) {
  col <- params$column
  result_col <- paste0("_pb_", col, "_valid")

  if (!col %in% names(df)) {
    df[[result_col]] <- TRUE
    return(df)
  }

  terms <- cdisc_ct_get_codelist(ct, params$codelist)
  if (is.null(terms)) {
    df[[result_col]] <- TRUE
    return(df)
  }

  upper_terms <- toupper(terms)
  v <- as.character(df[[col]])
  df[[result_col]] <- ifelse(
    is.na(v) | v == "",
    TRUE,
    toupper(v) %in% upper_terms
  )
  df
}

# _pb_<col>_consistent: TRUE = value equals the mode, or NA
.cdisc_op_consistency_check <- function(df, params, ct, datasets,
                                        define_meta = NULL) {
  col <- params$column
  result_col <- paste0("_pb_", col, "_consistent")

  if (!col %in% names(df)) {
    df[[result_col]] <- TRUE
    return(df)
  }

  v <- as.character(df[[col]])
  non_empty <- v[!is.na(v) & v != ""]

  if (length(non_empty) == 0L) {
    df[[result_col]] <- TRUE
    return(df)
  }

  freq <- sort(table(non_empty), decreasing = TRUE)
  expected <- names(freq)[1]
  df[[result_col]] <- ifelse(is.na(v) | v == "", TRUE, v == expected)
  df
}

# _pb_<col>_iso8601: TRUE = valid ISO 8601 partial/complete datetime or NA
.cdisc_op_iso8601_check <- function(df, params, ct, datasets,
                                    define_meta = NULL) {
  col <- params$column
  result_col <- paste0("_pb_", col, "_iso8601")

  if (!col %in% names(df)) {
    df[[result_col]] <- TRUE
    return(df)
  }

  v <- as.character(df[[col]])
  df[[result_col]] <- ifelse(
    is.na(v) | v == "",
    TRUE,
    vapply(v, function(x) cdisc_is_iso8601(x), logical(1), USE.NAMES = FALSE)
  )
  df
}

# _pb_<col>_unique: TRUE = value is unique within the USUBJID group
.cdisc_op_unique_per_subject <- function(df, params, ct, datasets,
                                         define_meta = NULL) {
  col <- params$column
  result_col <- paste0("_pb_", col, "_unique")

  if (!col %in% names(df) || !"USUBJID" %in% names(df)) {
    df[[result_col]] <- TRUE
    return(df)
  }

  # Build composite keys and find duplicates
  keys <- paste(df[["USUBJID"]], df[[col]], sep = "\r")
  key_counts <- table(keys)
  dup_keys <- names(key_counts[key_counts > 1L])
  df[[result_col]] <- !(keys %in% dup_keys)
  df
}

# _pb_<col>_present: broadcast scalar, TRUE = column exists in the dataset
.cdisc_op_column_presence <- function(df, params, ct, datasets,
                                      define_meta = NULL) {
  col <- params$column
  result_col <- paste0("_pb_", col, "_present")
  df[[result_col]] <- col %in% names(df)
  df
}

# Batch column_presence for a list of variables
.cdisc_op_has_required_variables <- function(df, params, ct, datasets,
                                             define_meta = NULL) {
  variables <- params$variables %||% list()
  for (col in variables) {
    result_col <- paste0("_pb_", col, "_present")
    df[[result_col]] <- col %in% names(df)
  }
  df
}

# _pb_variable_order_valid: broadcast scalar, TRUE = variables in expected order
.cdisc_op_valid_variable_order <- function(df, params, ct, datasets,
                                           define_meta = NULL) {
  expected <- params$expected_order %||% list()
  expected <- as.character(unlist(expected, use.names = FALSE))
  col_names <- names(df)

  # Filter to variables that are actually present and get their positions
  present <- expected[expected %in% col_names]
  valid <- TRUE

  if (length(present) >= 2L) {
    positions <- match(present, col_names)
    for (i in seq_len(length(positions) - 1L)) {
      if (positions[i] > positions[i + 1L]) {
        valid <- FALSE
        break
      }
    }
  }

  df[["_pb_variable_order_valid"]] <- valid
  df
}

# _pb_<col>_type_valid: broadcast scalar, TRUE = dtype matches expected category
.cdisc_op_variable_type_check <- function(df, params, ct, datasets,
                                          define_meta = NULL) {
  col <- params$column
  expected <- tolower(params$expected_type %||% "character")
  result_col <- paste0("_pb_", col, "_type_valid")

  if (!col %in% names(df)) {
    df[[result_col]] <- TRUE
    return(df)
  }

  col_data <- df[[col]]
  is_num <- is.numeric(col_data)
  is_char <- is.character(col_data) || is.factor(col_data)

  valid <- if (expected == "numeric") {
    is_num
  } else if (expected == "character") {
    is_char
  } else {
    TRUE
  }

  df[[result_col]] <- valid
  df
}

# ── Define-XML-aware operations (stubs: always pass) ─────────────────────────
# R has no Define-XML importer yet; these match Python's define_meta=None
# fallback. The stubs still add the expected _pb_* columns so conditions
# referencing them don't error.

.cdisc_op_define_var_declared <- function(df, params, ct, datasets,
                                          define_meta = NULL) {
  df[[paste0("_pb_", params$column, "_in_define")]] <- TRUE
  df
}

.cdisc_op_define_required_check <- function(df, params, ct, datasets,
                                            define_meta = NULL) {
  df[[paste0("_pb_", params$column, "_mandatory_ok")]] <- TRUE
  df
}

.cdisc_op_define_codelist_check <- function(df, params, ct, datasets,
                                            define_meta = NULL) {
  df[[paste0("_pb_", params$column, "_define_valid")]] <- TRUE
  df
}

.cdisc_op_define_type_check <- function(df, params, ct, datasets,
                                        define_meta = NULL) {
  df[[paste0("_pb_", params$column, "_define_type_ok")]] <- TRUE
  df
}

# ── Operation registry ───────────────────────────────────────────────────────

.cdisc_op_registry <- list(
  codelist_check         = .cdisc_op_codelist_check,
  consistency_check      = .cdisc_op_consistency_check,
  iso8601_check          = .cdisc_op_iso8601_check,
  unique_per_subject     = .cdisc_op_unique_per_subject,
  column_presence        = .cdisc_op_column_presence,
  has_required_variables = .cdisc_op_has_required_variables,
  valid_variable_order   = .cdisc_op_valid_variable_order,
  variable_type_check    = .cdisc_op_variable_type_check,
  define_var_declared    = .cdisc_op_define_var_declared,
  define_required_check  = .cdisc_op_define_required_check,
  define_codelist_check  = .cdisc_op_define_codelist_check,
  define_type_check      = .cdisc_op_define_type_check
)
