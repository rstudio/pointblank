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

# Rule execution statuses
CDISC_STATUS_PASS <- "pass"
CDISC_STATUS_FAIL <- "fail"
CDISC_STATUS_ERROR <- "error"
CDISC_STATUS_NOT_APPLICABLE <- "not_applicable"
CDISC_STATUS_NOT_SUPPORTED <- "not_supported"

# Internal: construct a row-level finding
cdisc_row_finding <- function(
    rule_id,
    dataset,
    row,
    usubjid,
    checked_column,
    checked_value,
    context,
    message = NULL
) {
  list(
    rule_id = rule_id,
    dataset = dataset,
    row = row,
    usubjid = usubjid,
    checked_column = checked_column,
    checked_value = checked_value,
    context = context,
    message = message
  )
}

# Internal: construct a per-rule result
cdisc_rule_result <- function(
    rule_id,
    rule_type,
    dataset,
    status,
    sensitivity = "Error",
    description = "",
    message = NULL,
    n_issues = 0L,
    row_findings = list()
) {
  list(
    rule_id = rule_id,
    rule_type = rule_type,
    dataset = dataset,
    status = status,
    sensitivity = sensitivity,
    description = description,
    message = message,
    n_issues = as.integer(n_issues),
    row_findings = row_findings
  )
}

# Internal: construct the aggregated conformance result (S3 class)
cdisc_conformance_result <- function(
    standard,
    version,
    ct_packages,
    rule_results
) {
  structure(
    list(
      standard = standard,
      version = version,
      ct_packages = ct_packages,
      rule_results = rule_results
    ),
    class = "cdisc_conformance_result"
  )
}


#' Did all conformance rules pass?
#'
#' @description
#'
#' Given a `cdisc_conformance_result` object produced by [validate_sdtmig()],
#' determine whether every rule passed without any findings. Returns `TRUE` only
#' when no rule has a `"fail"` status.
#'
#' @param x *A CDISC conformance result*
#'
#'   `obj:<cdisc_conformance_result>` // **required**
#'
#'   A conformance result object returned by [validate_sdtmig()].
#'
#' @return A single logical value.
#'
#' @section Function ID:
#' 13-2
#'
#' @family CDISC
#'
#' @export
cdisc_all_passed <- function(x) {
  !any(
    vapply(
      x$rule_results,
      function(r) identical(r$status, CDISC_STATUS_FAIL),
      logical(1)
    )
  )
}

# Total issue count across all rules
cdisc_n_total_issues <- function(x) {
  sum(vapply(x$rule_results, function(r) r$n_issues, integer(1)))
}

# Table of status counts
cdisc_status_counts <- function(x) {
  statuses <- vapply(x$rule_results, function(r) r$status, character(1))
  table(statuses)
}

# Filter rule results by status
cdisc_rules <- function(x, status = NULL) {
  if (is.null(status)) {
    return(x$rule_results)
  }
  Filter(function(r) r$status == status, x$rule_results)
}

# Flatten all row findings across rule results
cdisc_findings <- function(x) {
  out <- list()
  for (r in x$rule_results) {
    out <- c(out, r$row_findings)
  }
  out
}

# Return a data frame of issues (rules with n_issues > 0)
cdisc_issues <- function(x) {
  rows <- lapply(x$rule_results, function(r) {
    if (r$n_issues > 0L) {
      data.frame(
        dataset = r$dataset,
        rule_id = r$rule_id,
        rule_type = r$rule_type,
        message = r$message %||% r$description,
        n_issues = r$n_issues,
        sensitivity = r$sensitivity,
        status = r$status,
        stringsAsFactors = FALSE
      )
    }
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(
      data.frame(
        dataset = character(0),
        rule_id = character(0),
        rule_type = character(0),
        message = character(0),
        n_issues = integer(0),
        sensitivity = character(0),
        status = character(0),
        stringsAsFactors = FALSE
      )
    )
  }
  do.call(rbind, rows)
}

# Return a data frame of all row-level findings
cdisc_findings_df <- function(x) {
  findings <- cdisc_findings(x)
  if (length(findings) == 0L) {
    return(
      data.frame(
        rule_id = character(0),
        dataset = character(0),
        row = integer(0),
        usubjid = character(0),
        checked_column = character(0),
        checked_value = character(0),
        message = character(0),
        stringsAsFactors = FALSE
      )
    )
  }
  rows <- lapply(findings, function(f) {
    data.frame(
      rule_id = f$rule_id %||% NA_character_,
      dataset = f$dataset %||% NA_character_,
      row = f$row %||% NA_integer_,
      usubjid = f$usubjid %||% NA_character_,
      checked_column = f$checked_column %||% NA_character_,
      checked_value = f$checked_value %||% NA_character_,
      message = f$message %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @export
print.cdisc_conformance_result <- function(x, ...) {

  counts <- cdisc_status_counts(x)
  overall <- if (cdisc_all_passed(x)) "PASS" else "FAIL"
  counts_str <- paste(names(counts), counts, sep = "=", collapse = ", ")

  cat(
    sprintf(
      "<cdisc_conformance_result> %s %s\n",
      toupper(x$standard), x$version
    )
  )
  cat(sprintf("  %d rules (%s)\n", length(x$rule_results), counts_str))
  cat(sprintf("  %d issues -- %s\n", cdisc_n_total_issues(x), overall))

  invisible(x)
}
