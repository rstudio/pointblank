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

# Rule types handled natively
.cdisc_supported_types <- c(
  "RECORD_CHECK",
  "DATASET_METADATA_CHECK",
  "DOMAIN_PRESENCE_CHECK",
  "DATASET_CONTENTS_CHECK",
  "VARIABLE_METADATA_CHECK",
  "DEFINE_ITEM_METADATA_CHECK",
  "DEFINE_CODELIST_CHECK"
)

# SUPP-- and RELREC have non-standard structure; catch-all rules skip them
.cdisc_structural_datasets <- c("RELREC")

# Maximum row-level findings per rule
.cdisc_max_findings <- 100L

# Candidate identifier columns for row findings (priority order)
.cdisc_context_candidates <- c(
  "STUDYID", "DOMAIN", "SUBJID", "VISITNUM", "VISIT", "EPOCH",
  "AESEQ", "CMSEQ", "LBSEQ", "VSSEQ", "EXSEQ", "MHSEQ", "DSSEQ", "EGSEQ",
  "AETERM", "CMTRT", "LBTESTCD", "VSTESTCD", "EGTESTCD"
)

# Internal: create the engine state (a named list, not a class)
cdisc_engine_new <- function(standard, version, ct_packages = NULL,
                             rule_types = NULL) {
  rules <- cdisc_load_rules(standard, version, rule_types = rule_types)
  ct <- if (is.null(ct_packages)) {
    cdisc_ct_load_default()
  } else {
    cdisc_ct_load(ct_packages)
  }
  list(standard = standard, version = version, rules = rules, ct = ct)
}

# Internal: run all rules against the dataset collection
cdisc_engine_run <- function(engine, datasets) {

  # Normalize dataset names to uppercase
  names(datasets) <- toupper(names(datasets))

  results <- lapply(
    engine$rules,
    function(rule) .cdisc_evaluate_rule(rule, engine, datasets)
  )

  cdisc_conformance_result(
    standard = engine$standard,
    version = engine$version,
    ct_packages = engine$ct$packages,
    rule_results = results
  )
}

# ── Rule dispatch ────────────────────────────────────────────────────────────

.cdisc_evaluate_rule <- function(rule, engine, datasets) {

  if (!rule$rule_type %in% .cdisc_supported_types) {
    return(
      cdisc_rule_result(
        rule_id = rule$core_id,
        rule_type = rule$rule_type,
        dataset = "",
        status = cdisc_status_unsupported,
        sensitivity = rule$sensitivity,
        description = rule$description
      )
    )
  }

  # Partially Executable rules require extra inputs
  if (identical(rule$executability, "Partially Executable")) {
    rule_datasets <- as.character(unlist(rule$datasets, use.names = FALSE))
    missing_ds <- character(0)
    for (d in rule_datasets) {
      if (toupper(d) == "DEFINE") {
        # No Define-XML support in R yet
        missing_ds <- c(missing_ds, d)
      } else if (!toupper(d) %in% names(datasets)) {
        missing_ds <- c(missing_ds, d)
      }
    }
    if (length(missing_ds) > 0L) {
      return(
        cdisc_rule_result(
          rule_id = rule$core_id,
          rule_type = rule$rule_type,
          dataset = paste(rule_datasets, collapse = ", "),
          status = cdisc_status_na,
          sensitivity = rule$sensitivity,
          description = rule$description,
          message = paste0(
            "Required input(s) not provided: ",
            paste(missing_ds, collapse = ", ")
          )
        )
      )
    }
  }

  handler <- switch(
    rule$rule_type,
    "RECORD_CHECK"              = .cdisc_record_check,
    "DATASET_CONTENTS_CHECK"    = .cdisc_record_check,
    "DEFINE_CODELIST_CHECK"     = .cdisc_record_check,
    "DATASET_METADATA_CHECK"    = .cdisc_dataset_metadata_check,
    "VARIABLE_METADATA_CHECK"   = .cdisc_dataset_metadata_check,
    "DEFINE_ITEM_METADATA_CHECK" = .cdisc_dataset_metadata_check,
    "DOMAIN_PRESENCE_CHECK"     = .cdisc_domain_presence_check
  )

  tryCatch(
    handler(rule, engine, datasets),
    error = function(e) {
      domain <- if (length(rule$domains) > 0L) {
        as.character(rule$domains[[1]])
      } else {
        ""
      }
      cdisc_rule_result(
        rule_id = rule$core_id,
        rule_type = rule$rule_type,
        dataset = domain,
        status = cdisc_status_error,
        sensitivity = rule$sensitivity,
        description = rule$description,
        message = conditionMessage(e)
      )
    }
  )
}

# ── Rule type handlers ───────────────────────────────────────────────────────

# Per-row check: find rows where the condition tree
# evaluates to TRUE (= violation)
.cdisc_record_check <- function(rule, engine, datasets) {

  rule_domains <- as.character(unlist(rule$domains, use.names = FALSE))

  target_domains <- if (length(rule_domains) > 0L) {
    rule_domains
  } else {
    # Exclude SUPP-- and structural datasets from catch-all iteration
    ds_names <- names(datasets)
    ds_names[!startsWith(ds_names, "SUPP") &
               !ds_names %in% .cdisc_structural_datasets]
  }

  all_findings <- list()
  n_issues <- 0L

  for (domain in target_domains) {

    df <- datasets[[toupper(domain)]]
    if (is.null(df)) next

    df <- cdisc_apply_operations(
      df, rule$operations, engine$ct, datasets, define_meta = NULL
    )

    mask <- tryCatch(
      cdisc_evaluate_conditions(df, rule$conditions),
      cdisc_evaluation_error = function(e) NULL
    )
    if (is.null(mask)) next

    # which() returns 1-based indices of TRUE values, excluding NA
    failing_rows <- which(mask)
    n_issues <- n_issues + length(failing_rows)

    n_to_collect <- min(length(failing_rows), .cdisc_max_findings)
    for (i in seq_len(n_to_collect)) {
      row_idx <- failing_rows[i]
      finding <- .cdisc_build_row_finding(
        df = df,
        row_idx = row_idx,
        domain = domain,
        operations = rule$operations,
        conditions = rule$conditions,
        rule_id = rule$core_id,
        message = cdisc_rule_message(rule)
      )
      all_findings <- c(all_findings, list(finding))
    }
  }

  domain_label <- paste(target_domains, collapse = ", ")

  cdisc_rule_result(
    rule_id = rule$core_id,
    rule_type = rule$rule_type,
    dataset = domain_label,
    status = if (n_issues > 0L) cdisc_status_fail else cdisc_status_pass,
    sensitivity = rule$sensitivity,
    description = rule$description,
    message = if (n_issues > 0L) cdisc_rule_message(rule) else NULL,
    n_issues = n_issues,
    row_findings = all_findings
  )
}

# Dataset-level metadata check (column presence, sort keys, variable order)
.cdisc_dataset_metadata_check <- function(rule, engine, datasets) {

  rule_domains <- as.character(unlist(rule$domains, use.names = FALSE))

  target_domains <- if (length(rule_domains) > 0L) {
    rule_domains
  } else {
    ds_names <- names(datasets)
    ds_names[!startsWith(ds_names, "SUPP") &
               !ds_names %in% .cdisc_structural_datasets]
  }

  n_issues <- 0L
  first_failing_domain <- ""

  for (domain in target_domains) {

    df <- datasets[[toupper(domain)]]
    if (is.null(df)) next

    df <- cdisc_apply_operations(
      df, rule$operations, engine$ct, datasets, define_meta = NULL
    )

    mask <- tryCatch(
      cdisc_evaluate_conditions(df, rule$conditions),
      cdisc_evaluation_error = function(e) NULL
    )
    if (is.null(mask)) next

    if (any(mask, na.rm = TRUE)) {
      n_issues <- n_issues + 1L
      if (!nzchar(first_failing_domain)) {
        first_failing_domain <- domain
      }
    }
  }

  dataset_label <- if (nzchar(first_failing_domain)) {
    first_failing_domain
  } else if (length(target_domains) > 0L) {
    target_domains[1]
  } else {
    ""
  }

  cdisc_rule_result(
    rule_id = rule$core_id,
    rule_type = rule$rule_type,
    dataset = dataset_label,
    status = if (n_issues > 0L) cdisc_status_fail else cdisc_status_pass,
    sensitivity = rule$sensitivity,
    description = rule$description,
    message = if (n_issues > 0L) cdisc_rule_message(rule) else NULL,
    n_issues = n_issues
  )
}

# Check that required domains are present / prohibited domains are absent
.cdisc_domain_presence_check <- function(rule, engine, datasets) {

  params <- rule$actions$params %||% list()
  required_domains <- as.character(
    unlist(params$required_domains %||% list(), use.names = FALSE)
  )
  prohibited_domains <- as.character(
    unlist(params$prohibited_domains %||% list(), use.names = FALSE)
  )
  present_domains <- names(datasets)

  missing <- required_domains[!toupper(required_domains) %in% present_domains]
  found_prohibited <- prohibited_domains[
    toupper(prohibited_domains) %in% present_domains
  ]

  issues <- c(missing, found_prohibited)
  n_issues <- length(issues)

  message <- if (length(missing) > 0L) {
    paste0("Required domain(s) missing: ", paste(missing, collapse = ", "))
  } else if (length(found_prohibited) > 0L) {
    paste0(
      "Prohibited domain(s) present: ",
      paste(found_prohibited, collapse = ", ")
    )
  } else {
    NULL
  }

  domain_label <- paste(
    c(required_domains, prohibited_domains),
    collapse = ", "
  )

  cdisc_rule_result(
    rule_id = rule$core_id,
    rule_type = rule$rule_type,
    dataset = domain_label,
    status = if (n_issues > 0L) cdisc_status_fail else cdisc_status_pass,
    sensitivity = rule$sensitivity,
    description = rule$description,
    message = message,
    n_issues = n_issues
  )
}

# ── Row finding construction ─────────────────────────────────────────────────

# Recursively collect all column names referenced in a conditions tree
.cdisc_condition_columns <- function(conditions) {
  cols <- character(0)
  for (key in c("all", "any")) {
    subs <- conditions[[key]]
    if (!is.null(subs)) {
      for (sub in subs) {
        cols <- c(cols, .cdisc_condition_columns(sub))
      }
    }
  }
  name <- conditions$name
  if (!is.null(name)) {
    cols <- c(cols, name)
  }
  cols
}

# Build a row finding with smart column selection (1-based row index)
.cdisc_build_row_finding <- function(df, row_idx, domain, operations,
                                     conditions, rule_id, message) {

  col_names <- names(df)

  # USUBJID — the most important identifier
  usubjid <- if ("USUBJID" %in% col_names) {
    val <- df[["USUBJID"]][row_idx]
    if (!is.na(val)) as.character(val) else NULL
  } else {
    NULL
  }

  # Primary checked column: first operation that names a column present in df
  checked_col <- NULL
  for (op in operations) {
    op_col <- op$params$column
    if (!is.null(op_col) && op_col %in% col_names) {
      checked_col <- op_col
      break
    }
  }
  if (is.null(checked_col)) {
    cond_cols <- .cdisc_condition_columns(conditions)
    for (cc in cond_cols) {
      if (cc %in% col_names) {
        checked_col <- cc
        break
      }
    }
  }

  checked_val <- if (!is.null(checked_col)) {
    val <- df[[checked_col]][row_idx]
    if (!is.na(val)) as.character(val) else ""
  } else {
    NULL
  }

  # Context: small set of identifying columns
  context <- list()
  for (cand in .cdisc_context_candidates) {
    if (cand %in% col_names && !identical(cand, "USUBJID") &&
        !identical(cand, checked_col)) {
      val <- df[[cand]][row_idx]
      if (!is.na(val)) {
        s <- as.character(val)
        if (nzchar(s) && s != "NA") {
          context[[cand]] <- s
        }
      }
    }
  }

  cdisc_row_finding(
    rule_id = rule_id,
    dataset = domain,
    row = row_idx,
    usubjid = usubjid,
    checked_column = checked_col,
    checked_value = checked_val,
    context = context,
    message = message
  )
}
