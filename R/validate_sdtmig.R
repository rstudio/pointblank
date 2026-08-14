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


#' Validate SDTM datasets against the SDTMIG rule catalog
#'
#' @description
#'
#' Evaluate a collection of SDTM domain datasets against the bundled CDISC
#' SDTMIG conformance rule catalog. Rules are loaded from JSON files shipped
#' with **pointblank** and evaluated directly against data frames---no external
#' tools, Docker containers, or API calls are needed at runtime.
#'
#' The engine supports seven rule types: record checks (per-row value
#' validation), dataset metadata checks (column presence, variable order),
#' variable metadata checks, domain presence checks, dataset contents checks,
#' and Define-XML item/codelist checks (stubbed as always-pass until a
#' Define-XML importer is available in R).
#'
#' @param datasets `list` // **required**
#'
#'   A named list of data frames, where each name is a domain name (e.g.,
#'   `"DM"`, `"AE"`, `"LB"`) and each value is the corresponding data frame.
#'   Names are matched case-insensitively.
#'
#' @param version `scalar<character>` // *default:* `"3-4"`
#'
#'   The SDTMIG version string. Use hyphens (e.g., `"3-4"`) or dots (e.g.,
#'   `"3.4"`); dots are converted to hyphens automatically. Currently only
#'   version `"3-4"` is bundled.
#'
#' @param ct_packages `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Character vector of controlled terminology package slugs to load (e.g.,
#'   `"sdtm-ct-2024-09-27"`). When `NULL`, the most recent bundled CT package
#'   is loaded automatically.
#'
#' @param rule_types `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   Optional character vector of rule types to evaluate (e.g.,
#'   `"RECORD_CHECK"`). When `NULL`, all supported rule types are run.
#'
#' @return A `cdisc_conformance_result` object. Use `cdisc_all_passed()`,
#'   `cdisc_status_counts()`, `cdisc_issues()`, `cdisc_findings_df()`, and
#'   `print()` to inspect the results.
#'
#' @section Function ID:
#' 13-1
#'
#' @family CDISC
#'
#' @export
validate_sdtmig <- function(
    datasets,
    version = "3-4",
    ct_packages = NULL,
    rule_types = NULL
) {

  rlang::check_installed(
    "jsonlite",
    "to validate CDISC SDTMIG conformance."
  )

  ver <- gsub("\\.", "-", version)

  engine <- cdisc_engine_new(
    standard = "sdtmig",
    version = ver,
    ct_packages = ct_packages,
    rule_types = rule_types
  )

  cdisc_engine_run(engine, datasets)
}
