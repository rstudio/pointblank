# Validate SDTM datasets against the SDTMIG rule catalog

Evaluate a collection of SDTM domain datasets against the bundled CDISC
SDTMIG conformance rule catalog. Rules are loaded from JSON files
shipped with **pointblank** and evaluated directly against data
frames—no external tools, Docker containers, or API calls are needed at
runtime.

The engine supports seven rule types: record checks (per-row value
validation), dataset metadata checks (column presence, variable order),
variable metadata checks, domain presence checks, dataset contents
checks, and Define-XML item/codelist checks (stubbed as always-pass
until a Define-XML importer is available in R).

## Usage

``` r
validate_sdtmig(
  datasets,
  version = "3-4",
  ct_packages = NULL,
  rule_types = NULL
)
```

## Arguments

- datasets:

  `list` // **required**

  A named list of data frames, where each name is a domain name (e.g.,
  `"DM"`, `"AE"`, `"LB"`) and each value is the corresponding data
  frame. Names are matched case-insensitively.

- version:

  `scalar<character>` // *default:* `"3-4"`

  The SDTMIG version string. Use hyphens (e.g., `"3-4"`) or dots (e.g.,
  `"3.4"`); dots are converted to hyphens automatically. Currently only
  version `"3-4"` is bundled.

- ct_packages:

  `vector<character>` // *default:* `NULL` (`optional`)

  Character vector of controlled terminology package slugs to load
  (e.g., `"sdtm-ct-2024-09-27"`). When `NULL`, the most recent bundled
  CT package is loaded automatically.

- rule_types:

  `vector<character>` // *default:* `NULL` (`optional`)

  Optional character vector of rule types to evaluate (e.g.,
  `"RECORD_CHECK"`). When `NULL`, all supported rule types are run.

## Value

A `cdisc_conformance_result` object. Use
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
and [`print()`](https://rdrr.io/r/base/print.html) to inspect the
results.

## Function ID

13-1

## See also

Other CDISC:
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_findings()`](https://rstudio.github.io/pointblank/reference/cdisc_findings.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_rules()`](https://rstudio.github.io/pointblank/reference/cdisc_rules.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md)
