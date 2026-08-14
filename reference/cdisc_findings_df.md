# Get a data frame of row-level conformance findings

Returns a tidy data frame with one row per row-level finding from the
conformance result. Each row identifies the rule that fired, the domain
and row where the violation occurred, the subject (`USUBJID`), and the
specific column and value that triggered the finding.

For a rule-level summary instead, use
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md).

## Usage

``` r
cdisc_findings_df(x)
```

## Arguments

- x:

  *A CDISC conformance result*

  `obj:<cdisc_conformance_result>` // **required**

  A conformance result object returned by
  [`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md).

## Value

A data frame with columns `rule_id`, `dataset`, `row` (1-based index),
`usubjid`, `checked_column`, `checked_value`, and `message`. Returns an
empty data frame (zero rows) when there are no findings.

## Function ID

13-8

## See also

Other CDISC:
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_findings()`](https://rstudio.github.io/pointblank/reference/cdisc_findings.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_rules()`](https://rstudio.github.io/pointblank/reference/cdisc_rules.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md),
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md)
