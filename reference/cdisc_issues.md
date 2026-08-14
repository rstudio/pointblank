# Get a data frame of conformance issues

Returns a data frame containing one row per failing rule. This is a
rule-level summary (not row-level); use
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md)
for per-row detail. Each row includes the rule ID, affected dataset,
issue count, message, sensitivity, and status.

## Usage

``` r
cdisc_issues(x)
```

## Arguments

- x:

  *A CDISC conformance result*

  `obj:<cdisc_conformance_result>` // **required**

  A conformance result object returned by
  [`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md).

## Value

A data frame with columns `dataset`, `rule_id`, `rule_type`, `message`,
`n_issues`, `sensitivity`, and `status`. Returns an empty data frame
(zero rows) when there are no issues.

## Function ID

13-7

## See also

Other CDISC:
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_findings()`](https://rstudio.github.io/pointblank/reference/cdisc_findings.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_rules()`](https://rstudio.github.io/pointblank/reference/cdisc_rules.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md),
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md)
