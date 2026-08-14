# Get all row-level findings as a list

Flattens and returns all row-level findings across every rule result.
Each finding is a named list containing `rule_id`, `dataset`, `row`
(1-based), `usubjid`, `checked_column`, `checked_value`, `context`, and
`message`.

For a tabular version, use
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md)
instead.

## Usage

``` r
cdisc_findings(x)
```

## Arguments

- x:

  *A CDISC conformance result*

  `obj:<cdisc_conformance_result>` // **required**

  A conformance result object returned by
  [`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md).

## Value

A list of row-level finding objects.

## Function ID

13-6

## See also

Other CDISC:
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_rules()`](https://rstudio.github.io/pointblank/reference/cdisc_rules.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md),
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md)
