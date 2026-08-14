# Did all conformance rules pass?

Given a `cdisc_conformance_result` object produced by
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md),
determine whether every rule passed without any findings. Returns `TRUE`
only when no rule has a `"fail"` status.

## Usage

``` r
cdisc_all_passed(x)
```

## Arguments

- x:

  *A CDISC conformance result*

  `obj:<cdisc_conformance_result>` // **required**

  A conformance result object returned by
  [`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md).

## Value

A single logical value.

## Function ID

13-2

## See also

Other CDISC:
[`cdisc_findings()`](https://rstudio.github.io/pointblank/reference/cdisc_findings.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_rules()`](https://rstudio.github.io/pointblank/reference/cdisc_rules.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md),
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md)
