# Get a table of conformance status counts

Returns a frequency table of rule statuses from a conformance result.
The possible statuses are `"pass"`, `"fail"`, `"error"`,
`"not_applicable"`, and `"not_supported"`.

## Usage

``` r
cdisc_status_counts(x)
```

## Arguments

- x:

  *A CDISC conformance result*

  `obj:<cdisc_conformance_result>` // **required**

  A conformance result object returned by
  [`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md).

## Value

A named integer vector (a `table` object) with counts per status.

## Function ID

13-4

## See also

Other CDISC:
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_findings()`](https://rstudio.github.io/pointblank/reference/cdisc_findings.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_rules()`](https://rstudio.github.io/pointblank/reference/cdisc_rules.md),
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md)
