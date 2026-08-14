# Get conformance rule results, optionally filtered by status

Returns the list of individual rule results from a conformance result.
Each element is a named list containing `rule_id`, `rule_type`,
`dataset`, `status`, `sensitivity`, `description`, `message`,
`n_issues`, and `row_findings`. Optionally filter to only rules with a
specific status.

## Usage

``` r
cdisc_rules(x, status = NULL)
```

## Arguments

- x:

  *A CDISC conformance result*

  `obj:<cdisc_conformance_result>` // **required**

  A conformance result object returned by
  [`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md).

- status:

  *Filter by status*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A status string to filter by (e.g., `"fail"`, `"pass"`). When `NULL`,
  all rule results are returned.

## Value

A list of rule result objects.

## Function ID

13-5

## See also

Other CDISC:
[`cdisc_all_passed()`](https://rstudio.github.io/pointblank/reference/cdisc_all_passed.md),
[`cdisc_findings()`](https://rstudio.github.io/pointblank/reference/cdisc_findings.md),
[`cdisc_findings_df()`](https://rstudio.github.io/pointblank/reference/cdisc_findings_df.md),
[`cdisc_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_issues.md),
[`cdisc_n_total_issues()`](https://rstudio.github.io/pointblank/reference/cdisc_n_total_issues.md),
[`cdisc_status_counts()`](https://rstudio.github.io/pointblank/reference/cdisc_status_counts.md),
[`validate_sdtmig()`](https://rstudio.github.io/pointblank/reference/validate_sdtmig.md)
