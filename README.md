
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pointblank <a href='http://rich-iannone.github.io/pointblank/'><img src="man/figures/logo.svg" align="right" height="250px" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/pointblank)](https://cran.r-project.org/package=pointblank)
[![R build
status](https://github.com/rich-iannone/pointblank/workflows/R-CMD-check/badge.svg)](https://github.com/rich-iannone/pointblank/actions)
[![Coverage
status](https://codecov.io/gh/rich-iannone/pointblank/branch/master/graph/badge.svg)](https://codecov.io/gh/rich-iannone/pointblank?branch=master)

With the **pointblank** package it’s really easy to validate your data
with workflows attuned to your data quality needs. The **pointblank**
philosophy: a set of validation step functions should work seamlessly
with data in local data tables and with data in databases.

The two dominant workflows that **pointblank** enables are *data quality
reporting* and *pipeline-based data validations*. Both workflows make
use of a large collection of simple validation step functions (e.g., are
values in a specific column greater than those in another column or some
fixed value?), and, both allow for stepwise, temporary
mutation/alteration of the input table to enable much more sophisticated
validation checks.

<hr>

<img src="man/figures/data_quality_reporting_workflow.png">

The first workflow, *data quality reporting* allows for the easy
creation of a data quality analysis report. This is most useful in a
non-interactive mode where data quality for database tables and on-disk
data files must be periodically checked. The reporting component
(through a **pointblank** agent) allows for the collection of detailed
validation measures for each validation step, the optional extraction of
data rows that failed validation (with options on limits), and custom
functions that are invoked by exceeding set threshold failure rates.
Want to email the report regularly (or, only if certain conditions are
met)? Yep, you can do all that.

<hr>

<img src="man/figures/pipeline_based_data_validations.png">

The second workflow, *pipeline-based data validations* gives us a
different validation scheme that is valuable for data validation checks
during an ETL process. With **pointblank**’s validation step functions,
we can directly operate on data and trigger warnings, raise errors, or
write out logs when exceeding specified failure thresholds. It’s a cinch
to perform checks on import of the data and at key points during the
transformation process, perhaps stopping everything if things are
exceptionally bad with regard to data quality.

<hr>

The **pointblank** package is designed to be both straightforward yet
powerful. And fast\! All validation checks on remote tables are done
entirely in-database so we can add dozens or hundreds of validation
steps without any long waits for reporting. Here is a brief example of
how to use **pointblank** to validate a local table with an agent.

``` r
# Generate a simple `action_levels` object to
# set the `warn` state if a validation step
# has a single 'fail' unit
al <- action_levels(warn_at = 1)

# Create a pointblank `agent` object, with the
# tibble as the target table. Use two validation
# step functions, then, `interrogate()`. The
# agent now has some useful intel.
agent <- 
  dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  create_agent(name = "simple_tibble", actions = al) %>%
  col_vals_between(vars(a), 1, 9, na_pass = TRUE) %>%
  col_vals_lt(vars(c), 12, preconditions = ~ . %>% dplyr::mutate(c = a + b)) %>%
  interrogate()
```

Because an *agent* was used, we can get a report from it.

``` r
agent
```

<img src="man/figures/agent_report.png">

Next up is an example that follows the second, *agent*-less workflow
(where validation step functions operate directly on data). We use the
same two validation step functions as before but, this time, use them
directly on the data\! In this workflow, an error occur will occur if
there is a single ‘fail’ unit:

``` r
dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  col_vals_between(vars(a), 1, 9, na_pass = TRUE) %>%
  col_vals_lt(vars(c), 12, preconditions = ~ . %>% dplyr::mutate(c = a + b))
```

    Error: The validation (`col_vals_lt()`) meets or exceeds the stop threshold
     * VIOLATION: Expect that values in `c` (computed column) should be < `12`. Precondition applied: `. %>% dplyr::mutate(c = a + b)`.

We can downgrade this to a warning with the `warn_on_fail()` helper
function (assigning to `actions`). In this way, the data will be
returned, but warnings will appear.

``` r
# This `warn_on_fail()` is a nice wrapper for
# `action_levels`; it works great in this data
# checking workflow!
al <- warn_on_fail()

dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  col_vals_between(vars(a), 1, 9, na_pass = TRUE, actions = al) %>%
  col_vals_lt(vars(c), 12, preconditions = ~ . %>% dplyr::mutate(c = a + b), actions = al)
```

    The validation (`col_vals_lt()`) meets or exceeds the warn threshold
     * VIOLATION: Expect that values in `c` (computed column) should be < `12`. Precondition applied: `. %>% dplyr::mutate(c = a + b)`.

    #> # A tibble: 6 x 2
    #>       a     b
    #>   <dbl> <dbl>
    #> 1     5     6
    #> 2     7     1
    #> 3     6     0
    #> 4     5     6
    #> 5    NA     0
    #> 6     7     7

Should you need more fine-grained thresholds and resultant actions, the
`action_levels()` function can be used to specify multiple failure
thresholds and side effects for each failure state. However, the
`warn_on_fail()` and `stop_on_fail()` (applied by default, with `stop_at
= 1`) helpers should in most cases suffice for this workflow.

<hr>

While data validation is important one has to be familiar with the data
first. To that end, the `scan_data()` function is provided in
**pointblank** for generating a comprehensive summary of a tabular
dataset. The report content is customizable and can be produced in five
different languages: *English*, *French*, *German*, *Italian*, and
*Spanish* (validation reports can also be produced in either of these
languages). Here are several published examples of a **Table Scan** for
each of these languages (using the `dplyr::storms` dataset):

  - [Table Scan in
    English](https://rpubs.com/rich_i/pointblank_storms_english)
  - [Table Scan in
    French](https://rpubs.com/rich_i/pointblank_storms_french)
  - [Table Scan in
    German](https://rpubs.com/rich_i/pointblank_storms_german)
  - [Table Scan in
    Italian](https://rpubs.com/rich_i/pointblank_storms_italian)
  - [Table Scan in
    Spanish](https://rpubs.com/rich_i/pointblank_storms_spanish)

<hr>

There are many functions available in **pointblank** for making
comprehensive table validations. Each validation step function is paired
with an expectation function (of the form `expect_*()`). These serve as
table assertions and are equivalent in usage and behavior to
**testthat** tests.

<p align="center">

<img src="man/figures/pointblank_functions.svg" width="80%">

</p>

Want to try this out? The **pointblank** package is available on
**CRAN**:

``` r
install.packages("pointblank")
```

You can also install the development version of **pointblank** from
**GitHub**:

``` r
devtools::install_github("rich-iannone/pointblank")
```

If you encounter a bug, have usage questions, or want to share ideas to
make this package better, feel free to file an
[issue](https://github.com/rich-iannone/pointblank/issues).

## Code of Conduct

Please note that the pointblank project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/).<br>By
participating in this project you agree to abide by its terms.

## License

MIT © Richard Iannone
