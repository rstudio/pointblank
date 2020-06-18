
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
philosophy: a set of validation functions should work seamlessly with
data in local data tables and with data in databases.

The two dominant workflows that **pointblank** enables are *data quality
reporting* and *pipeline-based data validations*. Both workflows make
use of a large collection of simple validation functions (e.g., are
values in a specific column greater than those in another column or some
fixed value?), and, both allow for stepwise, temporary
mutation/alteration of the input table (through `preconditions`) to
enable much more sophisticated validation checks.

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
during an ETL process. With **pointblank**’s validation functions, we
can directly operate on data and trigger warnings, raise errors, or
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

Because an *agent* was used, we can get a **gt**-based report by
printing it.

<img src="man/figures/agent_report.png">

<hr>

Next up is an example that follows the second, *agent*-less workflow
(where validation functions operate directly on data). We use the same
two validation functions as before but, this time, use them directly on
the data\! In this workflow, by default, an error will occur if there is
a single ‘fail’ unit in any validation step:

``` r
dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  col_vals_between(vars(a), 1, 9, na_pass = TRUE) %>%
  col_vals_lt(vars(c), 12, preconditions = ~ . %>% dplyr::mutate(c = a + b))
```

    Error: Exceedance of failed test units where values in `c` should have been < `12`.
    The `col_vals_lt()` validation failed beyond the absolute threshold level (1).
    * failure level (2) >= failure threshold (1) 

We can downgrade this to a warning with the `warn_on_fail()` helper
function (assigning to `actions`). In this way, the data will be
returned, but warnings will appear.

``` r
# This `warn_on_fail()` function is a nice
# shortcut for `action_levels(warn_at = 1)`;
# it works great in this data checking workflow
# (and the threshold can still be adjusted)
al <- warn_on_fail()

dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  col_vals_between(vars(a), 1, 9, na_pass = TRUE, actions = al) %>%
  col_vals_lt(vars(c), 12, preconditions = ~ . %>% dplyr::mutate(c = a + b), actions = al)
```

    #> # A tibble: 6 x 2
    #>       a     b
    #>   <dbl> <dbl>
    #> 1     5     6
    #> 2     7     1
    #> 3     6     0
    #> 4     5     6
    #> 5    NA     0
    #> 6     7     7

    Warning message:
    Exceedance of failed test units where values in `c` should have been < `12`.
    The `col_vals_lt()` validation failed beyond the absolute threshold level (1).
    * failure level (2) >= failure threshold (1) 

Should you need more fine-grained thresholds and resultant actions, the
`action_levels()` function can be used to specify multiple failure
thresholds and side effects for each failure state. However, the
`warn_on_fail()` and `stop_on_fail()` (applied by default, with `stop_at
= 1`) helpers should in most cases suffice for this workflow.

<hr>

Using **pointblank** in an R Markdown workflow is enabled by default
once the **pointblank** library is loaded. The framework allows for
validation testing within specialized validation code chunks where the
`validate = TRUE` option is set. Using **pointblank** validation
functions on data in these marked code chunks will flag overall failure
if the stop threshold is exceeded anywhere. All errors are reported in
the validation code chunk after rendering the document to HTML, where
green or red status buttons indicate whether all validations succeeded
or failures occurred. Click them to reveal the otherwise hidden
validation statements and any associated error messages.

<p align="center">

<img src="man/figures/pointblank_rmarkdown.png" width="80%" style="border:2px solid #021a40;">

</p>

The above R Markdown document is available as a template in the RStudio
IDE (it’s called `Pointblank Validation`). Try it out\!

<hr>

While data validation is important, one has to be familiar with the data
first. To that end, the `scan_data()` function is provided in
**pointblank** for generating a comprehensive summary of a tabular
dataset. The report content is customizable, can be used inside an R
Markdown document, and (as with the validation report) it can be
produced in five different languages: *English*, *French*, *German*,
*Italian*, and *Spanish*. Here are several published examples of a
**Table Scan** for each of these languages using the `dplyr::storms`
dataset. Clicking any of these will take you to a highly interactive
**RPubs** document.

[![Table Scan in
English](https://img.shields.io/static/v1?label=Table%20Scan&message=English&color=blue)](https://rpubs.com/rich_i/pointblank_storms_english)   
[![Table Scan in
French](https://img.shields.io/static/v1?label=Table%20Scan&message=French&color=blue)](https://rpubs.com/rich_i/pointblank_storms_french)   
[![Table Scan in
German](https://img.shields.io/static/v1?label=Table%20Scan&message=German&color=blue)](https://rpubs.com/rich_i/pointblank_storms_german)   
[![Table Scan in
Italian](https://img.shields.io/static/v1?label=Table%20Scan&message=Italian&color=blue)](https://rpubs.com/rich_i/pointblank_storms_italian)   
[![Table Scan in
Spanish](https://img.shields.io/static/v1?label=Table%20Scan&message=Spanish&color=blue)](https://rpubs.com/rich_i/pointblank_storms_spanish)

<hr>

There are many functions available in **pointblank** for making
comprehensive table validations. Each validation function is associated
with an expectation function (of the form `expect_*()`). They are
equivalent in usage and behavior to **testthat** tests with the big
distinction that they check aspects of data tables (and not the results
of function calls). Furthermore, each validation function has an
associated test function (of the form `test_*()`) which always returns a
logical value (`TRUE` or `FALSE`).

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

<hr>

#### How **pointblank** Fits in with Other Packages that Validate Tabular Data

The **pointblank** package isn’t the only one of its kind available for
**R**. The reason for introducing yet another has to do with
**pointblank**’s goals:

  - ability to work with local tables and database tables with minimal
    changes in the API
  - great flexibility in workflows, allowing for: (1) report-based
    validations, (2) inline validations, (3) validation of data tables
    in unit tests (with the set of `expect_*()` functions), and (4)
    validation of data tables to support conditional expressions (with
    the set of `test_*()` functions)
  - extra tools for understanding new datasets (`scan_data()`) and
    validating data in specialized R Markdown code chunks
    (`validate_rmd()`)
  - reporting outputs translated to multiple spoken languages
  - developing an API that closely follows tidyverse conventions by
    adhering to the [tidyverse style guide](https://style.tidyverse.org)
  - lots of attention on making the package documentation and examples
    the best they can be

While **pointblank** is trying to do something different with its own
interface, it may not suit your specific needs. Here is a listing of
some of the other validation packages available for **R**, with links to
their respective project pages:

**assertr** ([GITHUB](https://github.com/ropensci/assertr), [WEBSITE](https://docs.ropensci.org/assertr))

**validate** ([GITHUB](https://github.com/data-cleaning/validate))

**dataMaid** ([GITHUB](https://github.com/ekstroem/dataMaid))

<hr>

##### Code of Conduct

Please note that the pointblank project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/).<br>By
participating in this project you agree to abide by its terms.

#### License

MIT © Richard Iannone
