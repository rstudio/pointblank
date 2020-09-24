<div align="center">

<a href='http://rich-iannone.github.io/pointblank/'><img src="man/figures/logo.svg" height="350px"/></a>

<br />

<a href="https://cran.r-project.org/package=pointblank"><img src="https://www.r-pkg.org/badges/version/pointblank" alt="CRAN status" /></a>
<a href="https://CRAN.R-project.org/package=pointblank"><img src="https://cranlogs.r-pkg.org/badges/pointblank" alt="downloads"></a>

<a href="https://github.com/rich-iannone/pointblank/actions"><img src="https://github.com/rich-iannone/pointblank/workflows/R-CMD-check/badge.svg" alt="R build status" /></a>
<a href="https://codecov.io/gh/rich-iannone/pointblank?branch=master"><img src="https://codecov.io/gh/rich-iannone/pointblank/branch/master/graph/badge.svg" alt="Coverage status" /></a>

<a href="https://www.tidyverse.org/lifecycle/#maturing"><img src="https://img.shields.io/badge/lifecycle-maturing-blue.svg" alt="Lifecycle: maturing" /></a>


<br />
</div>

With the **pointblank** package it’s really easy to methodically validate your
data whether in the form of data frames or as database tables. On top of the
validation toolset, the package gives you the means to define and keep current
with the information that *defines* your tables.

For table *validation*, the *agent* object works with a large collection of
simple (yet powerful!) validation functions. We can enable much more
sophisticated validation checks by using custom expressions and also through
stepwise mutation of the target table (through something we call
`preconditions`).

Sometimes we want to maintain table *information* and update it when the table
goes through changes. For that, we can use an *informant* object + associated
functions to help define the metadata entries and present it in a way that suits
you.

<hr>

<img src="man/figures/data_quality_reporting_workflow.png">

##### TABLE VALIDATIONS WITH AN AGENT AND ITS DETAILED REPORTING

Data validation can be carried out in *data quality reporting* workflow, 
ultimately resulting in the production of of a data quality analysis report.
This is most useful in a non-interactive mode where data quality for database
tables and on-disk data files must be periodically checked. The **pointblank**
*agent* is given a collection of validation functions to define validation
steps. We can get extracts of data rows that failed validation, set up custom
functions that are invoked by exceeding set threshold failure rates, etc. Want
to email the report regularly (or, only if certain conditions are met)? Yep,
you can do all that.

Here is an example of how to use **pointblank** to validate a local table
with an agent.

``` r
# Generate a simple `action_levels` object to
# set the `warn` state if a validation step
# has a single 'fail' test unit
al <- action_levels(warn_at = 1)

# Create a pointblank `agent` object, with the
# tibble as the target table. Use three validation
# functions, then, `interrogate()`. The agent will
# then have some useful intel.
agent <- 
  dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  create_agent(
    label = "A very *simple* example.",
    actions = al
  ) %>%
  col_vals_between(
    vars(a), 1, 9,
    na_pass = TRUE
  ) %>%
  col_vals_lt(
    vars(c), 12,
    preconditions = ~ . %>% dplyr::mutate(c = a + b)
  ) %>%
  col_is_numeric(vars(a, b)) %>%
  interrogate()
```

The **pointblank** package is designed to be both straightforward yet
powerful. And fast\! Local data frames don’t take very long to validate
extensively and all validation checks on remote tables are done entirely
in-database. So we can add dozens or even hundreds of validation steps
without any long waits for reporting.

Should you want to perform validation checks on database or *Spark*
tables, provide a `tbl_dbi` or `tbl_spark` object to `create_agent()`.
The **pointblank** package currently supports *PostgreSQL*. *MySQL*,
*MariaDB*, *DuckDB*, *SQLite*, and *Spark DataFrames* (through the
**sparklyr** package). The `db_tbl()` function is provided by
**pointblank** to make accessing a DB table insanely easy.

The reporting’s pretty sweet. We can get a **gt**-based report by
printing an *agent*.

<img src="man/figures/agent_report.png">

<hr>

<img src="man/figures/pipeline_based_data_validations.png">

##### VALIDATIONS DIRECTLY ON DATA

We can perform pipeline-based data validations using the same collection of
validation functions. This is useful for an ETL process where we
want to directly operate on data and trigger warnings, raise errors, or
write out logs when exceeding specified failure thresholds. It’s a cinch
to perform checks on import of the data and at key points during the
transformation process, perhaps stopping everything if things are
exceptionally bad with regard to data quality.

The following example uses the same three validation functions as before but,
this time, we use them directly on the data\! In this workflow, by default, an
error will occur if there is a single ‘fail’ test unit in any validation step:

``` r
dplyr::tibble(
    a = c(5, 7, 6, 5, NA, 7),
    b = c(6, 1, 0, 6,  0, 7)
  ) %>%
  col_vals_between(
    vars(a), 1, 9,
    na_pass = TRUE
  ) %>%
  col_vals_lt(
    vars(c), 12,
    preconditions = ~ . %>% dplyr::mutate(c = a + b)
  ) %>%
  col_is_numeric(vars(a, b))
```

    Error: Exceedance of failed test units where values in `c` should have been < `12`.
    The `col_vals_lt()` validation failed beyond the absolute threshold level (1).
    * failure level (2) >= failure threshold (1) 

We can downgrade this error to a warning with the `warn_on_fail()` helper
function (assigning it to `actions`). In this way, the data will be
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
  col_vals_between(
    vars(a), 1, 9,
    na_pass = TRUE,
    actions = warn_on_fail()
  ) %>%
  col_vals_lt(
    vars(c), 12,
    preconditions = ~ . %>% dplyr::mutate(c = a + b),
    actions = warn_on_fail()
  ) %>%
  col_is_numeric(vars(a, b))
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
thresholds and side effects for each failure state. However, with
`warn_on_fail()` and `stop_on_fail()` (applied by default, with
`stop_at = 1`), you should have good enough options for this validation
workflow.

<hr>

##### VALIDATIONS IN R MARKDOWN DOCUMENTS

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

<img src="man/figures/pointblank_rmarkdown.png" width="100%" style="border:2px solid #021a40;">

</p>

The above R Markdown document is available as a template in the RStudio
IDE (it’s called `Pointblank Validation`). Try it out\!

<hr>

##### TABLE SCANS

While data validation is important, one has to be familiar with the data
first. To that end, the `scan_data()` function is provided in
**pointblank** for generating a comprehensive summary of a tabular
dataset. The report content is customizable, can be used inside an R
Markdown document, and (as with the validation report) it can be
produced in eight different languages: *English*, *French*, *German*,
*Italian*, *Spanish*, *Portuguese*, *Chinese* (China mainland), and
*Russian*. Here are several published examples of a **Table Scan** for
each of these languages using the `dplyr::storms` dataset. Clicking any
of these will take you to a highly interactive **RPubs** document.

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
[![Table Scan in
Portuguese](https://img.shields.io/static/v1?label=Table%20Scan&message=Portuguese&color=blue)](https://rpubs.com/rich_i/pointblank_storms_portuguese)   
[![Table Scan in
Chinese](https://img.shields.io/static/v1?label=Table%20Scan&message=Chinese&color=blue)](https://rpubs.com/rich_i/pointblank_storms_chinese)   
[![Table Scan in
Russian](https://img.shields.io/static/v1?label=Table%20Scan&message=Russian&color=blue)](https://rpubs.com/rich_i/pointblank_storms_russian)   

Database tables can be used with `scan_data()` as well. Here are two
examples using the `full_region` table of the **Rfam** database (hosted
publicly at “mysql-rfam-public.ebi.ac.uk”) and the `assembly` table of
the **Ensembl** database (hosted publicly at “ensembldb.ensembl.org”).

[![Rfam:
full\_region](https://img.shields.io/static/v1?label=Table%20Scan&message=Rfam:%20full_region&color=green)](https://rpubs.com/rich_i/rfam_full_region)   
[![Ensembl:
assembly](https://img.shields.io/static/v1?label=Table%20Scan&message=Ensembl:%20assembly&color=green)](https://rpubs.com/rich_i/ensembl_assembly)

<hr>

##### OVERVIEW OF PACKAGE FUNCTIONS

There are many functions available in **pointblank** for making
comprehensive table validations. Each validation function is associated
with an expectation function (of the form `expect_*()`). They are
equivalent in usage and behavior to **testthat** tests with the big
distinction that they check aspects of data tables (and not the results
of function calls). Furthermore, each validation function has an
associated test function (of the form `test_*()`) which always returns a
logical value (`TRUE` or `FALSE`).

<p align="center">

<img src="man/figures/pointblank_functions.svg" width="100%">

</p>

<hr>

##### INSTALLATION

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

##### How **pointblank** Fits in with Other Packages that Validate Tabular Data

The **pointblank** package isn’t the only one of its kind available for
**R**. The reason for introducing yet another has to do with
**pointblank**’s goals:

  - ability to work with local tables, database tables, and Spark
    DataFrames (via **sparklyr**) with minimal changes in the API
  - great flexibility in data validation workflows, allowing for: (1)
    report-based validations, (2) inline validations, (3) validation of
    data tables in unit tests (with the set of `expect_*()` functions),
    and (4) validation of data tables to support conditional expressions
    (with the set of `test_*()` functions)
  - enabling a workflow for collecting and reporting on useful
    information about your data tables
  - extra tools for understanding new datasets (`scan_data()`) and
    validating data in specialized R Markdown code chunks
    (`validate_rmd()`)
  - reporting outputs translated to multiple spoken languages
  - developing an API that closely follows tidyverse conventions by
    adhering to the [tidyverse style guide](https://style.tidyverse.org)
  - lots of attention on making the package documentation and examples
    the best they can be

While **pointblank** is trying to do something different, it may not
suit your specific needs. Here is a listing of some other validation
**R** packages:

**assertr** ([GITHUB](https://github.com/ropensci/assertr), [WEBSITE](https://docs.ropensci.org/assertr))

**validate** ([GITHUB](https://github.com/data-cleaning/validate))

**dataMaid** ([GITHUB](https://github.com/ekstroem/dataMaid))

<hr>

##### Code of Conduct

Please note that the pointblank project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/).<br>By
participating in this project you agree to abide by its terms.

##### License

MIT © Richard Iannone
