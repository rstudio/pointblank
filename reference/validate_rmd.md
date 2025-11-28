# Perform **pointblank** validation testing within R Markdown documents

The `validate_rmd()` function sets up a framework for validation testing
within specialized validation code chunks inside an R Markdown document.
To enable this functionality, `validate_rmd()` should be called early
within an R Markdown document code chunk (preferably in the `setup`
chunk) to signal that validation should occur within specific code
chunks. The validation code chunks require the `validate = TRUE` option
to be set. Using **pointblank** validation functions on data in these
marked code chunks will flag overall failure if the stop threshold is
exceeded anywhere. All errors are reported in the validation code chunk
after rendering the document to HTML, where a centered status button
either indicates success or the number of overall failures. Clicking the
button reveals the otherwise hidden validation statements and their
error messages (if any).

## Usage

``` r
validate_rmd(summary = TRUE, log_to_file = NULL)
```

## Arguments

- summary:

  *Include a validation summary*

  `scalar<logical>` // *default:* `TRUE`

  If `TRUE` then there will be a leading summary of all validations in
  the rendered R Markdown document. With `FALSE`, this element is not
  shown.

- log_to_file:

  *Log validation results to a file*

  `scalar<logical|character>` // *default:* `NULL` (`optional`)

  An option to log errors to a text file. By default, no logging is done
  but `TRUE` will write log entries to `"validation_errors.log"` in the
  working directory. To both enable logging and to specify a file name,
  include a path to a log file of the desired name.

## Function ID

1-4

## See also

Other Planning and Prep:
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
[`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
