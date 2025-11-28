# A specialized version of `stopifnot()` for **pointblank**: `stop_if_not()`

This variation of [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html)
works well as a standalone replacement for
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) but is also
customized for use in validation checks in R Markdown documents where
**pointblank** is loaded and
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
is invoked. Using `stop_if_not()` in a code chunk where the
`validate = TRUE` option is set will yield the correct reporting of
successes and failures whereas
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) does not.

## Usage

``` r
stop_if_not(...)
```

## Arguments

- ...:

  R expressions that should each evaluate to (a logical vector of all)
  `TRUE`.

## Value

`NULL` if all statements in `...` are `TRUE`.

## Function ID

13-5

## See also

Other Utility and Helper Functions:
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md),
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md),
[`col_schema()`](https://rstudio.github.io/pointblank/reference/col_schema.md),
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md),
[`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md)

## Examples

``` r
# This checks whether the number of
# rows in `small_table` is greater
# than `10`
stop_if_not(nrow(small_table) > 10)
#> NULL

# This will stop for sure: there
# isn't a `time` column in `small_table`
# (but there are the `date_time` and
# `date` columns)
# stop_if_not("time" %in% colnames(small_table))

# You're not bound to using tabular
# data here, any statements that
# evaluate to logical vectors will work
stop_if_not(1 < 20:25 - 18)
#> NULL
```
