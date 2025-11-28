# Introduction to the Data Tests for Conditionals Workflow (VALID-IV)

![](images/VALID-IV.svg)

The *VALID-IV: Data Tests for Conditionals* workflow is probably not
much of a workflow really. But maybe you consider programming and
control flow a sort of workflow. If that’s the case and you are
programming with data, the functions of this workflow might be useful
for that. A set of `test_*()` functions, with the same arguments as the
corresponding `expect_*()` functions of the
[**VALID-III**](https://rstudio.github.io/pointblank/articles/VALID-III.md)
workflow, are used with data tables and the result of each call returns
a single logical value (`TRUE` or `FALSE`). Here’s the complete list of
functions with a phrase for what each function tests:

- [`test_col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md):
  Test whether column data are less than a specified value.
- [`test_col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md):
  Test whether column data are less than or equal to a specified value.
- [`test_col_vals_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_equal.md):
  Test whether column data are equal to a specified value.
- [`test_col_vals_not_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md):
  Test whether column data are not equal to a specified value.
- [`test_col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md):
  Test whether column data are greater than or equal to a specified
  value.
- [`test_col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md):
  Test whether column data are greater than a specified value.
- [`test_col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md):
  Test whether column data are between two specified values.
- [`test_col_vals_not_between()`](https://rstudio.github.io/pointblank/reference/col_vals_not_between.md):
  Test whether column data are not between two specified values.
- [`test_col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md):
  Test whether column data are part of a specified set of values.
- [`test_col_vals_not_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md):
  Test whether column data are not part of a specified set of values.
- [`test_col_vals_make_set()`](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md):
  Test whether a set of values is entirely accounted for in a column of
  values.
- [`test_col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md):
  Test whether a set of values is a subset of a column of values.
- [`test_col_vals_increasing()`](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md):
  Test whether column data increase by row.
- [`test_col_vals_decreasing()`](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md):
  Test whether column data decrease by row.
- [`test_col_vals_null()`](https://rstudio.github.io/pointblank/reference/col_vals_null.md):
  Test whether column data are `NULL`/`NA`.
- [`test_col_vals_not_null()`](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md):
  Test whether column data are not `NULL`/`NA`.
- [`test_col_vals_regex()`](https://rstudio.github.io/pointblank/reference/col_vals_regex.md):
  Test whether strings in column data match a regex pattern.
- [`test_col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md):
  Test whether values in column data fit within a specification.
- [`test_col_vals_expr()`](https://rstudio.github.io/pointblank/reference/col_vals_expr.md):
  Test whether column data agree with a predicate expression.
- [`test_rows_distinct()`](https://rstudio.github.io/pointblank/reference/rows_distinct.md):
  Test whether row data are distinct.
- [`test_rows_complete()`](https://rstudio.github.io/pointblank/reference/rows_complete.md):
  Test whether row data are complete.
- [`test_col_is_character()`](https://rstudio.github.io/pointblank/reference/col_is_character.md):
  Test whether the columns contain character/string data.
- [`test_col_is_numeric()`](https://rstudio.github.io/pointblank/reference/col_is_numeric.md):
  Test whether the columns contain numeric values.
- [`test_col_is_integer()`](https://rstudio.github.io/pointblank/reference/col_is_integer.md):
  Test whether the columns contain integer values.
- [`test_col_is_logical()`](https://rstudio.github.io/pointblank/reference/col_is_logical.md):
  Test whether the columns contain logical values.
- [`test_col_is_date()`](https://rstudio.github.io/pointblank/reference/col_is_date.md):
  Test whether the columns contain R `Date` objects.
- [`test_col_is_posix()`](https://rstudio.github.io/pointblank/reference/col_is_posix.md):
  Test whether the columns contain `POSIXct` dates.
- [`test_col_is_factor()`](https://rstudio.github.io/pointblank/reference/col_is_factor.md):
  Test whether the columns contain R `factor` objects.
- [`test_col_exists()`](https://rstudio.github.io/pointblank/reference/col_exists.md):
  Test whether one or more columns actually exist.
- [`test_col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md):
  Test whether columns in the table (and their types) match a predefined
  schema.
- [`test_row_count_match()`](https://rstudio.github.io/pointblank/reference/row_count_match.md):
  Test whether the row count matches that of a different table.
- [`test_col_count_match()`](https://rstudio.github.io/pointblank/reference/col_count_match.md):
  Test whether the column count matches that of a different table.
- [`test_conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md):
  Test whether multiple rowwise validations result in joint validity.
- [`test_serially()`](https://rstudio.github.io/pointblank/reference/serially.md):
  Run several tests and a final validation in a serial manner as a
  combined test.
- [`test_specially()`](https://rstudio.github.io/pointblank/reference/specially.md):
  Perform a test based on a user-defined function.

## Exactly Like the `expect_*()` Functions Except You Get a `TRUE` or `FALSE`

The interface of each `test_*()` function is an exact match to the
`expect_*()` counterpart. If you haven’t used either of those but have
used the standard validation functions, here’s a quick rundown.

The following arguments from the validation functions (e.g.,
[`col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md)
and many more) have been removed in the corresponding `test_*()`
functions:

- `actions`
- `step_id`
- `label`
- `brief`
- `active`

Instead of `actions` we do get the `threshold` argument as a simplified
replacement. What’s supplied here is a single failure threshold value.
By default this is set to `1` meaning that a single test that fails will
result in an overall failure and the return of `FALSE` (otherwise,
`TRUE`).

The rules for threshold setting (in
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`warn_on_fail()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
and
[`stop_on_fail()`](https://rstudio.github.io/pointblank/reference/action_levels.md))
will be explained in some detail here. Whole numbers beyond `1` indicate
that any failing units up to that absolute threshold value will result
in a `TRUE`. Likewise, fractional values (between `0` and `1`) act as a
proportional failure threshold, where `0.25` means that 25% or more
failing test units results in a `FALSE`.

We can use the `preconditions` argument in cases where we’d like to
transform the input data before evaluation of the test. If you would
like to do things to the input table like summarize it, perform
filtering, mutate one or more columns, perform table joins, etc., then
this is a good way to go about that.

## Here’s Several Examples Quick Snap

Let’s have some examples before leaving this article. They will all use
our `small_table`:

``` r
small_table
```

    ## # A tibble: 13 × 8
    ##    date_time           date           a b             c      d e     f    
    ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
    ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-010     7   834. TRUE  low  
    ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-010     8   108. FALSE low  
    ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA  2230. TRUE  high

If you’d like to test your **pointblank** validation skill, guess
whether each of these is `TRUE` or `FALSE` before hovering over the line
of code.  
  

`small_table %>% test_col_is_logical(vars(e))`

  

`small_table %>% test_col_vals_between( vars(c), left = 1, right = 10 )`

  

`small_table %>% test_col_vals_between( vars(c), left = 0, right = vars(d), na_pass = TRUE, threshold = 0.5 )`

  

`small_table %>% test_col_vals_between( vars(c), left = 0, right = vars(d), na_pass = TRUE, threshold = 3 )`

  

`small_table %>% test_col_schema_match( col_schema(.tbl = small_table_sqlite()) )`

  

`small_table %>% test_col_vals_regex(vars(b), regex = ".-[a-z]{3}.*") `

  

`small_table %>% test_col_exists(columns = "g")`

  

`small_table %>% test_col_vals_gt( vars(z), value = vars(a), preconditions = ~ . %>% dplyr::mutate(z = c + e), threshold = 0.5 )`

  
And there you have it. A nice set of examples revealing their
truthy/falsy nature only `::after` closer inspection.
