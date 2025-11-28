# Introduction to the Expectations in Unit Tests Workflow (VALID-III)

![](images/VALID-III.svg)

The *VALID-III: Expectations in Unit Tests* workflow is all about
checking your data alongside your usual **testthat** tests. The
functions used for this workflow all begin with the `expect_` prefix and
are based on the set of validation functions used in the
[**VALID-I**](https://rstudio.github.io/pointblank/articles/VALID-I.md)
and
[**VALID-II**](https://rstudio.github.io/pointblank/articles/VALID-II.md)
workflows. Here’s the complete list of functions with a phrase for each
function’s expectation:

- [`expect_col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md):
  Expect that column data are less than a specified value.
- [`expect_col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md):
  Expect that column data are less than or equal to a specified value.
- [`expect_col_vals_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_equal.md):
  Expect that column data are equal to a specified value.
- [`expect_col_vals_not_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md):
  Expect that column data are not equal to a specified value.
- [`expect_col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md):
  Expect that column data are greater than or equal to a specified
  value.
- [`expect_col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md):
  Expect that column data are greater than a specified value.
- [`expect_col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md):
  Expect that column data are between two specified values.
- [`expect_col_vals_not_between()`](https://rstudio.github.io/pointblank/reference/col_vals_not_between.md):
  Expect that column data are not between two specified values.
- [`expect_col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md):
  Expect that column data are part of a specified set of values.
- [`expect_col_vals_not_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md):
  Expect that column data are not part of a specified set of values.
- [`expect_col_vals_make_set()`](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md):
  Expect that a set of values entirely accounted for in a column of
  values.
- [`expect_col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md):
  Expect that a set of values a subset of a column of values.
- [`expect_col_vals_increasing()`](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md):
  Expect column data increasing by row.
- [`expect_col_vals_decreasing()`](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md):
  Expect column data decreasing by row.
- [`expect_col_vals_null()`](https://rstudio.github.io/pointblank/reference/col_vals_null.md):
  Expect that column data are `NULL`/`NA`.
- [`expect_col_vals_not_null()`](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md):
  Expect that column data are not `NULL`/`NA`.
- [`expect_col_vals_regex()`](https://rstudio.github.io/pointblank/reference/col_vals_regex.md):
  Expect that strings in column data match a regex pattern.
- [`expect_col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md):
  Expect that values in column data fit within a specification.
- [`expect_col_vals_expr()`](https://rstudio.github.io/pointblank/reference/col_vals_expr.md):
  Expect that column data agree with a predicate expression.
- [`expect_rows_distinct()`](https://rstudio.github.io/pointblank/reference/rows_distinct.md):
  Expect that row data are distinct.
- [`expect_rows_complete()`](https://rstudio.github.io/pointblank/reference/rows_complete.md):
  Expect that row data are complete.
- [`expect_col_is_character()`](https://rstudio.github.io/pointblank/reference/col_is_character.md):
  Expect that the columns contain character/string data.
- [`expect_col_is_numeric()`](https://rstudio.github.io/pointblank/reference/col_is_numeric.md):
  Expect that the columns contain numeric values.
- [`expect_col_is_integer()`](https://rstudio.github.io/pointblank/reference/col_is_integer.md):
  Expect that the columns contain integer values.
- [`expect_col_is_logical()`](https://rstudio.github.io/pointblank/reference/col_is_logical.md):
  Expect that the columns contain logical values.
- [`expect_col_is_date()`](https://rstudio.github.io/pointblank/reference/col_is_date.md):
  Expect that the columns contain R `Date` objects.
- [`expect_col_is_posix()`](https://rstudio.github.io/pointblank/reference/col_is_posix.md):
  Expect that the columns contain `POSIXct` dates.
- [`expect_col_is_factor()`](https://rstudio.github.io/pointblank/reference/col_is_factor.md):
  Expect that the columns contain R `factor` objects.
- [`expect_col_exists()`](https://rstudio.github.io/pointblank/reference/col_exists.md):
  Expect that one or more columns actually exist.
- [`expect_col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md):
  Expect that columns in the table (and their types) match a predefined
  schema.
- [`expect_row_count_match()`](https://rstudio.github.io/pointblank/reference/row_count_match.md):
  Expect that the row count matches that of a different table.
- [`expect_col_count_match()`](https://rstudio.github.io/pointblank/reference/col_count_match.md):
  Expect that the column count matches that of a different table.
- [`expect_conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md):
  Expect that multiple rowwise validations result in joint validity.
- [`expect_serially()`](https://rstudio.github.io/pointblank/reference/serially.md):
  Run several tests and a final validation in a serial manner with a
  passing expectation.
- [`expect_specially()`](https://rstudio.github.io/pointblank/reference/specially.md):
  Perform a specialized validation with a user-defined function with a
  passing expectation.

Now that we know that the functions are similar in intent but different
in name, let’s learn how to use these functions effectively.

## Using `expect_*()` Functions in the **testthat** Way

The **testthat** package has collection of functions that begin with
`expect_`. It’s no coincidence that the **pointblank** for the
**VALID-III** workflow adopts the same naming convention. The idea is to
use these functions interchangeably with those from **testthat** in the
standard **testthat** workflow (in a `test-<name>.R` file, inside the
`tests/testthat` folder). The big difference here is that instead of
testing function outputs, we are testing data tables. However, tables
may be returned from function calls and the `expect_*()` functions
offered up by **pointblank** might offer more flexibility for testing
that data. For instance
[`expect_col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
allows us to write an expectation with fine control on boundary values
(and whether they are inclusive bounds), whether `NA` values should be
ignored, and we can even set a failure threshold if that makes sense for
the expectation.

Here’s an example of how to generate some tests on data with
**testthat** and also with **pointblank**. For the `small_table`
dataset, let’s write expectations that show that non-NA values in column
`c` are between `2` and `9`.

``` r
testthat::expect_true(all(na.omit(small_table$c) >= 2))
testthat::expect_true(all(na.omit(small_table$c) <= 9))
```

There is no **testthat** function that tests for values between two
values. The original strategy was to use
[`testthat::expect_gte()`](https://testthat.r-lib.org/reference/comparison-expectations.html)
and `testthat::lte()` with `small_table$c` as the `object` in both,
however, that doesn’t work because that results in a logical vector
greater than length 1. Also, there is no allowance for `NA` values to be
skipped. The best I could do was the above.

The **pointblank** version of this task makes for a more succinct and
understandable expectation expression:

``` r
expect_col_vals_between(small_table, c, 2, 9, na_pass = TRUE)
```

The arguments in the
[`expect_col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
give us everything we need to check tabular data without having to do
subsetting and perform other transformations. There are a few added
benefits. Should data come from a data source other than a local data
frame, the SQL expressions are handled internally and they have been
tested extensively across all the supported database types and in Spark
DataFrames as well.

## These `expect_*()` Functions Are Simpler Than Their Counterparts

All of the `expect_*()` functions have the same leading arguments of
their validation function counterparts but they omit the following
arguments at the end of their signatures:

- `actions`
- `step_id`
- `label`
- `brief`
- `active`

While we lose the `actions` argument, we get in its place the
`threshold` argument. This is a simple failure threshold value for use
with the expectation (`expect_*`) and the test (`test_*`) functions. By
default, `threshold` is set to `1` which means that any single test unit
failing will result in an overall failure (i.e., the expectation will
fail).

As with the thresholds set in the
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
functions (or the shortcut functions
[`warn_on_fail()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
and
[`stop_on_fail()`](https://rstudio.github.io/pointblank/reference/action_levels.md)),
whole numbers beyond `1` indicate that any failing units up to that
absolute threshold value will result in a succeeding expectation.
Likewise, fractional values (between `0` and `1`) act as a proportional
failure threshold, where `0.25` means that 25% of failing test units
results in a failed expectation.

The `preconditions` argument can be used to transform the input data
before evaluation of the expectation. This is useful is some cases where
you might need to summarize the input data table, mutate columns,
perform some filtering, or even perform table joins beforehand.
