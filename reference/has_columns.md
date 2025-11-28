# Determine if one or more columns exist in a table

This utility function can help you easily determine whether a column of
a specified name is present in a table object. This function works well
enough on a table object but it can also be used as part of a formula in
any validation function's `active` argument. Using
`active = ~ . %>% has_columns(column_1)` means that the validation step
will be inactive if the target table doesn't contain a column named
`column_1`. We can also use multiple columns in
[`c()`](https://rdrr.io/r/base/c.html), so having
`active = ~ . %>% has_columns(c(column_1, column_2))` in a validation
step will make it inactive at
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
time unless the columns `column_1` and `column_2` are both present.

## Usage

``` r
has_columns(x, columns)
```

## Arguments

- x:

  *A data table*

  `obj:<tbl_*>` // **required**

  The input table. This can be a data frame, tibble, a `tbl_dbi` object,
  or a `tbl_spark` object.

- columns:

  *The target columns*

  `<tidy-select>` // *required*

  One or more columns or column-selecting expressions. Each element is
  checked for a match in the table `x`.

## Value

A length-1 logical vector.

## Examples

The `small_table` dataset in the package has the columns `date_time`,
`date`, and the `a` through `f` columns.

    small_table
    #> # A tibble: 13 x 8
    #>    date_time           date           a b             c      d e     f
    #>    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
    #>  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high
    #>  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low
    #>  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high
    #>  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid
    #>  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low
    #>  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid
    #>  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high
    #>  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low
    #>  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    #> 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    #> 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-010     7   834. TRUE  low
    #> 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-010     8   108. FALSE low
    #> 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA  2230. TRUE  high

With `has_columns()` we can check for column existence by using it
directly on the table.

    small_table %>% has_columns(columns = date)

    ## [1] TRUE

Multiple column names can be supplied. The following is `TRUE` because
both columns are present in `small_table`.

    small_table %>% has_columns(columns = c(a, b))

    ## [1] TRUE

It's possible to use a tidyselect helper as well:

    small_table %>% has_columns(columns = c(a, starts_with("b")))

    ## [1] TRUE

Because column `h` isn't present, this returns `FALSE` (all specified
columns need to be present to obtain `TRUE`).

    small_table %>% has_columns(columns = c(a, h))

    ## [1] FALSE

The same holds in the case of tidyselect helpers. Because no columns
start with `"h"`, including `starts_with("h")` returns `FALSE` for the
entire check.

    small_table %>% has_columns(columns = starts_with("h"))
    small_table %>% has_columns(columns = c(a, starts_with("h")))

    ## [1] FALSE
    ## [1] FALSE

The `has_columns()` function can be useful in expressions that involve
the target table, especially if it is uncertain that the table will
contain a column that's involved in a validation.

In the following agent-based validation, the first two steps will be
'active' because all columns checked for in the expressions are present.
The third step becomes inactive because column `j` isn't there (without
the `active` statement there we would get an evaluation failure in the
agent report).

    agent <-
      create_agent(
        tbl = small_table,
        tbl_name = "small_table"
      ) %>%
      col_vals_gt(
        columns = c, value = vars(a),
        active = ~ . %>% has_columns(c(a, c))
      ) %>%
      col_vals_lt(
        columns = h, value = vars(d),
        preconditions = ~ . %>% dplyr::mutate(h = d - a),
        active = ~ . %>% has_columns(c(a, d))
      ) %>%
      col_is_character(
        columns = j,
        active = ~ . %>% has_columns(j)
      ) %>%
      interrogate()

Through the agent's x-list, we can verify that no evaluation error (any
evaluation at all, really) had occurred. The third value, representative
of the third validation step, is actually `NA` instead of `FALSE`
because the step became inactive.

    x_list <- get_agent_x_list(agent = agent)

    x_list$eval_warning

    ## [1] FALSE FALSE    NA

## Function ID

13-2

## See also

Other Utility and Helper Functions:
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md),
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md),
[`col_schema()`](https://rstudio.github.io/pointblank/reference/col_schema.md),
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md),
[`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)
