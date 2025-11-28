# Table Transformer: get a table's column names

With any table object, you can produce a summary table that contains
table's column names. The output summary table will have two columns and
as many rows as there are columns in the input table. The first column
is the `".param."` column, which is an integer-based column containing
the indices of the columns from the input table. The second column,
`"value"`, contains the column names from the input table.

## Usage

``` r
tt_tbl_colnames(tbl)
```

## Arguments

- tbl:

  *A data table*

  `obj:<tbl_*>` // **required**

  A table object to be used as input for the transformation. This can be
  a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.

## Value

A `tibble` object.

## Examples

Get the column names of the `game_revenue` dataset that is included in
the **pointblank** package.

    tt_tbl_colnames(tbl = game_revenue)
    #> # A tibble: 11 x 2
    #>    .param. value
    #>      <int> <chr>
    #>  1       1 player_id
    #>  2       2 session_id
    #>  3       3 session_start
    #>  4       4 time
    #>  5       5 item_type
    #>  6       6 item_name
    #>  7       7 item_revenue
    #>  8       8 session_duration
    #>  9       9 start_day
    #> 10      10 acquisition
    #> 11      11 country

This output table is useful when you want to validate the column names
of the table. Here, we check that `game_revenue` table, included in the
**pointblank** package, has certain column names present with
[`test_col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md).

    tt_tbl_colnames(tbl = game_revenue) %>%
      test_col_vals_make_subset(
        columns = value,
        set = c("acquisition", "country")
      )
    #> [1] TRUE

We can check to see whether the column names in the `specifications`
table are all less than `15` characters in length. For this, we would
use the combination of `tt_tbl_colnames()`, then
[`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md),
and finally
[`test_col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md)
to perform the test.

    specifications %>%
      tt_tbl_colnames() %>%
      tt_string_info() %>%
      test_col_vals_lt(
        columns = value,
        value = 15
      )
    #> [1] FALSE

This returned `FALSE` and this is because the column name
`credit_card_numbers` is 16 characters long.

## Function ID

12-4

## See also

Other Table Transformers:
[`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md),
[`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md),
[`tt_summary_stats()`](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md),
[`tt_tbl_dims()`](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md),
[`tt_time_shift()`](https://rstudio.github.io/pointblank/reference/tt_time_shift.md),
[`tt_time_slice()`](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
