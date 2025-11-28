# Table Transformer: obtain a summary table for string columns

With any table object, you can produce a summary table that is scoped to
string-based columns. The output summary table will have a leading
column called `".param."` with labels for each of the three rows, each
corresponding to the following pieces of information pertaining to
string length:

1.  Mean String Length (`"length_mean"`)

2.  Minimum String Length (`"length_min"`)

3.  Maximum String Length (`"length_max"`)

Only string data from the input table will generate columns in the
output table. Column names from the input will be used in the output,
preserving order as well.

## Usage

``` r
tt_string_info(tbl)
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

Get string information for the string-based columns in the
`game_revenue` dataset that is included in the **pointblank** package.

    tt_string_info(tbl = game_revenue)
    #> # A tibble: 3 x 7
    #>   .param.     player_id session_id item_type item_name acquisition country
    #>   <chr>           <dbl>      <dbl>     <dbl>     <dbl>       <dbl>   <dbl>
    #> 1 length_mean        15         24      2.22      7.35        7.97    8.53
    #> 2 length_min         15         24      2         5           5       5
    #> 3 length_max         15         24      3        11          14      14

Ensure that `player_id` and `session_id` values always have the same
fixed numbers of characters (`15` and `24`, respectively) throughout the
table.

    tt_string_info(tbl = game_revenue) %>%
      col_vals_equal(
        columns = player_id,
        value = 15
      ) %>%
      col_vals_equal(
        columns = session_id,
        value = 24
      )
    #> # A tibble: 3 x 7
    #>   .param.     player_id session_id item_type item_name acquisition country
    #>   <chr>           <dbl>      <dbl>     <dbl>     <dbl>       <dbl>   <dbl>
    #> 1 length_mean        15         24      2.22      7.35        7.97    8.53
    #> 2 length_min         15         24      2         5           5       5
    #> 3 length_max         15         24      3        11          14      14

We see data, and not an error, so both validations were successful!

Let's use a `tt_string_info()`-transformed table with the
[`test_col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md)
to check that the maximum string length in column `f` of the
`small_table` dataset is no greater than `4`.

    tt_string_info(tbl = small_table) %>%
      test_col_vals_lte(
        columns = f,
        value = 4
      )
    #> [1] TRUE

## Function ID

12-2

## See also

Other Table Transformers:
[`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md),
[`tt_summary_stats()`](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md),
[`tt_tbl_colnames()`](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md),
[`tt_tbl_dims()`](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md),
[`tt_time_shift()`](https://rstudio.github.io/pointblank/reference/tt_time_shift.md),
[`tt_time_slice()`](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
