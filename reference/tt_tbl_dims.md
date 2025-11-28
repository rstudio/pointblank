# Table Transformer: get the dimensions of a table

With any table object, you can produce a summary table that contains
nothing more than the table's dimensions: the number of rows and the
number of columns. The output summary table will have two columns and
two rows. The first is the `".param."` column with the labels `"rows"`
and `"columns"`; the second column, `"value"`, contains the row and
column counts.

## Usage

``` r
tt_tbl_dims(tbl)
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

Get the dimensions of the `game_revenue` dataset that is included in the
**pointblank** package.

    tt_tbl_dims(tbl = game_revenue)
    #> # A tibble: 2 x 2
    #>   .param. value
    #>   <chr>   <int>
    #> 1 rows     2000
    #> 2 columns    11

This output table is useful when a table validation depends on its
dimensions. Here, we check that `game_revenue` has at least `1500` rows.

    tt_tbl_dims(tbl = game_revenue) %>%
      dplyr::filter(.param. == "rows") %>%
      test_col_vals_gt(
        columns = value,
        value = 1500
      )
    #> [1] TRUE

We can check `small_table` to ensure that number of columns is less than
`10`.

    tt_tbl_dims(tbl = small_table) %>%
      dplyr::filter(.param. == "columns") %>%
      test_col_vals_lt(
        columns = value,
        value = 10
      )
    #> [1] TRUE

## Function ID

12-3

## See also

Other Table Transformers:
[`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md),
[`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md),
[`tt_summary_stats()`](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md),
[`tt_tbl_colnames()`](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md),
[`tt_time_shift()`](https://rstudio.github.io/pointblank/reference/tt_time_shift.md),
[`tt_time_slice()`](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
