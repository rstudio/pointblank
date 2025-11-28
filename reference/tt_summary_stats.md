# Table Transformer: obtain a summary stats table for numeric columns

With any table object, you can produce a summary table that is scoped to
the numeric column values. The output summary table will have a leading
column called `".param."` with labels for each of the nine rows, each
corresponding to the following summary statistics:

1.  Minimum (`"min"`)

2.  5th Percentile (`"p05"`)

3.  1st Quartile (`"q_1"`)

4.  Median (`"med"`)

5.  3rd Quartile (`"q_3"`)

6.  95th Percentile (`"p95"`)

7.  Maximum (`"max"`)

8.  Interquartile Range (`"iqr"`)

9.  Range (`"range"`)

Only numerical data from the input table will generate columns in the
output table. Column names from the input will be used in the output,
preserving order as well.

## Usage

``` r
tt_summary_stats(tbl)
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

Get summary statistics for the `game_revenue` dataset that is included
in the **pointblank** package.

    tt_summary_stats(tbl = game_revenue)
    #> # A tibble: 9 x 3
    #>   .param. item_revenue session_duration
    #>   <chr>          <dbl>            <dbl>
    #> 1 min             0                 3.2
    #> 2 p05             0.02              8.2
    #> 3 q_1             0.09             18.5
    #> 4 med             0.38             26.5
    #> 5 q_3             1.25             33.8
    #> 6 p95            22.0              39.5
    #> 7 max           143.               41
    #> 8 iqr             1.16             15.3
    #> 9 range         143.               37.8

Table transformers work great in conjunction with validation functions.
Let's ensure that the maximum revenue for individual purchases in the
`game_revenue` table is less than \$150.

    tt_summary_stats(tbl = game_revenue) %>%
      col_vals_lt(
        columns = item_revenue,
        value = 150,
        segments = .param. ~ "max"
      )
    #> # A tibble: 9 x 3
    #>   .param. item_revenue session_duration
    #>   <chr>          <dbl>            <dbl>
    #> 1 min             0                 3.2
    #> 2 p05             0.02              8.2
    #> 3 q_1             0.09             18.5
    #> 4 med             0.38             26.5
    #> 5 q_3             1.25             33.8
    #> 6 p95            22.0              39.5
    #> 7 max           143.               41
    #> 8 iqr             1.16             15.3
    #> 9 range         143.               37.8

We see data, and not an error, so the validation was successful!

Let's do another: for in-app purchases in the `game_revenue` table,
check that the median revenue is somewhere between \$8 and \$12.

    game_revenue %>%
      dplyr::filter(item_type == "iap") %>%
      tt_summary_stats() %>%
      col_vals_between(
        columns = item_revenue,
        left = 8, right = 12,
        segments = .param. ~ "med"
      )
    #> # A tibble: 9 x 3
    #>   .param. item_revenue session_duration
    #>   <chr>          <dbl>            <dbl>
    #> 1 min             0.4              3.2
    #> 2 p05             1.39             5.99
    #> 3 q_1             4.49            14.0
    #> 4 med            10.5             22.6
    #> 5 q_3            20.3             30.6
    #> 6 p95            66.0             38.8
    #> 7 max           143.              41
    #> 8 iqr            15.8             16.7
    #> 9 range         143.              37.8

We can get more creative with this transformer. Why not use a
transformed table in a validation plan? While performing validations of
the `game_revenue` table with an agent we can include the same revenue
check as above by using `tt_summary_stats()` in the `preconditions`
argument. This transforms the target table into a summary table for the
validation step. The final step of the transformation in `preconditions`
is a
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
step that isolates the row of the median statistic.

    agent <-
      create_agent(
        tbl = game_revenue,
        tbl_name = "game_revenue",
        label = "`tt_summary_stats()` example.",
        actions = action_levels(
          warn_at = 0.10,
          stop_at = 0.25,
          notify_at = 0.35
        )
      ) %>%
      rows_complete() %>%
      rows_distinct() %>%
      col_vals_between(
        columns = item_revenue,
        left = 8, right = 12,
        preconditions = ~ . %>%
          dplyr::filter(item_type == "iap") %>%
          tt_summary_stats() %>%
          dplyr::filter(.param. == "med")
      ) %>%
      interrogate()

Printing the `agent` in the console shows the validation report in the
Viewer. Here is an excerpt of validation report. Take note of the final
step (`STEP 3`) as it shows the entry that corresponds to the
[`col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
validation step that uses the summary stats table as its target.

![This image was generated from the first code example in the
\`tt_summary_stats()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_tt_summary_stats_1.png)

## Function ID

12-1

## See also

Other Table Transformers:
[`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md),
[`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md),
[`tt_tbl_colnames()`](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md),
[`tt_tbl_dims()`](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md),
[`tt_time_shift()`](https://rstudio.github.io/pointblank/reference/tt_time_shift.md),
[`tt_time_slice()`](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
