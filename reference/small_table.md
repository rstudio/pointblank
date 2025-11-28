# A small table that is useful for testing

This is a small table with a few different types of columns. It's
probably just useful when testing the functions from **pointblank**.
Rows 9 and 10 are exact duplicates. The `c` column contains two `NA`
values.

## Usage

``` r
small_table
```

## Format

A tibble with 13 rows and 8 variables:

- date_time:

  A date-time column (of the `POSIXct` class) with dates that correspond
  exactly to those in the `date` column. Time values are somewhat
  randomized but all 'seconds' values are `00`.

- date:

  A `Date` column with dates from `2016-01-04` to `2016-01-30`.

- a:

  An `integer` column with values ranging from `1` to `8`.

- b:

  A `character` column with values that adhere to a common pattern.

- c:

  An `integer` column with values ranging from `2` to `9`. Contains two
  `NA` values.

- d:

  A numeric column with values ranging from `108` to `10000`.

- e:

  A `logical` column.

- f:

  A `character` column with `"low"`, `"mid"`, and `"high"` values.

## Function ID

14-1

## See also

Other Datasets:
[`game_revenue`](https://rstudio.github.io/pointblank/reference/game_revenue.md),
[`game_revenue_info`](https://rstudio.github.io/pointblank/reference/game_revenue_info.md),
[`small_table_sqlite()`](https://rstudio.github.io/pointblank/reference/small_table_sqlite.md),
[`specifications`](https://rstudio.github.io/pointblank/reference/specifications.md)

## Examples

``` r
# Here is a glimpse at the data
# available in `small_table`
dplyr::glimpse(small_table)
#> Rows: 13
#> Columns: 8
#> $ date_time <dttm> 2016-01-04 11:00:00, 2016-01-04 00:32:00, 2016-01-05 13:32:…
#> $ date      <date> 2016-01-04, 2016-01-04, 2016-01-05, 2016-01-06, 2016-01-09,…
#> $ a         <int> 2, 3, 6, 2, 8, 4, 7, 4, 3, 3, 4, 2, 1
#> $ b         <chr> "1-bcd-345", "5-egh-163", "8-kdg-938", "5-jdo-903", "3-ldm-0…
#> $ c         <dbl> 3, 8, 3, NA, 7, 4, 3, 2, 9, 9, 7, 8, NA
#> $ d         <dbl> 3423.29, 9999.99, 2343.23, 3892.40, 283.94, 3291.03, 843.34,…
#> $ e         <lgl> TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FAL…
#> $ f         <chr> "high", "low", "high", "mid", "low", "mid", "high", "low", "…
```
