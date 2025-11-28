# A table with game revenue data

This table is a subset of the `sj_all_revenue` table from the
**intendo** data package. It's the first 2,000 rows from that table
where revenue records range from `2015-01-01` to `2015-01-21`.

## Usage

``` r
game_revenue
```

## Format

A tibble with 2,000 rows and 11 variables:

- player_id:

  A `character` column with unique identifiers for each user/player.

- session_id:

  A `character` column that contains unique identifiers for each player
  session.

- session_start:

  A date-time column that indicates when the session (containing the
  revenue event) started.

- time:

  A date-time column that indicates exactly when the player purchase (or
  revenue event) occurred.

- item_type:

  A `character` column that provides the class of the item purchased.

- item_name:

  A `character` column that provides the name of the item purchased.

- item_revenue:

  A `numeric` column with the revenue amounts per item purchased.

- session_duration:

  A `numeric` column that states the length of the session (in minutes)
  for which the purchase occurred.

- start_day:

  A `Date` column that provides the date of first login for the player
  making a purchase.

- acquisition:

  A `character` column that provides the method of acquisition for the
  player.

- country:

  A `character` column that provides the probable country of residence
  for the player.

## Function ID

14-4

## See also

Other Datasets:
[`game_revenue_info`](https://rstudio.github.io/pointblank/reference/game_revenue_info.md),
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md),
[`small_table_sqlite()`](https://rstudio.github.io/pointblank/reference/small_table_sqlite.md),
[`specifications`](https://rstudio.github.io/pointblank/reference/specifications.md)

## Examples

``` r
# Here is a glimpse at the data
# available in `game_revenue`
dplyr::glimpse(game_revenue)
#> Rows: 2,000
#> Columns: 11
#> $ player_id        <chr> "ECPANOIXLZHF896", "ECPANOIXLZHF896", "ECPANOIXLZHF89…
#> $ session_id       <chr> "ECPANOIXLZHF896-eol2j8bs", "ECPANOIXLZHF896-eol2j8bs…
#> $ session_start    <dttm> 2015-01-01 01:31:03, 2015-01-01 01:31:03, 2015-01-01…
#> $ time             <dttm> 2015-01-01 01:31:27, 2015-01-01 01:36:57, 2015-01-01…
#> $ item_type        <chr> "iap", "iap", "iap", "ad", "ad", "ad", "ad", "ad", "a…
#> $ item_name        <chr> "offer2", "gems3", "gold7", "ad_20sec", "ad_5sec", "a…
#> $ item_revenue     <dbl> 8.991, 22.491, 107.991, 0.760, 0.030, 0.070, 0.080, 1…
#> $ session_duration <dbl> 16.3, 16.3, 16.3, 16.3, 35.2, 35.2, 35.2, 35.2, 35.2,…
#> $ start_day        <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01, 2015…
#> $ acquisition      <chr> "google", "google", "google", "google", "google", "go…
#> $ country          <chr> "Germany", "Germany", "Germany", "Germany", "Germany"…
```
