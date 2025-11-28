# A table with metadata for the `game_revenue` dataset

This table contains metadata for the `game_revenue` table. The first
column (named `column`) provides the column names from `game_revenue`.
The second column (`info`) contains descriptions for each of the columns
in that dataset. This table is in the correct format for use in the
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md)
function.

## Usage

``` r
game_revenue_info
```

## Format

A tibble with 11 rows and 2 variables:

- column:

  A `character` column with unique identifiers for each user/player.

- info:

  A `character` column that contains unique identifiers for each player
  session.

## Function ID

14-5

## See also

Other Datasets:
[`game_revenue`](https://rstudio.github.io/pointblank/reference/game_revenue.md),
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md),
[`small_table_sqlite()`](https://rstudio.github.io/pointblank/reference/small_table_sqlite.md),
[`specifications`](https://rstudio.github.io/pointblank/reference/specifications.md)

## Examples

``` r
# Here is a glimpse at the data
# available in `game_revenue_info`
dplyr::glimpse(game_revenue_info)
#> Rows: 11
#> Columns: 2
#> $ column <chr> "player_id", "session_id", "session_start", "time", "item_type"…
#> $ info   <chr> "A `character` column with unique identifiers for each user/pla…
```
