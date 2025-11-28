# Add column information from another data table

The `info_columns_from_tbl()` function is a wrapper around the
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
function and is useful if you wish to apply *info text* to columns where
that information already exists in a data frame (or in some form that
can readily be coaxed into a data frame). The form of the input `tbl`
(the one that contains column metadata) has a few basic requirements:

- the data frame must have two columns

- both columns must be of class `character`

- the first column should contain column names and the second should
  contain the *info text*

Each column that matches across tables (i.e., the `tbl` and the target
table of the informant) will have a new entry for the `"info"` property.
Empty or missing info text will be pruned from `tbl`.

## Usage

``` r
info_columns_from_tbl(x, tbl, .add = TRUE)
```

## Arguments

- x:

  *The pointblank informant object*

  `obj:<ptblank_informant>` // **required**

  A **pointblank** *informant* object that is commonly created through
  the use of the
  [`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
  function.

- tbl:

  *Metadata table with column information*

  `obj:<tbl_*>` // **required**

  The two-column data frame which contains metadata about the target
  table in the informant object.

- .add:

  *Add to existing info text*

  `scalar<logical>` // *default:* `TRUE`

  Should new text be added to existing text? This is `TRUE` by default;
  setting to `FALSE` replaces any existing text for the `"info"`
  property.

## Value

A `ptblank_informant` object.

## Examples

Create a pointblank `informant` object with
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md).
We can specify a `tbl` with the `~` followed by a statement that gets
the `game_revenue` dataset.

    informant <-
      create_informant(
        tbl = ~ game_revenue,
        tbl_name = "game_revenue",
        label = "An example."
      )

We can add *info text* to describe the data in the various columns of
the table by using
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
or information in another table (with `info_columns_from_tbl()`). Here,
we'll do the latter. The `game_revenue_info` dataset is included in
**pointblank** and it contains metadata for `game_revenue`.

    game_revenue_info
    #> # A tibble: 11 x 2
    #>    column           info
    #>    <chr>            <chr>
    #>  1 player_id        A `character` column with unique identifiers for each user/~
    #>  2 session_id       A `character` column that contains unique identifiers for e~
    #>  3 session_start    A date-time column that indicates when the session (contain~
    #>  4 time             A date-time column that indicates exactly when the player p~
    #>  5 item_type        A `character` column that provides the class of the item pu~
    #>  6 item_name        A `character` column that provides the name of the item pur~
    #>  7 item_revenue     A `numeric` column with the revenue amounts per item purcha~
    #>  8 session_duration A `numeric` column that states the length of the session (i~
    #>  9 start_day        A `Date` column that provides the date of first login for t~
    #> 10 acquisition      A `character` column that provides the method of acquisitio~
    #> 11 country          A `character` column that provides the probable country of ~

The `info_columns_from_tbl()` function takes a table object where the
first column has the column names and the second contains the *info
text*.

    informant <-
      informant %>%
      info_columns_from_tbl(tbl = game_revenue_info)

Upon printing the `informant` object, we see the additions made to the
'Columns' section by the
`info_columns_from_tbl(tbl = game_revenue_info)` call.

    informant

![This image was generated from the first code example in the
\`info_columns_from_tbl()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_info_columns_from_tbl_1.png)

We can continue to add more *info text* to describe the columns since
the process is additive. The `info_columns_from_tbl()` function
populates the `info` subsection and any calls of
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
that also target a `info` subsection will append text. Here, we'll add
content for the `item_revenue` and `acquisition` columns and view the
updated report.

    informant <-
      informant %>%
      info_columns(
        columns = item_revenue,
        info = "Revenue reported in USD."
      ) %>%
      info_columns(
        columns = acquisition,
        `top list` = "{top5_aq}"
      ) %>%
      info_snippet(
        snippet_name = "top5_aq",
        fn = snip_list(column = "acquisition")
      ) %>%
      incorporate()

    informant

![This image was generated from the second code example in the
\`info_columns_from_tbl()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_info_columns_from_tbl_2.png)

## Function ID

3-3

## See also

The
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
function, which allows for manual entry of *info text*.

Other Information Functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md),
[`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md),
[`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md)
