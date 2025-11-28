# A `fn` for `info_snippet()`: get the highest value from a column

The `snip_highest()` function can be used as an
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
function (i.e., provided to `fn`) to get the highest numerical, time
value, or alphabetical value from a column in the target table.

## Usage

``` r
snip_highest(column)
```

## Arguments

- column:

  *The target column*

  `scalar<character>` // **required**

  The name of the column that contains the target values.

## Value

A formula needed for
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)'s
`fn` argument.

## Examples

Generate an informant object, add a snippet with
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
and `snip_highest()` (giving us a method to get the highest value in
column `a`); define a location for the snippet result in
[`{ }`](https://rdrr.io/r/base/Paren.html) and then
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
the snippet into the info text. Note here that the order of the
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
and
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
calls doesn't matter.

    informant <-
      create_informant(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "An example."
      ) %>%
      info_columns(
        columns = a,
        `Highest Value` = "Highest value is {highest_a}."
      ) %>%
      info_snippet(
        snippet_name = "highest_a",
        fn = snip_highest(column = "a")
      ) %>%
      incorporate()

We can print the `informant` object to see the information report.

    informant

![This image was generated from the first code example in the
\`snip_highest()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_snip_highest_1.png)

## Function ID

3-9

## See also

Other Information Functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md),
[`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md)
