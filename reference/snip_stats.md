# A `fn` for `info_snippet()`: get an inline statistical summary

The `snip_stats()` function can be used as an
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
function (i.e., provided to `fn`) to produce a five- or seven-number
statistical summary. This inline summary works well within a paragraph
of text and can help in describing the distribution of numerical values
in a column.

For a given column, three different types of inline statistical
summaries can be provided:

1.  a five-number summary (`"5num"`): minimum, Q1, median, Q3, maximum

2.  a seven-number summary (`"7num"`): P2, P9, Q1, median, Q3, P91, P98

3.  Bowley's seven-figure summary (`"bowley"`): minimum, P10, Q1,
    median, Q3, P90, maximum

## Usage

``` r
snip_stats(column, type = c("5num", "7num", "bowley"))
```

## Arguments

- column:

  *The target column*

  `scalar<character>` // **required**

  The name of the column that contains the target values.

- type:

  *Type of statistical summary*

  `singl-kw:[5num|7num|bowley]` // *default:* `"5num"`

  The type of summary. By default, the `"5num"` keyword is used to
  generate a five-number summary. Two other options provide seven-number
  summaries: `"7num"` and `"bowley"`.

## Value

A formula needed for
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)'s
`fn` argument.

## Examples

Generate an informant object, add a snippet with
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
and `snip_stats()` (giving us a method to get some summary stats for
column `d`). Define a location for the snippet result in
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
        columns = d,
        `Stats` = "Stats (fivenum): {stats_d}."
      ) %>%
      info_snippet(
        snippet_name = "stats_d",
        fn = snip_stats(column = "d")
      ) %>%
      incorporate()

We can print the `informant` object to see the information report.

    informant

![This image was generated from the first code example in the
\`snip_stats()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_snip_stats_1.png)

## Function ID

3-7

## See also

Other Information Functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md),
[`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md)
