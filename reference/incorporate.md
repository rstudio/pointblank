# Given an *informant* object, update and incorporate table snippets

When the *informant* object has a number of snippets available (by using
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md))
and the strings to use them (by using the `info_*()` functions and
`{<snippet_name>}` in the text elements), the process of incorporating
aspects of the table into the info text can occur by using the
`incorporate()` function. After that, the information will be fully
updated (getting the current state of table dimensions, re-rendering the
info text, etc.) and we can print the *informant* object or use the
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
function to see the information report.

## Usage

``` r
incorporate(informant)
```

## Arguments

- informant:

  *The pointblank informant object*

  `obj:<ptblank_informant>` // **required**

  A **pointblank** *informant* object that is commonly created through
  the use of the
  [`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
  function.

## Value

A `ptblank_informant` object.

## Examples

Take the `small_table` and assign it to `changing_table` (we'll modify
it later):

    changing_table <- small_table

    changing_table
    #> # A tibble: 13 x 8
    #>    date_time           date           a b             c      d e     f
    #>    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
    #>  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high
    #>  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low
    #>  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high
    #>  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid
    #>  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low
    #>  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid
    #>  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high
    #>  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low
    #>  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    #> 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    #> 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-010     7   834. TRUE  low
    #> 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-010     8   108. FALSE low
    #> 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA  2230. TRUE  high

Use
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
to generate an informant object with `changing_table` given to the `tbl`
argument with a leading `~` (ensures that the table will be fetched each
time it is needed, instead of being statically stored in the object).
We'll add two snippets with
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
add information with the
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
and
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md)
functions and then use `incorporate()` to work the snippets into the
info text.

    informant <-
      create_informant(
        tbl = ~ changing_table,
        tbl_name = "changing_table",
        label = "`informant()` example"
      ) %>%
      info_snippet(
        snippet_name = "row_count",
        fn = ~ . %>% nrow()
      ) %>%
      info_snippet(
        snippet_name = "col_count",
        fn = ~ . %>% ncol()
      ) %>%
      info_columns(
        columns = a,
        info = "In the range of 1 to 10. ((SIMPLE))"
      ) %>%
      info_columns(
        columns = starts_with("date"),
        info = "Time-based values (e.g., `Sys.time()`)."
      ) %>%
      info_columns(
        columns = date,
        info = "The date part of `date_time`. ((CALC))"
      ) %>%
      info_section(
        section_name = "rows",
        row_count = "There are {row_count} rows available."
      ) %>%
      incorporate()

We can print the resulting object to see the information report.

    informant

![This image was generated from the first code example in the
\`incorporate()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_incorporate_1.png)

Let's modify `test_table` to give it more rows and an extra column.

    changing_table <-
      dplyr::bind_rows(changing_table, changing_table) %>%
      dplyr::mutate(h = a + c)

Using `incorporate()` will cause the snippets to be reprocessed and
accordingly the content of the report will be updated to keep up with
the current state of the `changing_table`.

    informant <- informant %>% incorporate()

When printed again, we'll also see that the row and column counts in the
header have been updated to reflect the new dimensions of the target
table. Furthermore, the info text in the `ROWS` section has updated text
(`"There are 26 rows available."`).

    informant

![This image was generated from the second code example in the
\`incorporate()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_incorporate_2.png)

## Function ID

7-1

## See also

Other Incorporate and Report:
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
