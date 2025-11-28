# Generate a useful text 'snippet' from the target table

Getting little snippets of information from a table goes hand-in-hand
with mixing those bits of info with your table info. Call
`info_snippet()` to define a snippet and how you'll get that from the
target table. The snippet definition is supplied either with a formula,
or, with a **pointblank**-supplied `snip_*()` function. So long as you
know how to interact with a table and extract information, you can
easily define snippets for a *informant* object. And once those snippets
are defined, you can insert them into the *info text* as defined through
the other `info_*()` functions
([`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
and
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md)).
Use curly braces with just the `snippet_name` inside (e.g.,
`"This column has {n_cat} categories."`).

## Usage

``` r
info_snippet(x, snippet_name, fn)
```

## Arguments

- x:

  *The pointblank informant object*

  `obj:<ptblank_informant>` // **required**

  A **pointblank** *informant* object that is commonly created through
  the use of the
  [`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
  function.

- snippet_name:

  *The snippet name*

  `scalar<character>` // **required**

  The name for snippet, which is used for interpolating the result of
  the snippet formula into *info text* defined by an `info_*()`
  function.

- fn:

  *Function for snippet text generation*

  `<function>` // **required**

  A formula that obtains a snippet of data from the target table. It's
  best to use a leading dot (`.`) that stands for the table itself and
  use pipes to construct a series of operations to be performed on the
  table (e.g., `~ . %>% dplyr::pull(column_2) %>% max(na.rm = TRUE)`).
  So long as the result is a length-1 vector, it'll likely be valid for
  insertion into some info text. Alternatively, a `snip_*()` function
  can be used here (these functions always return a formula that's
  suitable for all types of data sources).

## Value

A `ptblank_informant` object.

## Snip functions provided in **pointblank**

For convenience, there are several `snip_*()` functions provided in the
package that work on column data from the *informant*'s target table.
These are:

- [`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md):
  get a list of column categories

- [`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md):
  get an inline statistical summary

- [`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md):
  get the lowest value from a column

- [`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md)
  : get the highest value from a column

As it's understood what the target table is, only the `column` in each
of these functions is necessary for obtaining the resultant text.

## YAML

A **pointblank** informant can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used to regenerate an informant (with
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md))
or perform the 'incorporate' action using the target table (via
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md)).
Snippets are stored in the YAML representation and here is is how they
are expressed in both R code and in the YAML output (showing both the
`meta_snippets` and `columns` keys to demonstrate their relationship
here).

    # R statement
    informant %>%
      info_columns(
        columns = date_time,
        `Latest Date` = "The latest date is {latest_date}."
      ) %>%
      info_snippet(
        snippet_name = "latest_date",
        fn = ~ . %>% dplyr::pull(date) %>% max(na.rm = TRUE)
      ) %>%
      incorporate()

    # YAML representation
    meta_snippets:
      latest_date: ~. %>% dplyr::pull(date) %>% max(na.rm = TRUE)
    ...
    columns:
      date_time:
        _type: POSIXct, POSIXt
        Latest Date: The latest date is {latest_date}.
      date:
        _type: Date
      item_count:
        _type: integer

## Examples

Take the `small_table` dataset included in **pointblank** and assign it
to `test_table`. We'll modify it later.

    test_table <- small_table

Generate an informant object, add two snippets with `info_snippet()`,
add information with some other `info_*()` functions and then
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
the snippets into the info text. The first snippet will be made with the
expression `~ . %>% nrow()` (giving us the number of rows in the
dataset) and the second uses the
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md)
function with column `a` (giving us the highest value in that column).

    informant <-
      create_informant(
        tbl = ~ test_table,
        tbl_name = "test_table",
        label = "An example."
      ) %>%
      info_snippet(
        snippet_name = "row_count",
        fn = ~ . %>% nrow()
      ) %>%
      info_snippet(
        snippet_name = "max_a",
        fn = snip_highest(column = "a")
      ) %>%
      info_columns(
        columns = a,
        info = "In the range of 1 to {max_a}. ((SIMPLE))"
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

We can print the `informant` object to see the information report.

    informant

![This image was generated from the first code example in the
\`info_snippet()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_info_snippet_1.png)

Let's modify `test_table` with some **dplyr** to give it more rows and
an extra column.

    test_table <-
      dplyr::bind_rows(test_table, test_table) %>%
      dplyr::mutate(h = a + c)

Using
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
on the `informant` object will cause the snippets to be reprocessed,
and, the info text to be updated.

    informant <- informant %>% incorporate()

    informant

![This image was generated from the second code example in the
\`info_snippet()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_info_snippet_2.png)

## Function ID

3-5

## See also

Other Information Functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md),
[`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md),
[`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md)
