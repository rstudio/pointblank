# Get a table from a local or remote file

If your target table is in a file, stored either locally or remotely,
the `file_tbl()` function can make it possible to access it in a single
function call. Compatible file types for this function are: CSV
(`.csv`), TSV (`.tsv`), RDA (`.rda`), and RDS (`.rds`) files. This
function generates an in-memory `tbl_df` object, which can be used as a
target table for
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
and
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md).
Another great option is supplying a table-prep formula involving
`file_tbl()` to
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
so that you have access to tables based on flat files though single
names via a table store.

In the remote data use case, we can specify a URL starting with
`http://`, `https://`, etc., and ending with the file containing the
data table. If data files are available in a GitHub repository then we
can use the
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md)
function to specify the name and location of the table data in a
repository.

## Usage

``` r
file_tbl(file, type = NULL, ..., keep = FALSE, verify = TRUE)
```

## Arguments

- file:

  The complete file path leading to a compatible data table either in
  the user system or at a `http://`, `https://`, `ftp://`, or `ftps://`
  URL. For a file hosted in a GitHub repository, a call to the
  [`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md)
  function can be used here.

- type:

  The file type. This is normally inferred by file extension and is by
  default `NULL` to indicate that the extension will dictate the type of
  file reading that is performed internally. However, if there is no
  extension (and valid extensions are `.csv`, `.tsv`, `.rda`, and
  `.rds`), we can provide the type as either of `csv`, `tsv`, `rda`, or
  `rds`.

- ...:

  Options passed to **readr**'s `read_csv()` or `read_tsv()` function.
  Both functions have the same arguments and one or the other will be
  used internally based on the file extension or an explicit value given
  to `type`.

- keep:

  In the case of a downloaded file, should it be stored in the working
  directory (`keep = TRUE`) or should it be downloaded to a temporary
  directory? By default, this is `FALSE`.

- verify:

  If `TRUE` (the default) then a verification of the data object having
  the `data.frame` class will be carried out.

## Value

A `tbl_df` object.

## Examples

### Producing tables from CSV files

A local CSV file can be obtained as a tbl object by supplying a path to
the file and some CSV reading options (the ones used by
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html))
to the `file_tbl()` function. For this example we could obtain a path to
a CSV file in the **pointblank** package with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

    csv_path <-
      system.file(
        "data_files", "small_table.csv",
        package = "pointblank"
      )

Then use that path in `file_tbl()` with the option to specify the column
types in that CSV.

    tbl <-
      file_tbl(
        file = csv_path,
        col_types = "TDdcddlc"
      )

    tbl

    ## # A tibble: 13 × 8
    ##    date_time           date           a b           c      d e     f
    ##    <dttm>              <date>     <dbl> <chr>   <dbl>  <dbl> <lgl> <chr>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE  high
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE  low
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE  high
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE mid
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE  low
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE  mid
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE  high
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE low
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high
    ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-…     7   834. TRUE  low
    ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-…     8   108. FALSE low
    ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-…    NA  2230. TRUE  high

Now that we have a \`tbl\` object that is a tibble it could be
introduced to
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
for validation.

    agent <- create_agent(tbl = tbl)

A different strategy is to provide the data-reading function call
directly to
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md):

    agent <-
      create_agent(
        tbl = ~ file_tbl(
          file = system.file(
            "data_files", "small_table.csv",
            package = "pointblank"
          ),
          col_types = "TDdcddlc"
        )
      ) %>%
      col_vals_gt(columns = a, value = 0)

All of the file-reading instructions are encapsulated in the `tbl`
expression (with the leading `~`) so the agent will always obtain the
most recent version of the table (and the logic can be translated to
YAML, for later use).

### Producing tables from files on GitHub

A CSV can be obtained from a public GitHub repo by using the
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md)
helper function. Let's create an agent a supply a table-prep formula
that gets the same CSV file from the GitHub repository for the
pointblank package.

    agent <-
      create_agent(
        tbl = ~ file_tbl(
          file = from_github(
            file = "inst/data_files/small_table.csv",
            repo = "rstudio/pointblank"
          ),
          col_types = "TDdcddlc"
        ),
        tbl_name = "small_table",
        label = "`file_tbl()` example.",
      ) %>%
      col_vals_gt(columns = a, value = 0) %>%
      interrogate()

    agent

![This image was generated from the first code example in the
\`file_tbl()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_file_tbl_1.png)

This interrogated the data that was obtained from the remote source
file, and, there's nothing to clean up (by default, the downloaded file
goes into a system temp directory).

### File access, table creation, and prep via the table store

Using table-prep formulas in a centralized table store can make it
easier to work with tables from disparate sources. Here's how to
generate a table store with two named entries for table preparations
involving the
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
and `file_tbl()` functions.

    store <-
      tbl_store(
        small_table_file ~ file_tbl(
          file = system.file(
            "data_files", "small_table.csv",
            package = "pointblank"
          ),
          col_types = "TDdcddlc"
        ),
        small_high_file ~ {{ small_table_file }} %>%
          dplyr::filter(f == "high")
      )

Now it's easy to access either of these tables via
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md).
We can reference the table in the store by its name (given to the left
of the `~`).

    tbl_get(tbl = "small_table_file", store = store)

    ## # A tibble: 13 × 8
    ##    date_time           date           a b           c      d e     f
    ##    <dttm>              <date>     <dbl> <chr>   <dbl>  <dbl> <lgl> <chr>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE  high
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE  low
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE  high
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE mid
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE  low
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE  mid
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE  high
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE low
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high
    ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-…     7   834. TRUE  low
    ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-…     8   108. FALSE low
    ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-…    NA  2230. TRUE  high

The second table in the table store is a mutated version of the first.
It's just as easily obtainable via
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md):

    tbl_get(tbl = "small_high_file", store = store)

    ## # A tibble: 6 × 8
    ##   date_time           date           a b             c     d e     f
    ##   <dttm>              <date>     <dbl> <chr>     <dbl> <dbl> <lgl> <chr>
    ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE  high
    ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE  high
    ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high
    ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high

The table-prep formulas in the `store` object could also be used in
functions with a `tbl` argument (like
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
and
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)).
This is accomplished most easily with the
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
function.

    agent <-
      create_agent(
        tbl = ~ tbl_source(
          tbl = "small_table_file",
          store = store
        )
      )

    informant <-
      create_informant(
        tbl = ~ tbl_source(
          tbl = "small_high_file",
          store = store
        )
      )

## Function ID

1-7

## See also

Other Planning and Prep:
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
[`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
