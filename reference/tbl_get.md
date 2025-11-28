# Obtain a materialized table via a table store

The `tbl_get()` function gives us the means to materialize a table that
has an entry in a table store (i.e., has a table-prep formula with a
unique name). The table store that is used for this can be in the form
of a `tbl_store` object (created with the
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
function) or an on-disk YAML representation of a table store (created by
using
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
with a `tbl_store` object).

Should you want a table-prep formula from a table store to use as a
value for `tbl` (in
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
or
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md)),
then have a look at the
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
function.

## Usage

``` r
tbl_get(tbl, store = NULL)
```

## Arguments

- tbl:

  The table to retrieve from a table `store`. This table could be
  identified by its name (e.g., `tbl = "large_table"`) or by supplying a
  reference using a subset (with `$`) of the `tbl_store` object (e.g.,
  `tbl = store$large_table`). If using the latter method then nothing
  needs to be supplied to `store`.

- store:

  Either a table store object created by the
  [`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
  function or a path to a table store YAML file created by
  [`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md).

## Value

A table object.

## Examples

Define a `tbl_store` object by adding several table-prep formulas in
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md).

    store <-
      tbl_store(
        small_table_duck ~ db_tbl(
          table = small_table,
          dbname = ":memory:",
          dbtype = "duckdb"
        ),
        ~ db_tbl(
          table = "rna",
          dbname = "pfmegrnargs",
          dbtype = "postgres",
          host = "hh-pgsql-public.ebi.ac.uk",
          port = 5432,
          user = I("reader"),
          password = I("NWDMCE5xdipIjRrp")
        ),
        sml_table ~ pointblank::small_table
      )

Once this object is available, we can access the tables named:
`"small_table_duck"`, `"rna"`, and `"sml_table"`. Let's check that the
`"rna"` table is accessible through `tbl_get()`:

    tbl_get(
      tbl = "rna",
      store = store
    )

    ## # Source:   table<rna> [?? x 9]
    ## # Database: postgres [reader@hh-pgsql-public.ebi.ac.uk:5432/pfmegrnargs]
    ##          id upi        timestamp           userstamp crc64   len seq_short
    ##     <int64> <chr>      <dttm>              <chr>     <chr> <int> <chr>
    ##  1 24583872 URS000177… 2019-12-02 13:26:08 rnacen    C380…   511 ATTGAACG…
    ##  2 24583873 URS000177… 2019-12-02 13:26:08 rnacen    BC42…   390 ATGGGCGA…
    ##  3 24583874 URS000177… 2019-12-02 13:26:08 rnacen    19A5…   422 CTACGGGA…
    ##  4 24583875 URS000177… 2019-12-02 13:26:08 rnacen    66E1…   534 AGGGTTCG…
    ##  5 24583876 URS000177… 2019-12-02 13:26:08 rnacen    CC8F…   252 TACGTAGG…
    ##  6 24583877 URS000177… 2019-12-02 13:26:08 rnacen    19E4…   413 ATGGGCGA…
    ##  7 24583878 URS000177… 2019-12-02 13:26:08 rnacen    AE91…   253 TACGAAGG…
    ##  8 24583879 URS000177… 2019-12-02 13:26:08 rnacen    E21A…   304 CAGCAGTA…
    ##  9 24583880 URS000177… 2019-12-02 13:26:08 rnacen    1AA7…   460 CCTACGGG…
    ## 10 24583881 URS000177… 2019-12-02 13:26:08 rnacen    2046…   440 CCTACGGG…
    ## # … with more rows, and 2 more variables: seq_long <chr>, md5 <chr>

An alternative method for getting the same table materialized is by
using `$` to get the formula of choice from `tbls` and passing that to
`tbl_get()`. The benefit of this is that we can use autocompletion to
show us what's available in the table store (i.e., appears after typing
the `$`).

    store$small_table_duck %>% tbl_get()

    ## # Source:   table<small_table> [?? x 8]
    ## # Database: duckdb_connection
    ##    date_time           date           a b             c      d e     f
    ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    ## # … with more rows

## Function ID

1-10

## See also

Other Planning and Prep:
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
[`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
