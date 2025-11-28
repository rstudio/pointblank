# Get a table from a database

If your target table is in a database, the `db_tbl()` function is a
handy way of accessing it. This function simplifies the process of
getting a `tbl_dbi` object, which usually involves a combination of
building a connection to a database and using the
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)
function with the connection and the table name (or a reference to a
table in a schema). You can use `db_tbl()` as the basis for obtaining a
database table for the `tbl` parameter in
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
or
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md).
Another great option is supplying a table-prep formula involving
`db_tbl()` to
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
so that you have access to database tables though single names via a
table store.

The username and password are supplied through environment variable
names. If desired, values for the username and password can be supplied
directly by enclosing such values in
[`I()`](https://rdrr.io/r/base/AsIs.html).

## Usage

``` r
db_tbl(
  table,
  dbtype,
  dbname = NULL,
  host = NULL,
  port = NULL,
  user = NULL,
  password = NULL,
  bq_project = NULL,
  bq_dataset = NULL,
  bq_billing = bq_project
)
```

## Arguments

- table:

  The name of the table, or, a reference to a table in a schema
  (two-element vector with the names of schema and table).
  Alternatively, this can be supplied as a data table to copy into an
  in-memory database connection. This only works if: (1) the `db` is
  chosen as either `"sqlite"` or `"duckdb"`, (2) the `dbname` was is set
  to `":memory:"`, and (3) the object supplied to `table` is a data
  frame or a tibble object.

- dbtype:

  Either an appropriate driver function (e.g.,
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html))
  or a shortname for the database type. Valid names are: `"postgresql"`,
  `"postgres"`, or `"pgsql"` (PostgreSQL, using the
  [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)
  driver function); `"mysql"` (MySQL, using
  [`RMySQL::MySQL()`](https://r-dbi.r-universe.dev/RMySQL/reference/MySQLDriver-class.html));
  `bigquery` or `bq` (BigQuery, using
  [`bigrquery::bigquery()`](https://bigrquery.r-dbi.org/reference/bigquery.html));
  `"duckdb"` (DuckDB, using
  [`duckdb::duckdb()`](https://r.duckdb.org/reference/duckdb.html)); and
  `"sqlite"` (SQLite, using
  [`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html)).

- dbname:

  The database name.

- host, port:

  The database host and optional port number.

- user, password:

  The environment variables used to access the username and password for
  the database. Enclose in [`I()`](https://rdrr.io/r/base/AsIs.html)
  when using literal username or password values.

- bq_project, bq_dataset, bq_billing:

  If accessing a table from a *BigQuery* data source, there's the
  requirement to provide the table's associated project (`bq_project`)
  and dataset (`bq_dataset`) names. By default, the project to be billed
  will be the same as the one provided for `bq_project` but the
  `bq_billing` argument can be changed to reflect a different BigQuery
  project.

## Value

A `tbl_dbi` object.

## Examples

### Obtaining in-memory database tables

You can use an in-memory database table and by supplying it with an
in-memory table. This works with the DuckDB database and the key thing
is to use `dbname = ":memory"` in the `db_tbl()` call.

    small_table_duckdb <-
      db_tbl(
        table = small_table,
        dbtype = "duckdb",
        dbname = ":memory:"
      )

    small_table_duckdb

    ## # Source:   table<small_table> [?? x 8]
    ## # Database: duckdb_connection
    ##    date_time           date           a b         c      d e     f
    ##    <dttm>              <date>     <int> <chr> <dbl>  <dbl> <lgl> <chr>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bc…     3  3423. TRUE  high
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-eg…     8 10000. TRUE  low
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kd…     3  2343. TRUE  high
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jd…    NA  3892. FALSE mid
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ld…     7   284. TRUE  low
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dh…     4  3291. TRUE  mid
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-kn…     3   843. TRUE  high
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-bo…     2  1036. FALSE low
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bc…     9   838. FALSE high
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bc…     9   838. FALSE high
    ## # … with more rows

The in-memory option also works using the SQLite database. The only
change required is setting the `dbtype` to `"sqlite"`:

    small_table_sqlite <-
      db_tbl(
        table = small_table,
        dbtype = "sqlite",
        dbname = ":memory:"
      )

    small_table_sqlite

    ## # Source:   table<small_table> [?? x 8]
    ## # Database: sqlite 3.37.0 [:memory:]
    ##     date_time  date     a b             c      d     e f
    ##         <dbl> <dbl> <int> <chr>     <dbl>  <dbl> <int> <chr>
    ##  1 1451905200 16804     2 1-bcd-345     3  3423.     1 high
    ##  2 1451867520 16804     3 5-egh-163     8 10000.     1 low
    ##  3 1452000720 16805     6 8-kdg-938     3  2343.     1 high
    ##  4 1452100980 16806     2 5-jdo-903    NA  3892.     0 mid
    ##  5 1452342960 16809     8 3-ldm-038     7   284.     1 low
    ##  6 1452492900 16811     4 2-dhe-923     4  3291.     1 mid
    ##  7 1452883560 16815     7 1-knw-093     3   843.     1 high
    ##  8 1453030020 16817     4 5-boe-639     2  1036.     0 low
    ##  9 1453264200 16820     3 5-bce-642     9   838.     0 high
    ## 10 1453264200 16820     3 5-bce-642     9   838.     0 high
    ## # … with more rows

It's also possible to obtain a table from a remote file and shove it
into an in-memory database. For this, we can use the all-powerful
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md) +
`db_tbl()` combo.

    all_revenue_large_duckdb <-
      db_tbl(
        table = file_tbl(
          file = from_github(
            file = "sj_all_revenue_large.rds",
            repo = "rich-iannone/intendo",
            subdir = "data-large"
          )
        ),
        dbtype = "duckdb",
        dbname = ":memory:"
      )

    all_revenue_large_duckdb

    ## # Source:   table<sj_all_revenue_large.rds> [?? x 11]
    ## # Database: duckdb_connection
    ##    player_id       session_id   session_start       time
    ##    <chr>           <chr>        <dttm>              <dttm>
    ##  1 IRZKSAOYUJME796 IRZKSAOYUJM… 2015-01-01 00:18:41 2015-01-01 00:18:53
    ##  2 CJVYRASDZTXO674 CJVYRASDZTX… 2015-01-01 01:13:01 2015-01-01 01:13:07
    ##  3 CJVYRASDZTXO674 CJVYRASDZTX… 2015-01-01 01:13:01 2015-01-01 01:23:37
    ##  4 CJVYRASDZTXO674 CJVYRASDZTX… 2015-01-01 01:13:01 2015-01-01 01:24:37
    ##  5 CJVYRASDZTXO674 CJVYRASDZTX… 2015-01-01 01:13:01 2015-01-01 01:31:01
    ##  6 CJVYRASDZTXO674 CJVYRASDZTX… 2015-01-01 01:13:01 2015-01-01 01:31:43
    ##  7 CJVYRASDZTXO674 CJVYRASDZTX… 2015-01-01 01:13:01 2015-01-01 01:36:01
    ##  8 ECPANOIXLZHF896 ECPANOIXLZH… 2015-01-01 01:31:03 2015-01-01 01:31:27
    ##  9 ECPANOIXLZHF896 ECPANOIXLZH… 2015-01-01 01:31:03 2015-01-01 01:36:57
    ## 10 ECPANOIXLZHF896 ECPANOIXLZH… 2015-01-01 01:31:03 2015-01-01 01:37:45
    ## # … with more rows, and 7 more variables: item_type <chr>,
    ## #   item_name <chr>, item_revenue <dbl>, session_duration <dbl>,
    ## #   start_day <date>, acquisition <chr>, country <chr>

And that's really it.

### Obtaining remote database tables

For remote databases, we have to specify quite a few things but it's a
one-step process nonetheless. Here's an example that accesses the `rna`
table (in the *RNA Central* public database) using `db_tbl()`. Here, for
the `user` and `password` entries we are using the literal username and
password values (publicly available when visiting the *RNA Central*
website) by enclosing the values in
[`I()`](https://rdrr.io/r/base/AsIs.html).

    rna_db_tbl <-
      db_tbl(
        table = "rna",
        dbtype = "postgres",
        dbname = "pfmegrnargs",
        host = "hh-pgsql-public.ebi.ac.uk",
        port = 5432,
        user = I("reader"),
        password = I("NWDMCE5xdipIjRrp")
      )

    rna_db_tbl

    ## # Source:   table<rna> [?? x 9]
    ## # Database: postgres
    ## #   [reader@hh-pgsql-public.ebi.ac.uk:5432/pfmegrnargs]
    ##          id upi    timestamp           userstamp crc64   len seq_short
    ##     <int64> <chr>  <dttm>              <chr>     <chr> <int> <chr>
    ##  1 25222431 URS00… 2019-12-02 13:26:46 rnacen    E65C…   521 AGAGTTTG…
    ##  2 25222432 URS00… 2019-12-02 13:26:46 rnacen    6B91…   520 AGAGTTCG…
    ##  3 25222433 URS00… 2019-12-02 13:26:46 rnacen    03B8…   257 TACGTAGG…
    ##  4 25222434 URS00… 2019-12-02 13:26:46 rnacen    E925…   533 AGGGTTTG…
    ##  5 25222435 URS00… 2019-12-02 13:26:46 rnacen    C2D0…   504 GACGAACG…
    ##  6 25222436 URS00… 2019-12-02 13:26:46 rnacen    9EF6…   253 TACAGAGG…
    ##  7 25222437 URS00… 2019-12-02 13:26:46 rnacen    685A…   175 GAGGCAGC…
    ##  8 25222438 URS00… 2019-12-02 13:26:46 rnacen    4228…   556 AAAACATC…
    ##  9 25222439 URS00… 2019-12-02 13:26:46 rnacen    B7CC…   515 AGGGTTCG…
    ## 10 25222440 URS00… 2019-12-02 13:26:46 rnacen    038B…   406 ATTGAACG…
    ## # … with more rows, and 2 more variables: seq_long <chr>, md5 <chr>

You'd normally want to use the names of environment variables (envvars)
to more securely access the appropriate username and password values
when connecting to a DB. Here are all the necessary inputs:

    example_db_tbl <-
      db_tbl(
        table = "<table_name>",
        dbtype = "<database_type_shortname>",
        dbname = "<database_name>",
        host = "<connection_url>",
        port = "<connection_port>",
        user = "<DB_USER_NAME>",
        password = "<DB_PASSWORD>"
      )

Environment variables can be created by editing the user `.Renviron`
file and the `usethis::edit_r_environ()` function makes this pretty easy
to do.

### DB table access and prep via the table store

Using table-prep formulas in a centralized table store can make it
easier to work with DB tables in **pointblank**. Here's how to generate
a table store with two named entries for table preparations involving
the
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
and `db_tbl()` functions.

    store <-
      tbl_store(
        small_table_duck ~ db_tbl(
          table = pointblank::small_table,
          dbtype = "duckdb",
          dbname = ":memory:"
        ),
        small_high_duck ~ {{ small_table_duck }} %>%
          dplyr::filter(f == "high")
      )

Now it's easy to obtain either of these tables via
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md).
We can reference the table in the store by its name (given to the left
of the `~`).

    tbl_get(tbl = "small_table_duck", store = store)

    ## # Source:   table<pointblank::small_table> [?? x 8]
    ## # Database: duckdb_connection
    ##    date_time           date           a b           c      d e
    ##    <dttm>              <date>     <int> <chr>   <dbl>  <dbl> <lgl>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE
    ## # … with more rows, and 1 more variable: f <chr>

The second table in the table store is a mutated version of the first.
It's just as easily obtainable via
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md):

    tbl_get(tbl = "small_high_duck", store = store)

    ## # Source:   lazy query [?? x 8]
    ## # Database: duckdb_connection
    ##   date_time           date           a b             c     d e
    ##   <dttm>              <date>     <int> <chr>     <dbl> <dbl> <lgl>
    ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE
    ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE
    ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE
    ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE
    ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE
    ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE
    ## # … with more rows, and 1 more variable: f <chr>

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
          tbl = "small_table_duck",
          store = tbls
        )
      )

    informant <-
      create_informant(
        tbl = ~ tbl_source(
          tbl = "small_high_duck",
          store = tbls
        )
      )

## Function ID

1-6

## See also

Other Planning and Prep:
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
