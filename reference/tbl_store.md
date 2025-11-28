# Define a store of tables with table-prep formulas: a table store

It can be useful to set up all the data sources you need and just draw
from them when necessary. This upfront configuration with `tbl_store()`
lets us define the methods for obtaining tabular data from mixed sources
(e.g., database tables, tables generated from flat files, etc.) and
provide identifiers for these data preparation procedures.

What results from this work is a convenient way to materialize tables
with
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md).
We can also get any table-prep formula from the table store with
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md).
The content of a table-prep formulas can involve reading a table from a
location, or, it can involve data transformation. One can imagine
scenarios where we might (1) procure several mutated variations of the
same source table, (2) generate a table using disparate data sources, or
(3) filter the rows of a database table according to the system time.
Another nice aspect of organizing table-prep formulas in a single object
is supplying it to the `tbl` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
or
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
via `$` notation (e.g, `create_agent(tbl = <tbl_store>$<name>)`) or with
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
(e.g., `create_agent(tbl = ~ tbl_source("<name>", <tbl_store>))`).

## Usage

``` r
tbl_store(..., .list = list2(...), .init = NULL)
```

## Arguments

- ...:

  Expressions that contain table-prep formulas and table names for data
  retrieval. Two-sided formulas (e.g, `<LHS> ~ <RHS>`) are to be used,
  where the left-hand side is an identifier and the right-hand contains
  a statement that obtains a table (i.e., the table-prep formula). If
  the LHS is omitted then an identifier will be generated for you.

- .list:

  Allows for the use of a list as an input alternative to `...`.

- .init:

  We can optionally provide an initialization statement (in a one-sided
  formula) that should be executed whenever *any* of tables in the table
  store are obtained. This is useful, for instance, for including a
  [`library()`](https://rdrr.io/r/base/library.html) call that can be
  executed before any table-prep formulas in `...`.

## Value

A `tbl_store` object that contains table-prep formulas.

## YAML

A **pointblank** table store can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used in several ways. The ideal scenario
is to have pointblank agents and informants also in YAML form. This way
the agent and informant can refer to the table store YAML (via
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)),
and, the processing of both agents and informants can be performed with
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)
and
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md).
With the following R code, a table store with two table-prep formulas is
generated and written to YAML (if no filename is given then the YAML is
written to `"tbl_store.yml"`).

R statement for generating the `"tbl_store.yml"` file:

    tbl_store(
      tbl_duckdb ~ db_tbl(small_table, dbname = ":memory:", dbtype = "duckdb"),
      sml_table_high ~ small_table %>% dplyr::filter(f == "high"),
      .init = ~ library(tidyverse)
    ) %>%
      yaml_write()

YAML representation (`"tbl_store.yml"`):

    type: tbl_store
    tbls:
      tbl_duckdb: ~ db_tbl(small_table, dbname = ":memory:", dbtype = "duckdb")
      sml_table_high: ~ small_table %>% dplyr::filter(f == "high")
    init: ~library(tidyverse)

This is useful when you want to get fresh pulls of prepared data from a
source materialized in an R session (with the
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md)
function. For example, the `sml_table_high` table can be obtained by
using `tbl_get("sml_table_high", "tbl_store.yml")`. To get an agent to
check this prepared data periodically, then the following example with
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
will be useful:

R code to generate agent that checks `sml_table_high` and writing the
agent to YAML:

    create_agent(
      tbl = ~ tbl_source("sml_table_high", "tbl_store.yml"),
      label = "An example that uses a table store.",
      actions = action_levels(warn_at = 0.10)
    ) %>%
      col_exists(c(date, date_time)) %>%
      write_yaml()

The YAML representation (`"agent-sml_table_high.yml"`):

    tbl: ~ tbl_source("sml_table_high", "tbl_store.yml")
    tbl_name: sml_table_high
    label: An example that uses a table store.
    actions:
      warn_fraction: 0.1
    locale: en
    steps:
      - col_exists:
        columns: c(date, date_time)

Now, whenever the `sml_table_high` table needs to be validated, it can
be done with
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)
(e.g., `yaml_agent_interrogate("agent-sml_table_high.yml")`).

## Examples

### Creating an in-memory table store and adding table-prep formulas

The table store provides a way to get the tables we need fairly easily.
Think of an identifier for the table you'd like and then provide the
code necessary to obtain that table. Then repeat as many times as you
like!

Here we'll define two tables that can be materialized later:
`tbl_duckdb` (an in-memory DuckDB database table with **pointblank**'s
`small_table` dataset) and `sml_table_high` (a filtered version of
`tbl_duckdb`):

    store_1 <-
      tbl_store(
        tbl_duckdb ~
          db_tbl(
            pointblank::small_table,
            dbname = ":memory:",
            dbtype = "duckdb"
          ),
        sml_table_high ~
          db_tbl(
            pointblank::small_table,
            dbname = ":memory:",
            dbtype = "duckdb"
          ) %>%
          dplyr::filter(f == "high")
      )

We can see what's in the table store `store_1` by printing it out:

    store_1

    ## -- The `table_store` table-prep formulas
    ## 1 tbl_duckdb // ~ db_tbl(pointblank::small_table, dbname = ":memory:",
    ## dbtype = "duckdb")
    ## 2 sml_table_high // ~ db_tbl(pointblank::small_table, dbname = ":memory:",
    ## dbtype = "duckdb") %>% dplyr::filter(f == "high")
    ## ----

It's good to check that the tables can be obtained without error. We can
do this with the
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md)
function. With that function, we need to supply the given name of the
table-prep formula (in quotes) and the table store object.

    tbl_get(tbl = "tbl_duckdb", store = store_1)

    ## # Source:   table<pointblank::small_table> [?? x 8]
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

    tbl_get(tbl = "sml_table_high", store = store_1)

    ## # Source:   lazy query [?? x 8]
    ## # Database: duckdb_connection
    ##   date_time           date           a b             c     d e     f
    ##   <dttm>              <date>     <int> <chr>     <dbl> <dbl> <lgl> <chr>
    ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE  high
    ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE  high
    ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high
    ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high

We can shorten the `tbl_store()` statement with some syntax that
**pointblank** provides. The `sml_table_high` table-prep is simply a
transformation of `tbl_duckdb`, so, we can use `{{ tbl_duckdb }}` in
place of the repeated statement. Additionally, we can provide a
[`library()`](https://rdrr.io/r/base/library.html) call to the `.init`
argument of `tbl_store()` so that **dplyr** is available (thus allowing
us to use `filter(...)` instead of `dplyr::filter(...)`). Here is the
revised `tbl_store()` call:

    store_2 <-
      tbl_store(
        tbl_duckdb ~
          db_tbl(
            pointblank::small_table,
            dbname = ":memory:",
            dbtype = "duckdb"
          ),
        sml_table_high ~
          {{ tbl_duckdb }} %>%
          filter(f == "high"),
        .init = ~ library(tidyverse)
      )

Printing the table store `store_2` now shows that we used an `.init`
statement:

    store_2

    ## -- The `table_store` table-prep formulas
    ## 1 tbl_duckdb // ~ db_tbl(pointblank::small_table, dbname = ":memory:",
    ## dbtype = "duckdb")
    ## 2 sml_table_high // ~ {{tbl_duckdb}} %>% filter(f == "high")
    ## ----
    ## INIT // ~library(tidyverse)
    ## ----

Checking again with
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md)
should provide the same tables as before:

    tbl_get(tbl = "tbl_duckdb", store = store_2)

    ## # Source:   table<pointblank::small_table> [?? x 8]
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

    tbl_get(tbl = "sml_table_high", store = store_2)

    ## # Source:   lazy query [?? x 8]
    ## # Database: duckdb_connection
    ##   date_time           date           a b             c     d e     f
    ##   <dttm>              <date>     <int> <chr>     <dbl> <dbl> <lgl> <chr>
    ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE  high
    ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE  high
    ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high
    ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high

### Using a table store in a data validation workflow

Define a `tbl_store` object by adding table-prep formulas inside the
`tbl_store()` call.

    store_3 <-
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
        all_revenue ~ db_tbl(
          table = file_tbl(
            file = from_github(
              file = "sj_all_revenue_large.rds",
              repo = "rich-iannone/intendo",
              subdir = "data-large"
            )
          ),
          dbname = ":memory:",
          dbtype = "duckdb"
        ),
        sml_table ~ pointblank::small_table
      )

Let's get a summary of what's in the table store `store_3` through
printing:

    store_3

    ## -- The `table_store` table-prep formulas
    ## 1 small_table_duck // ~ db_tbl(table = small_table, dbname = ":memory:",
    ## dbtype = "duckdb")
    ## 2 rna // ~db_tbl(table = "rna", dbname = "pfmegrnargs", dbtype =
    ## "postgres", host = "hh-pgsql-public.ebi.ac.uk", port = 5432, user =
    ## I("reader"), password = I("NWDMCE5xdipIjRrp"))
    ## 3 all_revenue // ~ db_tbl(table = file_tbl(file = from_github(file =
    ## "sj_all_revenue_large.rds", repo = "rich-iannone/intendo", subdir =
    ## "data-large")), dbname = ":memory:", dbtype = "duckdb")
    ## 4 sml_table // ~ pointblank::small_table
    ## ----

Once this object is available, you can check that the table of interest
is produced to your specification with the
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md)
function.

    tbl_get(
      tbl = "small_table_duck",
      store = store_3
    )

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

Another way to get the same table materialized is by using `$` to get
the entry of choice for
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md).

    store_3$small_table_duck %>% tbl_get()

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

Creating an agent is easy when all table-prep formulas are encapsulated
in a `tbl_store` object. Use `$` notation to pass the appropriate
procedure for reading a table to the `tbl` argument.

    agent_1 <-
      create_agent(
        tbl = store_3$small_table_duck
      )

There are other ways to use the table store to assign a target table to
an agent, like using the
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
function (which extracts the table-prep formula from the table store).

    agent_2 <-
      create_agent(
        tbl = ~ tbl_source(
          tbl = "small_table_duck",
          store = store_3
          )
      )

### Writing a table store to a YAML file

The table store can be moved to YAML with `yaml_write` and the
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
call could then refer to that on-disk table store. Let's do that YAML
conversion.

    yaml_write(store_3)

The above writes the `tbl_store.yml` file (by not providing a `filename`
this default filename is chosen).

It can be convenient to read table-prep formulas from a YAML file that's
a table store. To achieve this, we can modify the
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
statement in the
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
call so that `store` refers to the on-disk YAML file.

    agent_3 <-
      create_agent(
        tbl = ~ tbl_source(
          tbl = "small_table_duck",
          store = "tbl_store.yml"
        )
      )

## Function ID

1-8

## See also

Other Planning and Prep:
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
[`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
