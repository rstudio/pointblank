# Create a **pointblank** *informant* object

The `create_informant()` function creates an *informant* object, which
is used in an *information management* workflow. The overall aim of this
workflow is to record, collect, and generate useful information on data
tables. We can supply any information that is useful for describing a
particular data table. The *informant* object created by the
`create_informant()` function takes information-focused functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
and
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md).

The `info_*()` series of functions allows for a progressive build up of
information about the target table. The
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
and
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md)
functions facilitate the entry of *info text* that concerns the table
columns and the table proper; the
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md)
function allows for the creation of arbitrary sections that can have
multiple subsections full of additional *info text*. The system allows
for dynamic values culled from the target table by way of
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
for getting named text extracts from queries, and the use of
`{<snippet_name>}` in the *info text*. To make the use of
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
more convenient for common queries, a set of `snip_*()` functions are
provided in the package
([`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md),
[`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md),
and
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md))
though you are free to use your own expressions.

Because snippets need to query the target table to return fragments of
*info text*, the
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
function needs to be used to initiate this action. This is also
necessary for the *informant* to update other metadata elements such as
row and column counts. Once the incorporation process is complete,
snippets and other metadata will be updated. Calling the *informant*
itself will result in a reporting table. This reporting can also be
accessed with the
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
function, where there are more reporting options.

## Usage

``` r
create_informant(
  tbl = NULL,
  tbl_name = NULL,
  label = NULL,
  agent = NULL,
  lang = NULL,
  locale = NULL,
  read_fn = NULL
)
```

## Arguments

- tbl:

  *Table or expression for reading in one*

  `obj:<tbl_*>|<tbl reading expression>` // **required**

  The input table. This can be a data frame, a tibble, a `tbl_dbi`
  object, or a `tbl_spark` object. Alternatively, an expression can be
  supplied to serve as instructions on how to retrieve the target table
  at incorporation-time. There are two ways to specify an association to
  a target table: (1) as a table-prep formula, which is a right-hand
  side (RHS) formula expression (e.g., `~ { <tbl reading code>}`),
  or (2) as a function (e.g., `function() { <tbl reading code>}`).

- tbl_name:

  *A table name*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A optional name to assign to the input table object. If no value is
  provided, a name will be generated based on whatever information is
  available.

- label:

  *An optional label for the information report*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional label for the information report. If no value is provided,
  a label will be generated based on the current system time. Markdown
  can be used here to make the label more visually appealing (it will
  appear in the header area of the information report).

- agent:

  *The pointblank agent object*

  `obj:<ptblank_agent>` // *default:* `NULL` (`optional`)

  A pointblank *agent* object. The table from this object can be
  extracted and used in the new informant instead of supplying a table
  in `tbl`.

- lang:

  *Reporting language*

  `scalar<character>` // *default:* `NULL` (`optional`)

  The language to use for the information report (a summary table that
  provides all of the available information for the table. By default,
  `NULL` will create English (`"en"`) text. Other options include French
  (`"fr"`), German (`"de"`), Italian (`"it"`), Spanish (`"es"`),
  Portuguese (`"pt"`), Turkish (`"tr"`), Chinese (`"zh"`), Russian
  (`"ru"`), Polish (`"pl"`), Danish (`"da"`), Swedish (`"sv"`), and
  Dutch (`"nl"`).

- locale:

  *Locale for value formatting within reports*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional locale ID to use for formatting values in the information
  report according the locale's rules. Examples include `"en_US"` for
  English (United States) and `"fr_FR"` for French (France); more
  simply, this can be a language identifier without a country
  designation, like "es" for Spanish (Spain, same as `"es_ES"`).

- read_fn:

  *[Deprecated](https://rdrr.io/r/base/Deprecated.html) Table reading
  function*

  `function` // *default:* `NULL` (`optional`)

  The `read_fn` argument is deprecated. Instead, supply a table-prep
  formula or function to `tbl`.

## Value

A `ptblank_informant` object.

## Supported Input Tables

The types of data tables that are officially supported are:

- data frames (`data.frame`) and tibbles (`tbl_df`)

- Spark DataFrames (`tbl_spark`)

- the following database tables (`tbl_dbi`):

  - *PostgreSQL* tables (using the
    [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)
    as driver)

  - *MySQL* tables (with
    [`RMySQL::MySQL()`](https://r-dbi.r-universe.dev/RMySQL/reference/MySQLDriver-class.html))

  - *Microsoft SQL Server* tables (via **odbc**)

  - *BigQuery* tables (using
    [`bigrquery::bigquery()`](https://bigrquery.r-dbi.org/reference/bigquery.html))

  - *DuckDB* tables (through
    [`duckdb::duckdb()`](https://r.duckdb.org/reference/duckdb.html))

  - *SQLite* (with
    [`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html))

Other database tables may work to varying degrees but they haven't been
formally tested (so be mindful of this when using unsupported backends
with **pointblank**).

## YAML

A **pointblank** informant can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used to regenerate an informant (with
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md))
or perform the 'incorporate' action using the target table (via
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md)).
Here is an example of how a complex call of `create_informant()` is
expressed in R code and in the corresponding YAML representation.

R statement:

    create_informant(
      tbl = ~ small_table,
      tbl_name = "small_table",
      label = "An example.",
      lang = "fr",
      locale = "fr_CA"
    )

YAML representation:

    type: informant
    tbl: ~small_table
    tbl_name: small_table
    info_label: An example.
    lang: fr
    locale: fr_CA
    table:
      name: small_table
      _columns: 8
      _rows: 13.0
      _type: tbl_df
    columns:
      date_time:
        _type: POSIXct, POSIXt
      date:
        _type: Date
      a:
        _type: integer
      b:
        _type: character
      c:
        _type: numeric
      d:
        _type: numeric
      e:
        _type: logical
      f:
        _type: character

The generated YAML includes some top-level keys where `type` and `tbl`
are mandatory, and, two metadata sections: `table` and `columns`. Keys
that begin with an underscore character are those that are updated
whenever
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
is called on an *informant*. The `table` metadata section can have
multiple subsections with *info text*. The `columns` metadata section
can similarly have have multiple subsections, so long as they are
children to each of the column keys (in the above YAML example,
`date_time` and `date` are column keys and they match the table's column
names). Additional sections can be added but they must have key names on
the top level that don't duplicate the default set (i.e., `type`,
`table`, `columns`, etc. are treated as reserved keys).

## Writing an Informant to Disk

An *informant* object can be written to disk with the
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
function. Informants are stored in the serialized RDS format and can be
easily retrieved with the
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
function.

It's recommended that table-prep formulas are supplied to the `tbl`
argument of `create_informant()`. In this way, when an *informant* is
read from disk through
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md),
it can be reused to access the target table (which may changed, hence
the need to use an expression for this).

## Examples

Let's walk through how we can generate some useful information for a
really small table. It's actually called `small_table` and we can find
it as a dataset in this package.

    small_table
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

Create a pointblank `informant` object with `create_informant()` and the
`small_table` dataset.

    informant <-
      create_informant(
        tbl = pointblank::small_table,
        tbl_name = "small_table",
        label = "`create_informant()` example."
      )

This function creates some information without any extra help by
profiling the supplied table object. It adds the `COLUMNS` section with
stubs for each of the target table's columns. We can use the
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
or
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md)
to provide descriptions for each of the columns. The `informant` object
can be printed to see the information report in the Viewer.

    informant

![This image was generated from the first code example in the
\`create_informant()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_create_informant_1.png)

If we want to make use of more report display options, we can
alternatively use the
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
function.

    report <-
      get_informant_report(
        informant,
        title = "Data Dictionary for `small_table`"
      )

    report

![This image was generated from the second code example in the
\`create_informant()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_create_informant_2.png)

## Function ID

1-3

## See also

Other Planning and Prep:
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
