# Obtain a table-prep formula from a table store

The `tbl_source()` function provides a convenient means to access a
table-prep formula from either a `tbl_store` object or a table store
YAML file (which can be created with the
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
function). A call to `tbl_source()` is most useful as an input to the
`tbl` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
or
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md).

Should you need to obtain the table itself (that is generated via the
table-prep formula), then the
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md)
function should be used for that.

## Usage

``` r
tbl_source(tbl, store = NULL)
```

## Arguments

- tbl:

  The table name associated with a table-prep formula. This is part of
  the table `store`. This table could be identified by its name (e.g.,
  `tbl = "large_table"`) or by supplying a reference using a subset
  (with `$`) of the `tbl_store` object (e.g.,
  `tbl = store$large_table`). If using the latter method then nothing
  needs to be supplied to `store`.

- store:

  Either a table store object created by the
  [`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
  function or a path to a table store YAML file created by
  [`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md).

## Value

A table-prep formula.

## Examples

Let's create a `tbl_store` object by giving two table-prep formulas to
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md).

    store <-
      tbl_store(
        small_table_duck ~ db_tbl(
          table = small_table,
          dbname = ":memory:",
          dbtype = "duckdb"
        ),
        sml_table ~ pointblank::small_table
      )

We can pass a table-prep formula to
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
via `tbl_source()`, add some validation steps, and interrogate the table
shortly thereafter.

    agent_1 <-
      create_agent(
        tbl = ~ tbl_source("sml_table", store),
        label = "`tbl_source()` example",
        actions = action_levels(warn_at = 0.10)
      ) %>%
      col_exists(columns = c(date, date_time)) %>%
      interrogate()

The `agent_1` object can be printed to see the validation report in the
Viewer.

    agent_1

![This image was generated from the first code example in the
\`tbl_source()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_tbl_source_1.png)

The `tbl_store` object can be transformed to YAML with the
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
function. The following statement writes the `tbl_store.yml` file by
default (but a different name could be used with the `filename`
argument):

    yaml_write(store)

Let's modify the agent's target to point to the table labeled as
`"sml_table"` in the YAML representation of the `tbl_store`.

    agent_2 <-
      agent_1 %>%
      set_tbl(
        ~ tbl_source(
            tbl = "sml_table",
            store = "tbl_store.yml"
          )
      )

We can likewise write the agent to a YAML file with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
(writes to `agent-sml_table.yml` by default but the `filename` allows
for any filename you want).

    yaml_write(agent_2)

Now that both the agent and the associated table store are present as
on-disk YAML, interrogations can be done by using
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md).

    agent <- yaml_agent_interrogate(filename = "agent-sml_table.yml")

## Function ID

1-9

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
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
