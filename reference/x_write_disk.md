# Write an *agent*, *informant*, *multiagent*, or table scan to disk

Writing an *agent*, *informant*, *multiagent*, or even a table scan to
disk with `x_write_disk()` can be useful for keeping data validation
intel or table information close at hand for later retrieval (with
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)).
By default, any data table that the *agent* or *informant* may have held
before being committed to disk will be expunged (not applicable to any
table scan since they never hold a table object). This behavior can be
changed by setting `keep_tbl` to `TRUE` but this only works in the case
where the table is not of the `tbl_dbi` or the `tbl_spark` class.

## Usage

``` r
x_write_disk(
  x,
  filename,
  path = NULL,
  keep_tbl = FALSE,
  keep_extracts = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  *One of several types of objects*

  `<object>` // **required**

  An *agent* object of class `ptblank_agent`, an *informant* of class
  `ptblank_informant`, or an table scan of class `ptblank_tbl_scan`.

- filename:

  *File name*

  `scalar<character>` // **required**

  The filename to create on disk for the `agent`, `informant`, or table
  scan.

- path:

  *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional path to which the file should be saved (this is
  automatically combined with `filename`).

- keep_tbl:

  *Keep data table inside object*

  `scalar<logical>` // *default:* `FALSE`

  An option to keep a data table that is associated with the *agent* or
  *informant* (which is the case when the *agent*, for example, is
  created using `create_agent(tbl = <data table, ...)`). The default is
  `FALSE` where the data table is removed before writing to disk. For
  database tables of the class `tbl_dbi` and for Spark DataFrames
  (`tbl_spark`) the table is always removed (even if `keep_tbl` is set
  to `TRUE`).

- keep_extracts:

  *Keep data extracts inside object*

  `scalar<logical>` // *default:* `FALSE`

  An option to keep any collected extract data for failing rows. Only
  applies to *agent* objects. By default, this is `FALSE` (i.e., extract
  data is removed).

- quiet:

  *Inform (or not) upon file writing*

  `scalar<logical>` // *default:* `FALSE`

  Should the function *not* inform when the file is written?

## Value

Invisibly returns `TRUE` if the file has been written.

## Details

It is recommended to set up a table-prep formula so that the *agent* and
*informant* can access refreshed data after being read from disk through
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md).
This can be done initially with the `tbl` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)/[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
by passing in a table-prep formula or a function that can obtain the
target table when invoked. Alternatively, we can use the
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md)
with a similarly crafted `tbl` expression to ensure that an *agent* or
*informant* can retrieve a table at a later time.

## Examples

### A: Writing an `agent` to disk

Let's go through the process of (1) developing an agent with a
validation plan (to be used for the data quality analysis of the
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md)
dataset), (2) interrogating the agent with the
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
function, and (3) writing the agent and all its intel to a file.

Creating an `action_levels` object is a common workflow step when
creating a pointblank agent. We designate failure thresholds to the
`warn`, `stop`, and `notify` states using
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md).

    al <-
      action_levels(
        warn_at = 0.10,
        stop_at = 0.25,
        notify_at = 0.35
      )

Now, let's create a pointblank `agent` object and give it the `al`
object (which serves as a default for all validation steps which can be
overridden). The data will be referenced in the `tbl` argument with a
leading `~`.

    agent <-
      create_agent(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "`x_write_disk()`",
        actions = al
      )

Then, as with any `agent` object, we can add steps to the validation
plan by using as many validation functions as we want. After that, use
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md).

    agent <-
      agent %>%
      col_exists(columns = c(date, date_time)) %>%
      col_vals_regex(
        columns = b,
        regex = "[0-9]-[a-z]{3}-[0-9]{3}"
      ) %>%
      rows_distinct() %>%
      col_vals_gt(columns = d, value = 100) %>%
      col_vals_lte(columns = c, value = 5) %>%
      interrogate()

The `agent` can be written to a file with the `x_write_disk()` function.

    x_write_disk(
      agent,
      filename = "agent-small_table.rds"
    )

We can read the file back as an agent with the
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
function and we'll get all of the intel along with the restored agent.

If you're consistently writing agent reports when periodically checking
data, we could make use of the
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md)
or
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md)
depending on the granularity you need. Here's an example that writes the
file with the format: `"<filename>-YYYY-mm-dd_HH-MM-SS.rds"`.

    x_write_disk(
      agent,
      filename = affix_datetime(
        "agent-small_table.rds"
      )
    )

### B: Writing an `informant` to disk

Let's go through the process of (1) creating an informant object that
minimally describes the
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md)
dataset, (2) ensuring that data is captured from the target table using
the
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
function, and (3) writing the informant to a file.

Create a pointblank `informant` object with
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
and the
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md)
dataset. Use
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
so that info snippets are integrated into the text.

    informant <-
      create_informant(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "`x_write_disk()`"
      ) %>%
      info_snippet(
        snippet_name = "high_a",
        fn = snip_highest(column = "a")
      ) %>%
      info_snippet(
        snippet_name = "low_a",
        fn = snip_lowest(column = "a")
      ) %>%
      info_columns(
        columns = a,
        info = "From {low_a} to {high_a}."
      ) %>%
      info_columns(
        columns = starts_with("date"),
        info = "Time-based values."
      ) %>%
      info_columns(
        columns = date,
        info = "The date part of `date_time`."
      ) %>%
      incorporate()

The `informant` can be written to a file with `x_write_disk()`. Let's do
this with
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md)
so that the filename has a datestamp.

    x_write_disk(
      informant,
      filename = affix_date(
        "informant-small_table.rds"
      )
    )

We can read the file back into a new informant object (in the same state
as when it was saved) by using
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md).

### C: Writing a multiagent to disk

Let's create one more pointblank agent object, provide it with some
validation steps, and
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md).

    agent_b <-
      create_agent(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "`x_write_disk()`",
        actions = al
      ) %>%
      col_vals_gt(
        columns = b,
        value = g,
        na_pass = TRUE,
        label = "b > g"
      ) %>%
      col_is_character(
        columns = c(b, f),
        label = "Verifying character-type columns"
      ) %>%
      interrogate()

Now we can combine the earlier `agent` object with the newer `agent_b`
to create a `multiagent`.

    multiagent <- create_multiagent(agent, agent_b)

The `multiagent` can be written to a file with the `x_write_disk()`
function.

    x_write_disk(
      multiagent,
      filename = "multiagent-small_table.rds"
    )

We can read the file back as a multiagent with the
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
function and we'll get all of the constituent agents and their
associated intel back as well.

### D: Writing a table scan to disk

We can get a report that describes all of the data in the `storms`
dataset.

    tbl_scan <- scan_data(tbl = dplyr::storms)

The table scan object can be written to a file with `x_write_disk()`.

    x_write_disk(
      tbl_scan,
      filename = "tbl_scan-storms.rds"
    )

## Function ID

9-1

## See also

Other Object Ops:
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md),
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md),
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md),
[`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md),
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md),
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
