# Export an *agent*, *informant*, *multiagent*, or table scan to HTML

The *agent*, *informant*, *multiagent*, and the table scan object can be
easily written as HTML with `export_report()`. Furthermore, any report
objects from the *agent*, *informant*, and *multiagent* (generated using
[`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md),
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md),
and
[`get_multiagent_report()`](https://rstudio.github.io/pointblank/reference/get_multiagent_report.md))
can be provided here for HTML export. Each HTML document written to disk
is self-contained and easily viewable in a web browser.

## Usage

``` r
export_report(x, filename, path = NULL, quiet = FALSE)
```

## Arguments

- x:

  *One of several types of objects*

  `<object>` // **required**

  An *agent* object of class `ptblank_agent`, an *informant* of class
  `ptblank_informant`, a *multiagent* of class `ptblank_multiagent`, a
  table scan of class `ptblank_tbl_scan`, or, customized reporting
  objects (`ptblank_agent_report`, `ptblank_informant_report`,
  `ptblank_multiagent_report.wide`, `ptblank_multiagent_report.long`).

- filename:

  *File name*

  `scalar<character>` // **required**

  The filename to create on disk for the HTML export of the object
  provided. It's recommended that the extension `".html"` is included.

- path:

  *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional path to which the file should be saved (this is
  automatically combined with `filename`).

- quiet:

  *Inform (or not) upon file writing*

  `scalar<logical>` // *default:* `FALSE`

  Should the function *not* inform when the file is written?

## Value

Invisibly returns `TRUE` if the file has been written.

## Examples

### A: Writing an agent report as HTML

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

Now create a pointblank `agent` object and give it the `al` object
(which serves as a default for all validation steps which can be
overridden). The data will be referenced in the `tbl` argument with a
leading `~`.

    agent <-
      create_agent(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "`export_report()`",
        actions = al
      )

As with any agent object, we can add steps to the validation plan by
using as many validation functions as we want. Then, we
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

The agent report can be written to an HTML file with `export_report()`.

    export_report(
      agent,
      filename = "agent-small_table.html"
    )

If you're consistently writing agent reports when periodically checking
data, we could make use of
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md)
or
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md)
depending on the granularity you need. Here's an example that writes the
file with the format: `"<filename>-YYYY-mm-dd_HH-MM-SS.html"`.

    export_report(
      agent,
      filename = affix_datetime(
        "agent-small_table.html"
      )
    )

### B: Writing an informant report as HTML

Let's go through the process of (1) creating an informant object that
minimally describes the
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md)
dataset, (2) ensuring that data is captured from the target table using
the
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
function, and (3) writing the informant report to HTML.

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
        label = "`export_report()`"
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

The informant report can be written to an HTML file with
`export_report()`. Let's do this with
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md)
so the filename has a datestamp.

    export_report(
      informant,
      filename = affix_date(
        "informant-small_table.html"
      )
    )

### C: Writing a table scan as HTML

We can get a report that describes all of the data in the `storms`
dataset.

    tbl_scan <- scan_data(tbl = dplyr::storms)

The table scan object can be written to an HTML file with
`export_report()`.

    export_report(
      tbl_scan,
      filename = "tbl_scan-storms.html"
    )

## Function ID

9-3

## See also

Other Object Ops:
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md),
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md),
[`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md),
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md),
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md),
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
