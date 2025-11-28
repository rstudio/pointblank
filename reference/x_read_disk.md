# Read an *agent*, *informant*, *multiagent*, or table scan from disk

An *agent*, *informant*, *multiagent*, or table scan that has been
written to disk (with
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md))
can be read back into memory with the `x_read_disk()` function. For an
*agent* or an *informant* object that has been generated in this way, it
may not have a data table associated with it (depending on whether the
`keep_tbl` option was `TRUE` or `FALSE` when writing to disk) but it
should still be able to produce reporting (by printing the *agent* or
*informant* to the console or using
[`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md)/[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)).
An *agent* will return an x-list with
[`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md)
and yield any available data extracts with
[`get_data_extracts()`](https://rstudio.github.io/pointblank/reference/get_data_extracts.md).
Furthermore, all of an *agent*'s validation steps will still be present
(along with results from the last interrogation).

## Usage

``` r
x_read_disk(filename, path = NULL, quiet = FALSE)
```

## Arguments

- filename:

  *File name*

  `scalar<character>` // **required**

  The name of a file that was previously written by
  [`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md).

- path:

  *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional path to the file (combined with `filename`).

- quiet:

  *Inform (or not) upon file writing*

  `scalar<logical>` // *default:* `FALSE`

  Should the function *not* inform when the file is written?

## Value

Either a `ptblank_agent`, `ptblank_informant`, or a `ptblank_tbl_scan`
object.

## Details

Should a written-to-disk *agent* or *informant* possess a table-prep
formula or a specific in-memory tablewe could use the
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
or
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
function again. For a *data quality reporting* workflow, it is useful to
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
target tables that evolve over time. While the same validation steps
will be used, more can be added before calling
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md).
For an *information management* workflow with an *informant* object,
using
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
will update aspects of the reporting such as table dimensions, and info
snippets/text will be regenerated.

## Examples

### A: Reading an agent from disk

The process of developing an agent and writing it to disk with the
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
function is explained in that function's documentation. Suppose we have
such a written file that's named `"agent-small_table.rds"`, we could
read that to a new agent object with `x_read_disk()`.

    agent <- x_read_disk("agent-small_table.rds")

### B: Reading an informant from disk

If there is an informant written to disk via
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
and it's named `"informant-small_table.rds"`. We could read that to a
new informant object with `x_read_disk()`.

    informant <- x_read_disk("informant-small_table.rds")

### C: Reading a multiagent from disk

The process of creating a multiagent and writing it to disk with the
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
function is shown in that function's documentation. Should we have such
a written file called `"multiagent-small_table.rds"`, we could read that
to a new multiagent object with `x_read_disk()`.

    multiagent <- x_read_disk("multiagent-small_table.rds")

### D: Reading a table scan from disk

If there is a table scan written to disk via
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
and it's named `"tbl_scan-storms.rds"`, we could read it back into R
with `x_read_disk()`.

    tbl_scan <- x_read_disk("tbl_scan-storms.rds")

## Function ID

9-2

## See also

Other Object Ops:
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md),
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md),
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md),
[`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md),
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md),
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
