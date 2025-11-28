# Put the current date into a file name

This function helps to affix the current date to a filename. This is
useful when writing *agent* and/or *informant* objects to disk as part
of a continuous process. The date can be in terms of UTC time or the
local system time. The date can be affixed either to the end of the
filename (before the file extension) or at the beginning with a
customizable delimiter.

The
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md),
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
functions allow for the writing of **pointblank** objects to disk.
Furthermore the
[`log4r_step()`](https://rstudio.github.io/pointblank/reference/log4r_step.md)
function has the `append_to` argument that accepts filenames, and, it's
reasonable that a series of log files could be differentiated by a date
component in the naming scheme. The modification of the filename string
takes effect immediately but not at the time of writing a file to disk.
In most cases, especially when using `affix_date()` with the
aforementioned file-writing functions, the file timestamps should
approximate the time components affixed to the filenames.

## Usage

``` r
affix_date(
  filename,
  position = c("end", "start"),
  format = "%Y-%m-%d",
  delimiter = "_",
  utc_time = TRUE
)
```

## Arguments

- filename:

  The filename to modify.

- position:

  Where to place the formatted date. This could either be at the `"end"`
  of the filename (the default) or at the `"start"`.

- format:

  A [`base::strptime()`](https://rdrr.io/r/base/strptime.html) format
  string for formatting the date. By default, this is `"%Y-%m-%d"` which
  expresses the date according to the ISO 8601 standard (as
  `YYYY-MM-DD`). Refer to the documentation on
  [`base::strptime()`](https://rdrr.io/r/base/strptime.html) for
  conversion specifications if planning to use a different format
  string.

- delimiter:

  The delimiter characters to use for separating the date string from
  the original file name.

- utc_time:

  An option for whether to use the current UTC time to establish the
  date (the default, with `TRUE`), or, use the system's local time
  (`FALSE`).

## Value

A character vector.

## Examples

### The basics of creating a filename with the current date

Taking the generic `"pb_file"` name for a file, we add the current date
to it as a suffix.

    affix_date(filename = "pb_file")

    ## [1] "pb_file_2022-04-01"

File extensions won't get in the way:

    affix_date(filename = "pb_file.rds")

    ## [1] "pb_file_2022-04-01.rds"

The date can be used as a prefix.

    affix_date(
      filename = "pb_file",
      position = "start"
    )

    ## [1] "2022-04-01_pb_file"

The date pattern can be changed and so can the delimiter.

    affix_date(
      filename = "pb_file.yml",
      format = "%Y%m%d",
      delimiter = "-"
    )

    ## [1] "pb_file-20220401.yml"

### Using a date-based filename in a **pointblank** workflow

We can use a file-naming convention involving dates when writing output
files immediately after interrogating. This is just one example (any
workflow involving a `filename` argument is applicable). It's really
advantageous to use date-based filenames when interrogating directly
from YAML in a scheduled process.

    yaml_agent_interrogate(
      filename = system.file(
        "yaml", "agent-small_table.yml",
        package = "pointblank"
      )
    ) %>%
      x_write_disk(
        filename = affix_date(
          filename = "small_table_agent.rds",
          delimiter = "-"
        ),
        keep_tbl = TRUE,
        keep_extracts = TRUE
      )

In the above, we used the written-to-disk agent (The
`"agent-small_table.yml"` YAML file) for an interrogation via
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md).
Then, the results were written to disk as an RDS file. In the `filename`
argument of
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md),
the `affix_date()` function was used to ensure that a daily run would
produce a file whose name indicates the day of execution.

## Function ID

13-3

## See also

The
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md)
function provides the same features except it produces a datetime string
by default.

Other Utility and Helper Functions:
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md),
[`col_schema()`](https://rstudio.github.io/pointblank/reference/col_schema.md),
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md),
[`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md),
[`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)
