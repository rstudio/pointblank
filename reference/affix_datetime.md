# Put the current datetime into a file name

This function helps to affix the current datetime to a filename. This is
useful when writing *agent* and/or *informant* objects to disk as part
of a continuous process. The datetime string can be based on the current
UTC time or the local system time. The datetime can be affixed either to
the end of the filename (before the file extension) or at the beginning
with a customizable delimiter. Optionally, the time zone information can
be included. If the datetime is based on the local system time, the user
system time zone is shown with the format `<time>(+/-)hhmm`. If using
UTC time, then the `<time>Z` format is adopted.

The
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md),
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
functions allow for the writing of **pointblank** objects to disk. The
modification of the filename string takes effect immediately but not at
the time of writing a file to disk. In most cases, especially when using
`affix_datetime()` with the aforementioned file-writing functions, the
file timestamps should approximate the time components affixed to the
filenames.

## Usage

``` r
affix_datetime(
  filename,
  position = c("end", "start"),
  format = "%Y-%m-%d_%H-%M-%S",
  delimiter = "_",
  utc_time = TRUE,
  add_tz = FALSE
)
```

## Arguments

- filename:

  The filename to modify.

- position:

  Where to place the formatted datetime. This could either be at the
  `"end"` of the filename (the default) or at the `"start"`.

- format:

  A [`base::strptime()`](https://rdrr.io/r/base/strptime.html) format
  string for formatting the datetime. By default, this is
  `"%Y-%m-%dT%H:%M:%S"` which expresses the date according to the ISO
  8601 standard. For example, if the current datetime is
  `2020-12-04 13:11:23`, the formatted string would become
  `"2020-12-04T13:11:23"`. Refer to the documentation on
  [`base::strptime()`](https://rdrr.io/r/base/strptime.html) for
  conversion specifications if planning to use a different format
  string.

- delimiter:

  The delimiter characters to use for separating the datetime string
  from the original file name.

- utc_time:

  An option for whether to use the current UTC time to establish the
  datetime (the default, with `TRUE`), or, use the system's local time
  (`FALSE`).

- add_tz:

  Should the time zone (as an offset from UTC) be provided? If `TRUE`
  then the UTC offset will be either provided as `<time>Z` (if
  `utc_time = TRUE`) or `<time>(+/-)hhmm`. By default, this is `FALSE`.

## Value

A character vector.

## Examples

### The basics of creating a filename with the current date and time

Taking the generic `"pb_file"` name for a file, we add the current
datetime to it as a suffix.

    affix_datetime(filename = "pb_file")

    ## [1] "pb_file_2022-04-01_00-32-53"

File extensions won't get in the way:

    affix_datetime(filename = "pb_file.rds")

    ## [1] "pb_file_2022-04-01_00-32-53.rds"

The datetime can be used as a prefix.

    affix_datetime(
      filename = "pb_file",
      position = "start"
    )

    ## [1] "2022-04-01_00-32-53_pb_file"

The datetime pattern can be changed and so can the delimiter.

    affix_datetime(
      filename = "pb_file.yml",
      format = "%Y%m%d_%H%M%S",
      delimiter = "-"
    )

    ## [1] "pb_file-20220401_003253.yml"

Time zone information can be included. By default, all datetimes are
given in the UTC time zone.

    affix_datetime(
      filename = "pb_file.yml",
      add_tz = TRUE
    )

    ## [1] "pb_file_2022-04-01_00-32-53Z.yml"

We can use the system's local time zone with `utc_time = FALSE`.

    affix_datetime(
      filename = "pb_file.yml",
      utc_time = FALSE,
      add_tz = TRUE
    )

    ## [1] "pb_file_2022-03-31_20-32-53-0400.yml"

### Using a datetime-based filename in a **pointblank** workflow

We can use a file-naming convention involving datetimes when writing
output files immediately after interrogating. This is just one example
(any workflow involving a `filename` argument is applicable). It's
really advantageous to use datetime-based filenames when interrogating
directly from YAML in a scheduled process, especially if multiple
validation runs per day are being executed on the same target table.

    yaml_agent_interrogate(
      filename = system.file(
        "yaml", "agent-small_table.yml",
        package = "pointblank"
      )
    ) %>%
      x_write_disk(
        filename = affix_datetime(
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
the `affix_datetime()` function was used to ensure that frequent runs
would produce files whose names indicate the day and time of execution.

## Function ID

13-4

## See also

The
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md)
function provides the same features except it produces a date string by
default.

Other Utility and Helper Functions:
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md),
[`col_schema()`](https://rstudio.github.io/pointblank/reference/col_schema.md),
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md),
[`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md),
[`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)
