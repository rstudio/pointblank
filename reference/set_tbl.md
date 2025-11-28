# Set a data table to an *agent* or an *informant*

Setting a data table to an *agent* or an *informant* with `set_tbl()`
replaces any associated table (a data frame, a tibble, objects of class
`tbl_dbi` or `tbl_spark`).

## Usage

``` r
set_tbl(x, tbl, tbl_name = NULL, label = NULL)
```

## Arguments

- x:

  *A pointblank agent or informant object*

  `obj:<ptblank_agent|ptblank_informant>` // **required**

  An *agent* object of class `ptblank_agent`, or, an *informant* of
  class `ptblank_informant`.

- tbl:

  *Table or expression for reading in one*

  `obj:<tbl_*>|<tbl reading expression>` // **required**

  The input table for the *agent* or the *informant*. This can be a data
  frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
  Alternatively, an expression can be supplied to serve as instructions
  on how to retrieve the target table at interrogation- or
  incorporation-time. There are two ways to specify an association to a
  target table: (1) as a table-prep formula, which is a right-hand side
  (RHS) formula expression (e.g., `~ { <tbl reading code>}`), or (2) as
  a function (e.g., `function() { <tbl reading code>}`).

- tbl_name:

  *A table name*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A optional name to assign to the new input table object. If no value
  is provided, a name will be generated based on whatever information is
  available.

- label:

  *An optional label for reporting*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional label for the validation plan or information report. If no
  value is provided then any existing label will be retained.

## Examples

Set proportional failure thresholds to the `warn`, `stop`, and `notify`
states using
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md).

    al <-
      action_levels(
          warn_at = 0.10,
          stop_at = 0.25,
        notify_at = 0.35
      )

Create an agent that has `small_table` set as the target table via
`tbl`. Apply the actions, add some validation steps and then interrogate
the data.

    agent_1 <-
      create_agent(
        tbl = small_table,
        tbl_name = "small_table",
        label = "An example.",
        actions = al
      ) %>%
      col_exists(columns = c(date, date_time)) %>%
      col_vals_regex(
        columns = b,
        regex = "[0-9]-[a-z]{3}-[0-9]{3}"
      ) %>%
      rows_distinct() %>%
      interrogate()

Replace the agent's association to `small_table` with a mutated version
of it (one that removes duplicate rows). Then, interrogate the new
target table.

    agent_2 <-
      agent_1 %>%
      set_tbl(
        tbl = small_table %>% dplyr::distinct()
      ) %>%
      interrogate()

## Function ID

9-4

## See also

Other Object Ops:
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md),
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md),
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md),
[`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md),
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md),
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
