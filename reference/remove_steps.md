# Remove one or more of an *agent*'s validation steps

Validation steps can be removed from an *agent* object through use of
the `remove_steps()` function. This is useful, for instance, when
getting an agent from disk (via the
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
function) and omitting one or more steps from the *agent*'s validation
plan. Please note that when removing validation steps all stored data
extracts will be removed from the *agent*.

## Usage

``` r
remove_steps(agent, i = NULL)
```

## Arguments

- agent:

  *The pointblank agent object*

  `obj:<ptblank_agent>` // **required**

  A **pointblank** *agent* object that is commonly created through the
  use of the
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
  function.

- i:

  *A validation step number*

  `scalar<integer>` // *default:* `NULL` (`optional`)

  The validation step number, which is assigned to each validation step
  in the order of definition. If `NULL` (the default) then step removal
  won't occur by index.

## Value

A `ptblank_agent` object.

A `ptblank_agent` object.

## Function ID

9-7

## See also

Instead of removal, the
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md)
function will simply change the `active` status of one or more
validation steps to `FALSE` (and
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md)
will do the opposite).

Other Object Ops:
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md),
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md),
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md),
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md),
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md),
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)

## Examples

``` r
# Create an agent that has the
# `small_table` object as the
# target table, add a few
# validation steps, and then use
# `interrogate()`
agent_1 <- 
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "An example."
  ) %>%
  col_exists(columns = date) %>%
  col_vals_regex(
    columns = b,
    regex = "[0-9]-[a-z]{3}-[0-9]"
  ) %>%
  interrogate()
  
# The second validation step has
# been determined to be unneeded and
# is to be removed; this can be done
# by using `remove_steps()` with the
# agent object
agent_2 <-
  agent_1 %>%
  remove_steps(i = 2) %>%
  interrogate()
```
