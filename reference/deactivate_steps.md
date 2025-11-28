# Deactivate one or more of an *agent*'s validation steps

Should the deactivation of one or more validation steps be necessary
after creation of the validation plan for an *agent*, the
`deactivate_steps()` function will be helpful for that. This has the
same effect as using the `active = FALSE` option (`active` is an
argument in all validation functions) for the selected validation steps.
Please note that this directly edits the validation step, wiping out any
function that may have been defined for whether the step should be
active or not.

## Usage

``` r
deactivate_steps(agent, i = NULL)
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
  in the order of definition. If `NULL` (the default) then step
  deactivation won't occur by index.

## Value

A `ptblank_agent` object.

## Function ID

9-6

## See also

For the opposite behavior, use the
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md)
function.

Other Object Ops:
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md),
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md),
[`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md),
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
  
# The second validation step is
# now being reconsidered and may
# be either phased out or improved
# upon; in the interim period it
# was decided that the step should
# be deactivated for now
agent_2 <-
  agent_1 %>%
  deactivate_steps(i = 2) %>%
  interrogate()
```
