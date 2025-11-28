# Activate one or more of an *agent*'s validation steps

If certain validation steps need to be activated after the creation of
the validation plan for an *agent*, use the `activate_steps()` function.
This is equivalent to using the `active = TRUE` for the selected
validation steps (`active` is an argument in all validation functions).
This will replace any function that may have been defined for the
`active` argument during creation of the targeted validation steps.

## Usage

``` r
activate_steps(agent, i = NULL)
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
  activation won't occur by index.

## Value

A `ptblank_agent` object.

## Function ID

9-5

## See also

For the opposite behavior, use the
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md)
function.

Other Object Ops:
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md),
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md),
[`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md),
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md),
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md),
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)

## Examples

``` r
# Create an agent that has the
# `small_table` object as the
# target table, add a few inactive
# validation steps, and then use
# `interrogate()`
agent_1 <- 
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    label = "An example."
  ) %>%
  col_exists(
    columns = date,
    active = FALSE
  ) %>%
  col_vals_regex(
    columns = b,
    regex = "[0-9]-[a-z]{3}-[0-9]{3}",
    active = FALSE
  ) %>%
  interrogate()

# In the above, the data is
# not actually interrogated
# because the `active` setting
# was `FALSE` in all steps; we
# can selectively change this
# with `activate_steps()`
agent_2 <-
  agent_1 %>%
  activate_steps(i = 1) %>%
  interrogate()
```
