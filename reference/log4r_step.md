# Enable logging of failure conditions at the validation step level

The `log4r_step()` function can be used as an action in the
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
function (as a list component for the `fns` list). Place a call to this
function in every failure condition that should produce a log (i.e.,
`warn`, `stop`, `notify`). Only the failure condition with the highest
severity for a given validation step will produce a log entry (skipping
failure conditions with lower severity) so long as the call to
`log4r_step()` is present.

## Usage

``` r
log4r_step(x, message = NULL, append_to = "pb_log_file")
```

## Arguments

- x:

  A reference to the x-list object prepared by the `agent`. This version
  of the x-list is the same as that generated via
  `get_agent_x_list(<agent>, i = <step>)` except this version is
  internally generated and hence only available in an internal
  evaluation context.

- message:

  The message to use for the log entry. When not provided, a default
  glue string is used for the messaging. This is dynamic since the
  internal
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) call
  occurs in the same environment as `x`, the x-list that's constrained
  to the validation step. The default message, used when
  `message = NULL` is the glue string
  `"Step {x$i} exceeded the {level} failure threshold (f_failed = {x$f_failed}) ['{x$type}']"`.
  As can be seen, a custom message can be crafted that uses other
  elements of the x-list with the `{x$<component>}` construction.

- append_to:

  The file to which log entries at the warn level are appended. This can
  alternatively be one or more **log4r** appenders.

## Value

Nothing is returned however log files may be written in very specific
conditions.

## YAML

A **pointblank** agent can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used to regenerate an agent (with
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md))
or interrogate the target table (via
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)).
Here is an example of how `log4r_step()` can be expressed in R code
(within
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md),
itself inside
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md))
and in the corresponding YAML representation.

R statement:

    create_agent(
      tbl = ~ small_table,
      tbl_name = "small_table",
      label = "An example.",
      actions = action_levels(
        warn_at = 1,
        fns = list(
          warn = ~ log4r_step(
            x, append_to = "example_log"
          )
        )
      )
    )

YAML representation:

    type: agent
    tbl: ~small_table
    tbl_name: small_table
    label: An example.
    lang: en
    locale: en
    actions:
      warn_count: 1.0
      fns:
        warn: ~log4r_step(x, append_to = "example_log")
    steps: []

Should you need to preview the transformation of an *agent* to YAML
(without any committing anything to disk), use the
[`yaml_agent_string()`](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md)
function. If you already have a `.yml` file that holds an *agent*, you
can get a glimpse of the R expressions that are used to regenerate that
agent with
[`yaml_agent_show_exprs()`](https://rstudio.github.io/pointblank/reference/yaml_agent_show_exprs.md).

## Examples

For the example provided here, we'll use the included `small_table`
dataset. We are also going to create an
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
list object since this is useful for demonstrating a logging scenario.
It will have a threshold for the `warn` state, and, an associated
function that should be invoked whenever the `warn` state is entered.
Here, the function call with `log4r_step()` will be invoked whenever
there is one failing test unit.

    al <-
      action_levels(
        warn_at = 1,
        fns = list(
          warn = ~ log4r_step(
            x, append_to = "example_log"
          )
        )
      )

Within the
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)-produced
object, it's important to match things up: notice that `warn_at` is
given a threshold and the list of functions given to `fns` has a `warn`
component.

Printing `al` will show us the settings for the `action_levels` object:

    al
    #> -- The `action_levels` settings
    #> WARN failure threshold of 1test units.
    #> \fns\ ~ log4r_step(x, append_to = "example_log")
    #> ----

Let's create an agent with `small_table` as the target table. We'll
apply the `action_levels` object created above as `al`, add two
validation steps, and then
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
the data.

    agent <-
      create_agent(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "An example.",
        actions = al
      ) %>%
      col_vals_gt(columns = d, 300) %>%
      col_vals_in_set(columns = f, c("low", "high")) %>%
      interrogate()

    agent

![This image was generated from the first code example in the
\`log4r_step()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_log4r_step_1.png)

From the agent report, we can see that both steps have yielded warnings
upon interrogation (i.e., filled yellow circles in the `W` column).

What's not immediately apparent is that when entering the `warn` state
in each validation step during interrogation, the `log4r_step()`
function call was twice invoked! This generated an `"example_log"` file
in the working directory (since it was not present before the
interrogation) and log entries were appended to the file. Here are the
contents of the file:

    WARN  [2022-06-28 10:06:01] Step 1 exceeded the WARN failure threshold
      (f_failed = 0.15385) ['col_vals_gt']
    WARN  [2022-06-28 10:06:01] Step 2 exceeded the WARN failure threshold
      (f_failed = 0.15385) ['col_vals_in_set']

## Function ID

5-1
