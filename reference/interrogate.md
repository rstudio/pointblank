# Given an agent that has a validation plan, perform an interrogation

When the agent has all the information on what to do (i.e., a validation
plan which is a series of validation steps), the interrogation process
can occur according its plan. After that, the agent will have gathered
intel, and we can use functions like
[`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md)
and
[`all_passed()`](https://rstudio.github.io/pointblank/reference/all_passed.md)
to understand how the interrogation went down.

## Usage

``` r
interrogate(
  agent,
  extract_failed = TRUE,
  extract_tbl_checked = TRUE,
  get_first_n = NULL,
  sample_n = NULL,
  sample_frac = NULL,
  sample_limit = 5000,
  show_step_label = FALSE,
  progress = interactive()
)
```

## Arguments

- agent:

  *The pointblank agent object*

  `obj:<ptblank_agent>` // **required**

  A **pointblank** *agent* object that is commonly created through the
  use of the
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
  function.

- extract_failed:

  *Collect failed rows as data extracts*

  `scalar<logical>` // *default:* `TRUE`

  An option to collect rows that didn't pass a particular validation
  step. The default is `TRUE` and further options allow for fine control
  of how these rows are collected.

- extract_tbl_checked:

  *Collect validation results from each step*

  `scalar<logical>` // *default:* `TRUE`

  An option to collect processed data frames produced by executing the
  validation steps. This information is necessary for some functions
  (e.g.,
  [`get_sundered_data()`](https://rstudio.github.io/pointblank/reference/get_sundered_data.md)),
  but may grow to a large size. To opt out of attaching this data to the
  agent, set this argument to `FALSE`.

- get_first_n:

  *Get the first n values*

  `scalar<integer>` // *default:* `NULL` (`optional`)

  If the option to collect non-passing rows is chosen, there is the
  option here to collect the first `n` rows here. Supply the number of
  rows to extract from the top of the non-passing rows table (the
  ordering of data from the original table is retained).

- sample_n:

  *Sample n values*

  `scalar<integer>` // *default:* `NULL` (`optional`)

  If the option to collect non-passing rows is chosen, this option
  allows for the sampling of `n` rows. Supply the number of rows to
  sample from the non-passing rows table. If `n` is greater than the
  number of non-passing rows, then all the rows will be returned.

- sample_frac:

  *Sample a fraction of values*

  `scalar<numeric>` // *default:* `NULL` (`optional`)

  If the option to collect non-passing rows is chosen, this option
  allows for the sampling of a fraction of those rows. Provide a number
  in the range of `0` and `1`. The number of rows to return may be
  extremely large (and this is especially when querying remote
  databases), however, the `sample_limit` option will apply a hard limit
  to the returned rows.

- sample_limit:

  *Row limit for sampling*

  `scalar<integer>` // *default:* `5000`

  A value that limits the possible number of rows returned when sampling
  non-passing rows using the `sample_frac` option.

- show_step_label:

  *Show step labels in progress*

  `scalar<logical>` // *default:* `FALSE`

  Whether to show the `label` value of each validation step in the
  console.

- progress:

  *Show interrogation progress*

  `scalar<logical>` // *default:*
  [`interactive()`](https://rdrr.io/r/base/interactive.html)

  Whether to show the progress of an agent's interrogation in the
  console. Defaults to `TRUE` in interactive sessions.

## Value

A `ptblank_agent` object.

## Examples

Create a simple table with two columns of numerical values.

    tbl <-
      dplyr::tibble(
        a = c(5, 7, 6, 5, 8, 7),
        b = c(7, 1, 0, 0, 0, 3)
      )

    tbl
    #> # A tibble: 6 x 2
    #>       a     b
    #>   <dbl> <dbl>
    #> 1     5     7
    #> 2     7     1
    #> 3     6     0
    #> 4     5     0
    #> 5     8     0
    #> 6     7     3

Validate that values in column `a` from `tbl` are always less than `5`.
Using `interrogate()` carries out the validation plan and completes the
whole process.

    agent <-
      create_agent(
        tbl = tbl,
        label = "`interrogate()` example"
      ) %>%
      col_vals_gt(columns = a, value = 5) %>%
      interrogate()

We can print the resulting object to see the validation report.

    agent

![This image was generated from the first code example in the
\`interrogate()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_interrogate_1.png)

## Function ID

6-1

## See also

Other Interrogate and Report:
[`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md)
