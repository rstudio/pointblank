# Did all of the validations fully *pass*?

Given an agent's validation plan that had undergone interrogation via
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md),
did every single validation step result in zero *failing* test units?
Using the `all_passed()` function will let us know whether that's `TRUE`
or not.

## Usage

``` r
all_passed(agent, i = NULL)
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

  *Validation step numbers*

  `vector<integer>` // *default:* `NULL` (`optional`)

  A vector of validation step numbers. These values are assigned to each
  validation step by **pointblank** in the order of definition. If
  `NULL` (the default), all validation steps will be used for the
  evaluation of complete *passing*.

## Value

A logical value.

## Details

The `all_passed()` function provides a single logical value based on an
interrogation performed in the *agent*-based workflow. For very
large-scale validation (where data quality is a known issue, and is
perhaps something to be tamed over time) this function is likely to be
less useful since it is quite stringent (all test units must pass across
all validation steps).

Should there be a requirement for logical values produced from
validation, a more flexible alternative is in using the test
(`test_*()`) variants of the validation functions. Each of those produce
a single logical value and each and have a `threshold` option for
failure levels. Another option is to utilize post-interrogation objects
within the *agent*'s x-list (obtained by using the
[`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md)
function). This allows for many possibilities in producing a single
logical value from an interrogation.

## Examples

Create a simple table with a column of numerical values.

    tbl <- dplyr::tibble(a = c(4, 5, 7, 8))

    tbl
    #> # A tibble: 4 x 1
    #>       a
    #>   <dbl>
    #> 1     4
    #> 2     5
    #> 3     7
    #> 4     8

Validate that values in column `a` are always greater than 4.

    agent <-
      create_agent(tbl = tbl) %>%
      col_vals_gt(columns = a, value = 3) %>%
      col_vals_lte(columns = a, value = 10) %>%
      col_vals_increasing(columns = a) %>%
      interrogate()

Determine if these column validations have all passed by using
`all_passed()` (they do).

    all_passed(agent = agent)

    #> [1] TRUE

## Function ID

8-4

## See also

Other Post-interrogation:
[`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md),
[`get_data_extracts()`](https://rstudio.github.io/pointblank/reference/get_data_extracts.md),
[`get_sundered_data()`](https://rstudio.github.io/pointblank/reference/get_sundered_data.md),
[`write_testthat_file()`](https://rstudio.github.io/pointblank/reference/write_testthat_file.md)
