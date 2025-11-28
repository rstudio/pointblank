# Set action levels: failure thresholds and functions to invoke

The `action_levels()` function works with the `actions` argument that is
present in the
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
function and in every validation step function (which also has an
`actions` argument). With it, we can provide threshold *failure* values
for any combination of `warn`, `stop`, or `notify` failure states.

We can react to any entering of a state by supplying corresponding
functions to the `fns` argument. They will undergo evaluation at the
time when the matching state is entered. If provided to
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
then the policies will be applied to every validation step, acting as a
default for the validation as a whole.

Calls of `action_levels()` could also be applied directly to any
validation step and this will act as an override if set also in
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md).
Usage of `action_levels()` is required to have any useful side effects
(i.e., warnings, throwing errors) in the case of validation functions
operating directly on data (e.g., `mtcars %>% col_vals_lt("mpg", 35)`).
There are two helper functions that are convenient when using validation
functions directly on data (the `agent`-less workflow): `warn_on_fail()`
and `stop_on_fail()`. These helpers either warn or stop (default failure
threshold for each is set to `1`), and, they do so with informative
warning or error messages. The `stop_on_fail()` helper is applied by
default when using validation functions directly on data (more
information on this is provided in *Details*).

## Usage

``` r
action_levels(warn_at = NULL, stop_at = NULL, notify_at = NULL, fns = NULL)

warn_on_fail(warn_at = 1)

stop_on_fail(stop_at = 1)
```

## Arguments

- warn_at:

  *Threshold value for the 'warn' failure state*

  `scalar<integer|numeric>(val>=0)` // *default:* `NULL` (`optional`)

  Either the threshold number or the threshold fraction of *failing*
  test units that result in entering the `warn` failure state.

- stop_at:

  *Threshold value for the 'stop' failure state*

  `scalar<integer|numeric>(val>=0)` // *default:* `NULL` (`optional`)

  Either the threshold number or the threshold fraction of *failing*
  test units that result in entering the `stop` failure state.

- notify_at:

  *Threshold value for the 'notify' failure state*

  `scalar<integer|numeric>(val>=0)` // *default:* `NULL` (`optional`)

  Either the threshold number or the threshold fraction of *failing*
  test units that result in entering the `notify` failure state.

- fns:

  *Functions to execute when entering failure states*

  `list` // *default:* `NULL` (`optional`)

  A named list of functions that is to be paired with the appropriate
  failure states. The syntax for this list involves using failure state
  names from the set of `warn`, `stop`, and `notify`. The functions
  corresponding to the failure states are provided as formulas (e.g.,
  `list(warn = ~ warning("Too many failures."))`. A series of
  expressions for each named state can be used by enclosing the set of
  statements with [`{ }`](https://rdrr.io/r/base/Paren.html).

## Value

An `action_levels` object.

## Details

The output of the `action_levels()` call in `actions` will be
interpreted slightly differently if using an *agent* or using validation
functions directly on a data table. For convenience, when working
directly on data, any values supplied to `warn_at` or `stop_at` will be
automatically given a stock
[`warning()`](https://rdrr.io/r/base/warning.html) or
[`stop()`](https://rdrr.io/r/base/stop.html) function. For example using
`small_table %>% col_is_integer("date")` will provide a detailed stop
message by default, indicating the reason for the failure. If you were
to supply the `fns` for `stop` or `warn` manually then the stock
functions would be overridden. Furthermore, if `actions` is NULL in this
workflow (the default), **pointblank** will use a `stop_at` value of `1`
(providing a detailed, context-specific error message if there are any
*failing* units). We can absolutely suppress this automatic stopping
behavior at each validation step by setting `active = FALSE`. In this
interactive data case, there is no stock function given for `notify_at`.
The `notify` failure state is less commonly used in this workflow as it
is in the *agent*-based one.

When using an *agent*, we often opt to not use any functions in `fns` as
the `warn`, `stop`, and `notify` failure states will be reported on when
using `create_agent_report()` (and, usually that's sufficient). Instead,
using the `end_fns` argument is a better choice since that scheme
provides useful data on the entire interrogation, allowing for finer
control on side effects and reducing potential for duplicating any side
effects.

## Defining threshold values

Any threshold values supplied for the `warn_at`, `stop_at`, or
`notify_at` arguments correspond to the `warn`, `stop`, and `notify`
failure states, respectively. A threshold value can either relates to an
absolute number of test units or a fraction-of-total test units that are
*failing*. Exceeding the threshold means entering one or more of the
`warn`, `stop`, or `notify` failure states.

If a threshold value is a decimal value between `0` and `1` then it's a
proportional failure threshold (e.g., `0.15` indicates that if 15
percent of the test units are found to be *failing*, then the designated
failure state is entered). Absolute values starting from `1` can be used
instead, and this constitutes an absolute failure threshold (e.g., `10`
means that if 10 of the test units are found to be *failing*, the
failure state is entered).

## Examples

For these examples, we will use the included `small_table` dataset.

    small_table
    #> # A tibble: 13 x 8
    #>    date_time           date           a b             c      d e     f
    #>    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
    #>  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high
    #>  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low
    #>  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high
    #>  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid
    #>  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low
    #>  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid
    #>  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high
    #>  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low
    #>  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    #> 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high
    #> 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-010     7   834. TRUE  low
    #> 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-010     8   108. FALSE low
    #> 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA  2230. TRUE  high

Create an `action_levels` object with fractional values for the `warn`,
`stop`, and `notify` states.

    al <-
      action_levels(
        warn_at = 0.2,
        stop_at = 0.8,
        notify_at = 0.5
      )

A summary of settings for the `al` object is shown by printing it.

Create a pointblank agent and apply the `al` object to `actions`. Add
two validation steps and interrogate the `small_table`.

    agent_1 <-
      create_agent(
        tbl = small_table,
        actions = al
      ) %>%
      col_vals_gt(
        columns = a, value = 2
      ) %>%
      col_vals_lt(
        columns = d, value = 20000
      ) %>%
      interrogate()

The report from the agent will show that the `warn` state has been
entered for the first validation step but not the second one. We can
confirm this in the console by inspecting the `warn` component in the
agent's x-list.

    x_list <- get_agent_x_list(agent = agent_1)

    x_list$warn

    ## [1]  TRUE FALSE

Applying the `action_levels` object to the agent means that all
validation steps will inherit these settings but we can override this by
applying another such object to the validation step instead (this time
using the `warn_on_fail()` shorthand).

    agent_2 <-
      create_agent(
        tbl = small_table,
        actions = al
      ) %>%
      col_vals_gt(
        columns = a, value = 2,
        actions = warn_on_fail(warn_at = 0.5)
      ) %>%
      col_vals_lt(
        columns = d, value = 20000
      ) %>%
      interrogate()

In this case, the first validation step has a less stringent failure
threshold for the `warn` state and it's high enough that the condition
is not entered. This can be confirmed in the console through inspection
of the x-list `warn` component.

    x_list <- get_agent_x_list(agent = agent_2)

    x_list$warn

    ## [1] FALSE FALSE

In the context of using validation functions directly on data (i.e., no
involvement of an agent) we want to trigger warnings and raise errors.
The following will yield a warning if it is executed (returning the
`small_table` data).

    small_table %>%
      col_vals_gt(
        columns = a, value = 2,
        actions = warn_on_fail(warn_at = 2)
      )

    ## # A tibble: 13 × 8
    ##    date_time           date           a b           c      d e
    ##    <dttm>              <date>     <int> <chr>   <dbl>  <dbl> <lgl>
    ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE
    ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE
    ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE
    ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE
    ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE
    ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE
    ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE
    ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE
    ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE
    ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE
    ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-…     7   834. TRUE
    ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-…     8   108. FALSE
    ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-…    NA  2230. TRUE
    ## # … with 1 more variable: f <chr>
    ## Warning message:
    ## Exceedance of failed test units where values in `a` should have been >
    ## `2`.
    ## The `col_vals_gt()` validation failed beyond the absolute threshold
    ## level (2).
    ## * failure level (4) >= failure threshold (2)

With the same pipeline, not supplying anything for `actions` (it's
`NULL` by default) will have the same effect as using
`stop_on_fail(stop_at = 1)`.

    small_table %>%
      col_vals_gt(columns = a, value = 2)

    ## Error: Exceedance of failed test units where values in `a` should have
    ## been > `2`.
    ## The `col_vals_gt()` validation failed beyond the absolute threshold
    ## level (1).
    ## * failure level (4) >= failure threshold (1)

Here's the equivalent set of statements:

    small_table %>%
      col_vals_gt(
        columns = a, value = 2,
        actions = stop_on_fail(stop_at = 1)
      )

    ## Error: Exceedance of failed test units where values in `a` should have
    ## been > `2`.
    ## The `col_vals_gt()` validation failed beyond the absolute threshold
    ## level (1).
    ## * failure level (4) >= failure threshold (1)

This is because the `stop_on_fail()` call is auto-injected in the
default case (when operating on data) for your convenience. Behind the
scenes a 'secret agent' uses 'covert actions': all so you can type less.

## Function ID

1-5

## See also

Other Planning and Prep:
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md),
[`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md),
[`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md),
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md),
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md),
[`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md),
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md),
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md),
[`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
