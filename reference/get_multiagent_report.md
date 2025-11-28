# Get a summary report using multiple agents

We can get an informative summary table from a collective of agents by
using the `get_multiagent_report()` function. Information from multiple
agent can be provided in three very forms: (1) the *Long Display*
(stacked reports), (2) the *Wide Display* (a comparison report), (3) as
a tibble with packed columns.

## Usage

``` r
get_multiagent_report(
  multiagent,
  display_table = TRUE,
  display_mode = c("long", "wide"),
  title = ":default:",
  lang = NULL,
  locale = NULL
)
```

## Arguments

- multiagent:

  A multiagent object of class `ptblank_multiagent`.

- display_table:

  Should a display table be generated? If `TRUE` (the default) a display
  table for the report will be shown in the Viewer. If `FALSE` then a
  tibble will be returned.

- display_mode:

  If we are getting a display table, should the agent data be presented
  in a `"long"` or `"wide"` form? The default is `"long"` but when
  comparing multiple runs where the target table is the same it might be
  preferable to choose `"wide"`.

- title:

  Options for customizing the title of the report when
  `display_table = TRUE`. The default is the keyword `":default:"` which
  produces generic title text. If no title is wanted, then the
  `":none:"` keyword option can be used. Another keyword option is
  `":tbl_name:"`, and that presents the name of the table as the title
  for the report (this can only be used when `display_mode = "long"`).
  Aside from keyword options, text can be provided for the title and
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) calls
  can be used to construct the text string. If providing text, it will
  be interpreted as Markdown text and transformed internally to HTML. To
  circumvent such a transformation, use text in
  [`I()`](https://rdrr.io/r/base/AsIs.html) to explicitly state that the
  supplied text should not be transformed.

- lang:

  *Reporting language*

  `scalar<character>` // *default:* `NULL` (`optional`)

  The language to use for the long or wide report forms. By default,
  `NULL` will preserve any language set in the component reports. The
  following options will force the same language across all component
  reports: English (`"en"`), French (`"fr"`), German (`"de"`), Italian
  (`"it"`), Spanish (`"es"`), Portuguese (`"pt"`), Turkish (`"tr"`),
  Chinese (`"zh"`), Russian (`"ru"`), Polish (`"pl"`), Danish (`"da"`),
  Swedish (`"sv"`), and Dutch (`"nl"`).

- locale:

  *Locale for value formatting*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional locale ID to use for formatting values in the long or wide
  report forms (according the locale's rules). Examples include
  `"en_US"` for English (United States) and `"fr_FR"` for French
  (France); more simply, this can be a language identifier without a
  country designation, like `"es"` for Spanish (Spain, same as
  `"es_ES"`). This `locale` option will override any previously set
  locale values.

## Value

A **gt** table object if `display_table = TRUE` or a tibble if
`display_table = FALSE`.

## The Long Display

When displayed as `"long"` the multiagent report will stack individual
agent reports in a single document in the order of the agents in the
multiagent object.

Each validation plan (possibly with interrogation info) will be provided
and the output for each is equivalent to calling
[`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md)
on each of the agents within the multiagent object.

## The Wide Display

When displayed as `"wide"` the multiagent report will show data from
individual agents as columns, with rows standing as validation steps
common across the agents.

Each validation step is represented with an icon (standing in for the
name of the validation function) and the associated SHA1 hash. This is a
highly trustworthy way for ascertaining which validation steps are
effectively identical across interrogations. This way of organizing the
report is beneficial because different agents may have used different
steps and we want to track the validation results where the validation
step doesn't change but the target table does (i.e., new rows are added,
existing rows are updated, etc.).

The single table from this display mode will have the following columns:

- STEP: the SHA1 hash for the validation step, possibly shared among
  several interrogations.

- *subsequent columns*: each column beyond `STEP` represents a separate
  interrogation from an *agent* object. The time stamp for the
  completion of each interrogation is shown as the column label.

## Examples

Let's walk through several theoretical data quality analyses of an
extremely small table. that table is called `small_table` and we can
find it as a dataset in this package.

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

To set failure limits and signal conditions, we designate proportional
failure thresholds to the `warn`, `stop`, and `notify` states using
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md).

    al <-
      action_levels(
        warn_at = 0.05,
        stop_at = 0.10,
        notify_at = 0.20
      )

We will create four different agents and have slightly different
validation steps in each of them. In the first, `agent_1`, eight
different validation steps are created and the agent will interrogate
the `small_table`.

    agent_1 <-
      create_agent(
        tbl = small_table,
        label = "An example.",
        actions = al
      ) %>%
      col_vals_gt(
        columns = date_time,
        value = vars(date),
        na_pass = TRUE
      ) %>%
      col_vals_gt(
        columns = b,
        value = vars(g),
        na_pass = TRUE
      ) %>%
      rows_distinct() %>%
      col_vals_equal(
        columns = d,
        value = vars(d),
        na_pass = TRUE
      ) %>%
      col_vals_between(
        columns = c,
        left = vars(a), right = vars(d)
      ) %>%
      col_vals_not_between(
        columns = c,
        left = 10, right = 20,
        na_pass = TRUE
      ) %>%
      rows_distinct(columns = d, e, f) %>%
      col_is_integer(columns = a) %>%
      interrogate()

The second agent, `agent_2`, retains all of the steps of `agent_1` and
adds two more (the last of which is inactive).

    agent_2 <-
      agent_1 %>%
      col_exists(columns = date, date_time) %>%
      col_vals_regex(
        columns = b,
        regex = "[0-9]-[a-z]{3}-[0-9]{3}",
        active = FALSE
      ) %>%
      interrogate()

The third agent, `agent_3`, adds a single validation step, removes the
fifth one, and deactivates the first.

    agent_3 <-
      agent_2 %>%
      col_vals_in_set(
        columns = f,
        set = c("low", "mid", "high")
      ) %>%
      remove_steps(i = 5) %>%
      deactivate_steps(i = 1) %>%
      interrogate()

The fourth and final agent, `agent_4`, reactivates steps 1 and 10, and
removes the sixth step.

    agent_4 <-
      agent_3 %>%
      activate_steps(i = 1) %>%
      activate_steps(i = 10) %>%
      remove_steps(i = 6) %>%
      interrogate()

While all the agents are slightly different from each other, we can
still get a combined report of them by creating a 'multiagent'.

    multiagent <-
      create_multiagent(
        agent_1, agent_2, agent_3, agent_4
      )

Calling `multiagent` in the console prints the multiagent report. But we
can generate a `"ptblank_multiagent_report"` object with the
`get_multiagent_report()` function and specify options for layout and
presentation.

By default, `get_multiagent_report()` gives you a long report with agent
reports being stacked. Think of this `"long"` option as the serial mode
of agent reports. However if we want to view interrogation results of
the same table over time, the wide view may be preferable. In this way
we can see whether the results of common validation steps improved or
worsened over consecutive interrogations of the data.

    report_wide <-
      get_multiagent_report(
        multiagent,
        display_mode = "wide"
      )

    report_wide

![This image was generated from the first code example in the
\`get_multiagent_report()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_get_multiagent_report_1.png)

## Function ID

10-3

## See also

Other The multiagent:
[`create_multiagent()`](https://rstudio.github.io/pointblank/reference/create_multiagent.md),
[`read_disk_multiagent()`](https://rstudio.github.io/pointblank/reference/read_disk_multiagent.md)
