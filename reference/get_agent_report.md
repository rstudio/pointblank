# Get a summary report from an agent

We can get an informative summary table from an agent by using the
`get_agent_report()` function. The table can be provided in two
substantially different forms: as a **gt** based display table (the
default), or, as a tibble. The amount of fields with intel is different
depending on whether or not the agent performed an interrogation (with
the
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
function). Basically, before
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
is called, the agent will contain just the validation plan (however many
rows it has depends on how many validation functions were supplied a
part of that plan). Post-interrogation, information on the passing and
failing test units is provided, along with indicators on whether certain
failure states were entered (provided they were set through `actions`).
The display table variant of the agent report, the default form, will
have the following columns:

- i (unlabeled): the validation step number.

- STEP: the name of the validation function used for the validation
  step,

- COLUMNS: the names of the target columns used in the validation step
  (if applicable).

- VALUES: the values used in the validation step, where applicable; this
  could be as literal values, as column names, an expression, etc.

- TBL: indicates whether any there were any changes to the target table
  just prior to interrogation. A rightward arrow from a small circle
  indicates that there was no mutation of the table. An arrow from a
  circle to a purple square indicates that preconditions were used to
  modify the target table. An arrow from a circle to a half-filled
  circle indicates that the target table has been segmented.

- EVAL: a symbol that denotes the success of interrogation evaluation
  for each step. A checkmark indicates no issues with evaluation. A
  warning sign indicates that a warning occurred during evaluation. An
  explosion symbol indicates that evaluation failed due to an error.
  Hover over the symbol for details on each condition.

- UNITS: the total number of test units for the validation step

- PASS: on top is the absolute number of *passing* test units and below
  that is the fraction of *passing* test units over the total number of
  test units.

- FAIL: on top is the absolute number of *failing* test units and below
  that is the fraction of *failing* test units over the total number of
  test units.

- W, S, N: indicators that show whether the `warn`, `stop`, or `notify`
  states were entered; unset states appear as dashes, states that are
  set with thresholds appear as unfilled circles when not entered and
  filled when thresholds are exceeded (colors for W, S, and N are amber,
  red, and blue)

- EXT: a column that provides buttons to download data extracts as CSV
  files for row-based validation steps having **failing** test units.
  Buttons only appear when there is data to collect.

The small version of the display table (obtained using `size = "small"`)
omits the `COLUMNS`, `TBL`, and `EXT` columns. The width of the small
table is 575px; the standard table is 875px wide.

The `ptblank_agent_report` can be exported to a standalone HTML document
with the
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md)
function.

If choosing to get a tibble (with `display_table = FALSE`), it will have
the following columns:

- i: the validation step number.

- type: the name of the validation function used for the validation
  step.

- columns: the names of the target columns used in the validation step
  (if applicable).

- values: the values used in the validation step, where applicable; for
  a
  [`conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md)
  validation step, this is a listing of all sub-validations.

- precon: indicates whether any there are any preconditions to apply
  before interrogation and, if so, the number of statements used.

- active: a logical value that indicates whether a validation step is
  set to `"active"` during an interrogation.

- eval: a character value that denotes the success of interrogation
  evaluation for each step. A value of `"OK"` indicates no issues with
  evaluation. The `"WARNING"` value indicates a warning occurred during
  evaluation. The `"ERROR"` VALUES indicates that evaluation failed due
  to an error. With `"W+E"` both warnings and an error occurred during
  evaluation.

- units: the total number of test units for the validation step.

- n_pass: the number of *passing* test units.

- f_pass: the fraction of *passing* test units.

- W, S, N: logical value stating whether the `warn`, `stop`, or `notify`
  states were entered. Will be `NA` for states that are unset.

- extract: an integer value that indicates the number of rows available
  in a data extract. Will be `NA` if no extract is available.

## Usage

``` r
get_agent_report(
  agent,
  arrange_by = c("i", "severity"),
  keep = c("all", "fail_states"),
  display_table = TRUE,
  size = "standard",
  title = ":default:",
  lang = NULL,
  locale = NULL
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

- arrange_by:

  *Method of arranging the report's table rows*

  `singl-kw:[i|severity]` // *default:* `"i"`

  A choice to arrange the report table rows by the validation step
  number (`"i"`, the default), or, to arrange in descending order by
  severity of the failure state (with `"severity"`).

- keep:

  *Which table rows should be kept?*

  `singl-kw:[all|fail_states]` // *default:* `"all"`

  An option to keep `"all"` of the report's table rows (the default),
  or, keep only those rows that reflect one or more `"fail_states"`.

- display_table:

  *Return a display-table report via gt*

  `scalar<logical>` // *default:* `TRUE`

  Should a display table be generated? If `TRUE`, and if the **gt**
  package is installed, a display table for the report will be shown in
  the Viewer. If `FALSE`, or if **gt** is not available, then a tibble
  will be returned.

- size:

  *Size option for display-table report*

  `scalar<character>` // *default:* `"standard"`

  The size of the display table, which can be either `"standard"` (the
  default) or `"small"`. This only applies to a display table (where
  `display_table = TRUE`).

- title:

  *Title customization options*

  `scalar<character>` // *default:* `":default:"`

  Options for customizing the title of the report. The default is the
  keyword `":default:"` which produces generic title text that refers to
  the **pointblank** package in the language governed by the `lang`
  option. Another keyword option is `":tbl_name:"`, and that presents
  the name of the table as the title for the report. If no title is
  wanted, then the `":none:"` keyword option can be used. Aside from
  keyword options, text can be provided for the title and
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) calls
  can be used to construct the text string. If providing text, it will
  be interpreted as Markdown text and transformed internally to HTML. To
  circumvent such a transformation, use text in
  [`I()`](https://rdrr.io/r/base/AsIs.html) to explicitly state that the
  supplied text should not be transformed.

- lang:

  *Reporting language*

  `scalar<character>` // *default:* `NULL` (`optional`)

  The language to use for automatic creation of briefs (short
  descriptions for each validation step) and for the *agent report* (a
  summary table that provides the validation plan and the results from
  the interrogation. By default, `NULL` will create English (`"en"`)
  text. Other options include French (`"fr"`), German (`"de"`), Italian
  (`"it"`), Spanish (`"es"`), Portuguese (`"pt"`), Turkish (`"tr"`),
  Chinese (`"zh"`), Russian (`"ru"`), Polish (`"pl"`), Danish (`"da"`),
  Swedish (`"sv"`), and Dutch (`"nl"`). This `lang` option will override
  any previously set language setting (e.g., by the
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
  call).

- locale:

  *Locale for value formatting*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional locale ID to use for formatting values in the *agent
  report* summary table according the locale's rules. Examples include
  `"en_US"` for English (United States) and `"fr_FR"` for French
  (France); more simply, this can be a language identifier without a
  country designation, like `"es"` for Spanish (Spain, same as
  `"es_ES"`). This `locale` option will override any previously set
  locale value (e.g., by the
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
  call).

## Value

A `ptblank_agent_report` object if `display_table = TRUE` or a tibble if
`display_table = FALSE`.

## Examples

For the example here, we'll use a simple table with a single numerical
column `a`.

    tbl <- dplyr::tibble(a = c(5, 7, 8, 5))

    tbl
    #> # A tibble: 4 x 1
    #>       a
    #>   <dbl>
    #> 1     5
    #> 2     7
    #> 3     8
    #> 4     5

Let's create an *agent* and validate that values in column `a` are
always greater than `4`.

    agent <-
      create_agent(
        tbl = tbl,
        tbl_name = "small_table",
        label = "An example."
      ) %>%
      col_vals_gt(columns = a, value = 4) %>%
      interrogate()

We can get a tibble-based report from the agent by using
`get_agent_report()` with `display_table = FALSE`.

    agent %>% get_agent_report(display_table = FALSE)

    ## # A tibble: 1 × 14
    ##       i type    columns values precon active eval  units n_pass
    ##   <int> <chr>   <chr>   <chr>  <chr>  <lgl>  <chr> <dbl>  <dbl>
    ## 1     1 col_va… a       4      NA     TRUE   OK        4      4
    ## # … with 5 more variables: f_pass <dbl>, W <lgl>, S <lgl>,
    ## #   N <lgl>, extract <int>

The full-featured display-table-based report can be viewed by printing
the `agent` object, but, we can get a `"ptblank_agent_report"` object
returned to us when using `display_table = TRUE` (the default for
`get_agent_report`).

    report <- get_agent_report(agent)

    report

![This image was generated from the first code example in the
\`get_agent_report()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_get_agent_report_1.png)

What can you do with the `report` object? Print it at will wherever,
and, it can serve as an input to the
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md)
function.

However, the better reason to use `get_agent_report()` over just
printing the agent for display-table purposes is to make use of the
different display options.

The agent report as a **gt** display table comes in two sizes:
`"standard"` (the default, 875px wide) and `"small"` (575px wide). Let's
take a look at the smaller-sized version of the report.

    small_report <-
      get_agent_report(
        agent = agent,
        size = "small"
      )

    small_report

![This image was generated from the second code example in the
\`get_agent_report()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_get_agent_report_2.png)

We can use our own title by supplying it to the `title` argument, or,
use a special keyword like `":tbl_name:"` to get the table name (set in
the
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
call) as the title.

    report_title <- get_agent_report(agent, title = ":tbl_name:")

    report_title

![This image was generated from the third code example in the
\`get_agent_report()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_get_agent_report_3.png)

There are more options! You can change the language of the display table
with the `lang` argument (this overrides the language set in
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)),
validation steps can be rearranged using the `arrange_by` argument, and
we can also apply some filtering with the `keep` argument in
`get_agent_report()`.

## Function ID

6-2

## See also

Other Interrogate and Report:
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
