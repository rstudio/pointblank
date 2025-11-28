# Do column data lie outside of two specified values or data in other columns?

The `col_vals_not_between()` validation function, the
`expect_col_vals_not_between()` expectation function, and the
`test_col_vals_not_between()` test function all check whether column
values in a table *do not* fall within a range. The range specified with
three arguments: `left`, `right`, and `inclusive`. The `left` and
`right` values specify the lower and upper bounds. The bounds can be
specified as single, literal values or as column names given in
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html). The
`inclusive` argument, as a vector of two logical values relating to
`left` and `right`, states whether each bound is inclusive or not. The
default is `c(TRUE, TRUE)`, where both endpoints are inclusive (i.e.,
`[left, right]`). For partially-unbounded versions of this function, we
can use the
[`col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md),
[`col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md),
[`col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md),
or
[`col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md)
validation functions. The validation function can be used directly on a
data table or with an *agent* object (technically, a `ptblank_agent`
object) whereas the expectation and test functions can only be used with
a data table. Each validation step or expectation will operate over the
number of test units that is equal to the number of rows in the table
(after any `preconditions` have been applied).

## Usage

``` r
col_vals_not_between(
  x,
  columns,
  left,
  right,
  inclusive = c(TRUE, TRUE),
  na_pass = FALSE,
  preconditions = NULL,
  segments = NULL,
  actions = NULL,
  step_id = NULL,
  label = NULL,
  brief = NULL,
  active = TRUE
)

expect_col_vals_not_between(
  object,
  columns,
  left,
  right,
  inclusive = c(TRUE, TRUE),
  na_pass = FALSE,
  preconditions = NULL,
  threshold = 1
)

test_col_vals_not_between(
  object,
  columns,
  left,
  right,
  inclusive = c(TRUE, TRUE),
  na_pass = FALSE,
  preconditions = NULL,
  threshold = 1
)
```

## Arguments

- x:

  *A pointblank agent or a data table*

  `obj:<ptblank_agent>|obj:<tbl_*>` // **required**

  A data frame, tibble (`tbl_df` or `tbl_dbi`), Spark DataFrame
  (`tbl_spark`), or, an *agent* object of class `ptblank_agent` that is
  commonly created with
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md).

- columns:

  *The target columns*

  `<tidy-select>` // **required**

  A column-selecting expression, as one would use inside
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  Specifies the column (or a set of columns) to which this validation
  should be applied. See the *Column Names* section for more
  information.

- left:

  *Definition of left bound*

  `<value expression>` // **required**

  The lower bound for the range. The validation includes this bound
  value (if the first element in `inclusive` is `TRUE`) in addition to
  values greater than `left`. This can be a single value or a compatible
  column given in
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html).

- right:

  *Definition of right bound*

  `<value expression>` // **required**

  The upper bound for the range. The validation includes this bound
  value (if the second element in `inclusive` is `TRUE`) in addition to
  values lower than `right`. This can be a single value or a compatible
  column given in
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html).

- inclusive:

  *Inclusiveness of bounds*

  `vector<logical>` // *default:* `c(TRUE, TRUE)`

  A two-element logical value that indicates whether the `left` and
  `right` bounds should be inclusive. By default, both bounds are
  inclusive.

- na_pass:

  *Allow missing values to pass validation*

  `scalar<logical>` // *default:* `FALSE`

  Should any encountered `NA` values be considered as passing test
  units? By default, this is `FALSE`. Set to `TRUE` to give `NA`s a
  pass.

- preconditions:

  *Input table modification prior to validation*

  `<table mutation expression>` // *default:* `NULL` (`optional`)

  An optional expression for mutating the input table before proceeding
  with the validation. This can either be provided as a one-sided R
  formula using a leading `~` (e.g.,
  `~ . %>% dplyr::mutate(col = col + 10)` or as a function (e.g.,
  `function(x) dplyr::mutate(x, col = col + 10)`. See the
  *Preconditions* section for more information.

- segments:

  *Expressions for segmenting the target table*

  `<segmentation expressions>` // *default:* `NULL` (`optional`)

  An optional expression or set of expressions (held in a list) that
  serve to segment the target table by column values. Each expression
  can be given in one of two ways: (1) as column names, or (2) as a
  two-sided formula where the LHS holds a column name and the RHS
  contains the column values to segment on. See the *Segments* section
  for more details on this.

- actions:

  *Thresholds and actions for different states*

  `obj:<action_levels>` // *default:* `NULL` (`optional`)

  A list containing threshold levels so that the validation step can
  react accordingly when exceeding the set levels for different states.
  This is to be created with the
  [`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
  helper function.

- step_id:

  *Manual setting of the step ID value*

  `scalar<character>` // *default:* `NULL` (`optional`)

  One or more optional identifiers for the single or multiple validation
  steps generated from calling a validation function. The use of step
  IDs serves to distinguish validation steps from each other and provide
  an opportunity for supplying a more meaningful label compared to the
  step index. By default this is `NULL`, and **pointblank** will
  automatically generate the step ID value (based on the step index) in
  this case. One or more values can be provided, and the exact number of
  ID values should (1) match the number of validation steps that the
  validation function call will produce (influenced by the number of
  `columns` provided), (2) be an ID string not used in any previous
  validation step, and (3) be a vector with unique values.

- label:

  *Optional label for the validation step*

  `vector<character>` // *default:* `NULL` (`optional`)

  Optional label for the validation step. This label appears in the
  *agent* report and, for the best appearance, it should be kept quite
  short. See the *Labels* section for more information.

- brief:

  *Brief description for the validation step*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A *brief* is a short, text-based description for the validation step.
  If nothing is provided here then an *autobrief* is generated by the
  *agent*, using the language provided in
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)'s
  `lang` argument (which defaults to `"en"` or English). The *autobrief*
  incorporates details of the validation step so it's often the
  preferred option in most cases (where a `label` might be better suited
  to succinctly describe the validation).

- active:

  *Is the validation step active?*

  `scalar<logical>` // *default:* `TRUE`

  A logical value indicating whether the validation step should be
  active. If the validation function is working with an *agent*, `FALSE`
  will make the validation step inactive (still reporting its presence
  and keeping indexes for the steps unchanged). If the validation
  function will be operating directly on data (no *agent* involvement),
  then any step with `active = FALSE` will simply pass the data through
  with no validation whatsoever. Aside from a logical vector, a
  one-sided R formula using a leading `~` can be used with `.` (serving
  as the input data table) to evaluate to a single logical value. With
  this approach, the **pointblank** function
  [`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md)
  can be used to determine whether to make a validation step active on
  the basis of one or more columns existing in the table (e.g.,
  `~ . %>% has_columns(c(d, e))`).

- object:

  *A data table for expectations or tests*

  `obj:<tbl_*>` // **required**

  A data frame, tibble (`tbl_df` or `tbl_dbi`), or Spark DataFrame
  (`tbl_spark`) that serves as the target table for the expectation
  function or the test function.

- threshold:

  *The failure threshold*

  `scalar<integer|numeric>(val>=0)` // *default:* `1`

  A simple failure threshold value for use with the expectation
  (`expect_`) and the test (`test_`) function variants. By default, this
  is set to `1` meaning that any single unit of failure in data
  validation results in an overall test failure. Whole numbers beyond
  `1` indicate that any failing units up to that absolute threshold
  value will result in a succeeding **testthat** test or evaluate to
  `TRUE`. Likewise, fractional values (between `0` and `1`) act as a
  proportional failure threshold, where `0.15` means that 15 percent of
  failing test units results in an overall test failure.

## Value

For the validation function, the return value is either a
`ptblank_agent` object or a table object (depending on whether an agent
object or a table was passed to `x`). The expectation function invisibly
returns its input but, in the context of testing data, the function is
called primarily for its potential side-effects (e.g., signaling
failure). The test function returns a logical value.

## Supported Input Tables

The types of data tables that are officially supported are:

- data frames (`data.frame`) and tibbles (`tbl_df`)

- Spark DataFrames (`tbl_spark`)

- the following database tables (`tbl_dbi`):

  - *PostgreSQL* tables (using the
    [`RPostgres::Postgres()`](https://rpostgres.r-dbi.org/reference/Postgres.html)
    as driver)

  - *MySQL* tables (with
    [`RMySQL::MySQL()`](https://r-dbi.r-universe.dev/RMySQL/reference/MySQLDriver-class.html))

  - *Microsoft SQL Server* tables (via **odbc**)

  - *BigQuery* tables (using
    [`bigrquery::bigquery()`](https://bigrquery.r-dbi.org/reference/bigquery.html))

  - *DuckDB* tables (through
    [`duckdb::duckdb()`](https://r.duckdb.org/reference/duckdb.html))

  - *SQLite* (with
    [`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html))

Other database tables may work to varying degrees but they haven't been
formally tested (so be mindful of this when using unsupported backends
with **pointblank**).

## Column Names

`columns` may be a single column (as symbol `a` or string `"a"`) or a
vector of columns (`c(a, b, c)` or `c("a", "b", "c")`). `{tidyselect}`
helpers are also supported, such as `contains("date")` and
`where(is.double)`. If passing an *external vector* of columns, it
should be wrapped in `all_of()`.

When multiple columns are selected by `columns`, the result will be an
expansion of validation steps to that number of columns (e.g.,
`c(col_a, col_b)` will result in the entry of two validation steps).

Previously, columns could be specified in
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html). This
continues to work, but [`c()`](https://rdrr.io/r/base/c.html) offers the
same capability and supersedes
[`vars()`](https://dplyr.tidyverse.org/reference/vars.html) in
`columns`.

## Missing Values

This validation function supports special handling of `NA` values. The
`na_pass` argument will determine whether an `NA` value appearing in a
test unit should be counted as a *pass* or a *fail*. The default of
`na_pass = FALSE` means that any `NA`s encountered will accumulate
failing test units.

## Preconditions

Providing expressions as `preconditions` means **pointblank** will
preprocess the target table during interrogation as a preparatory step.
It might happen that a particular validation requires a calculated
column, some filtering of rows, or the addition of columns via a join,
etc. Especially for an *agent*-based report this can be advantageous
since we can develop a large validation plan with a single target table
and make minor adjustments to it, as needed, along the way.

The table mutation is totally isolated in scope to the validation
step(s) where `preconditions` is used. Using **dplyr** code is suggested
here since the statements can be translated to SQL if necessary (i.e.,
if the target table resides in a database). The code is most easily
supplied as a one-sided **R** formula (using a leading `~`). In the
formula representation, the `.` serves as the input data table to be
transformed (e.g., `~ . %>% dplyr::mutate(col_b = col_a + 10)`).
Alternatively, a function could instead be supplied (e.g.,
`function(x) dplyr::mutate(x, col_b = col_a + 10)`).

## Segments

By using the `segments` argument, it's possible to define a particular
validation with segments (or row slices) of the target table. An
optional expression or set of expressions that serve to segment the
target table by column values. Each expression can be given in one of
two ways: (1) as column names, or (2) as a two-sided formula where the
LHS holds a column name and the RHS contains the column values to
segment on.

As an example of the first type of expression that can be used,
`vars(a_column)` will segment the target table in however many unique
values are present in the column called `a_column`. This is great if
every unique value in a particular column (like different locations, or
different dates) requires it's own repeating validation.

With a formula, we can be more selective with which column values should
be used for segmentation. Using `a_column ~ c("group_1", "group_2")`
will attempt to obtain two segments where one is a slice of data where
the value `"group_1"` exists in the column named `"a_column"`, and, the
other is a slice where `"group_2"` exists in the same column. Each group
of rows resolved from the formula will result in a separate validation
step.

If there are multiple `columns` specified then the potential number of
validation steps will be `m` columns multiplied by `n` segments
resolved.

Segmentation will always occur after `preconditions` (i.e., statements
that mutate the target table), if any, are applied. With this type of
one-two combo, it's possible to generate labels for segmentation using
an expression for `preconditions` and refer to those labels in
`segments` without having to generate a separate version of the target
table.

## Actions

Often, we will want to specify `actions` for the validation. This
argument, present in every validation function, takes a
specially-crafted list object that is best produced by the
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
function. Read that function's documentation for the lowdown on how to
create reactions to above-threshold failure levels in validation. The
basic gist is that you'll want at least a single threshold level
(specified as either the fraction of test units failed, or, an absolute
value), often using the `warn_at` argument. This is especially true when
`x` is a table object because, otherwise, nothing happens. For the
`col_vals_*()`-type functions, using `action_levels(warn_at = 0.25)` or
`action_levels(stop_at = 0.25)` are good choices depending on the
situation (the first produces a warning when a quarter of the total test
units fails, the other [`stop()`](https://rdrr.io/r/base/stop.html)s at
the same threshold level).

## Labels

`label` may be a single string or a character vector that matches the
number of expanded steps. `label` also supports `{glue}` syntax and
exposes the following dynamic variables contextualized to the current
step:

- `"{.step}"`: The validation step name

- `"{.col}"`: The current column name

- `"{.seg_col}"`: The current segment's column name

- `"{.seg_val}"`: The current segment's value/group

The glue context also supports ordinary expressions for further
flexibility (e.g., `"{toupper(.step)}"`) as long as they return a
length-1 string.

## Briefs

Want to describe this validation step in some detail? Keep in mind that
this is only useful if `x` is an *agent*. If that's the case, `brief`
the agent with some text that fits. Don't worry if you don't want to do
it. The *autobrief* protocol is kicked in when `brief = NULL` and a
simple brief will then be automatically generated.

## YAML

A **pointblank** agent can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used to regenerate an agent (with
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md))
or interrogate the target table (via
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)).
When `col_vals_not_between()` is represented in YAML (under the
top-level `steps` key as a list member), the syntax closely follows the
signature of the validation function. Here is an example of how a
complex call of `col_vals_not_between()` as a validation step is
expressed in R code and in the corresponding YAML representation.

R statement:

    agent %>%
      col_vals_not_between(
        columns = a,
        left = 1,
        right = 2,
        inclusive = c(TRUE, FALSE),
        na_pass = TRUE,
        preconditions = ~ . %>% dplyr::filter(a < 10),
        segments = b ~ c("group_1", "group_2"),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2),
        label = "The `col_vals_not_between()` step.",
        active = FALSE
      )

YAML representation:

    steps:
    - col_vals_not_between:
        columns: c(a)
        left: 1.0
        right: 2.0
        inclusive:
        - true
        - false
        na_pass: true
        preconditions: ~. %>% dplyr::filter(a < 10)
        segments: b ~ c("group_1", "group_2")
        actions:
          warn_fraction: 0.1
          stop_fraction: 0.2
        label: The `col_vals_not_between()` step.
        active: false

In practice, both of these will often be shorter as only the `columns`,
`left`, and `right` arguments require values. Arguments with default
values won't be written to YAML when using
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
(though it is acceptable to include them with their default when
generating the YAML by other means). It is also possible to preview the
transformation of an agent to YAML without any writing to disk by using
the
[`yaml_agent_string()`](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md)
function.

## Examples

The `small_table` dataset in the package has a column of numeric values
in `c` (there are a few `NA`s in that column). The following examples
will validate the values in that numeric column.

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

### A: Using an `agent` with validation functions and then [`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)

Validate that values in column `c` are all between `10` and `20`.
Because there are `NA` values, we'll choose to let those pass validation
by setting `na_pass = TRUE`. We'll determine if this validation has any
failing test units (there are 13 test units, one for each row).

    agent <-
      create_agent(tbl = small_table) %>%
      col_vals_not_between(
        columns = c,
        left = 10, right = 20,
        na_pass = TRUE
      ) %>%
      interrogate()

Printing the `agent` in the console shows the validation report in the
Viewer. Here is an excerpt of validation report, showing the single
entry that corresponds to the validation step demonstrated here.

![This image was generated from the first code example in the
\`col_vals_not_between()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_col_vals_not_between_1.png)

### B: Using the validation function directly on the data (no `agent`)

This way of using validation functions acts as a data filter. Data is
passed through but should [`stop()`](https://rdrr.io/r/base/stop.html)
if there is a single test unit failing. The behavior of side effects can
be customized with the `actions` option.

    small_table %>%
      col_vals_not_between(
        columns = c,
        left = 10, right = 20,
        na_pass = TRUE
      ) %>%
      dplyr::pull(c)
    #>  [1]  3  8  3 NA  7  4  3  2  9  9  7  8 NA

### C: Using the expectation function

With the `expect_*()` form, we would typically perform one validation at
a time. This is primarily used in **testthat** tests.

    expect_col_vals_not_between(
      small_table, columns = c,
      left = 10, right = 20,
      na_pass = TRUE
    )

### D: Using the test function

With the `test_*()` form, we should get a single logical value returned
to us.

    small_table %>%
      test_col_vals_not_between(
        columns = c,
        left = 10, right = 20,
        na_pass = TRUE
      )
    #> [1] TRUE

An additional note on the bounds for this function: they are inclusive
by default. We can modify the inclusiveness of the upper and lower
bounds with the `inclusive` option, which is a length-2 logical vector.

In changing the lower bound to be `9` and making it non-inclusive, we
get `TRUE` since although two values are `9` and they fall outside of
the lower (or left) bound (and any values 'not between' count as passing
test units).

    small_table %>%
      test_col_vals_not_between(
        columns = c,
        left = 9, right = 20,
        inclusive = c(FALSE, TRUE),
        na_pass = TRUE
      )
    #> [1] TRUE

## Function ID

2-8

## See also

The analogue to this function:
[`col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md).

Other validation functions:
[`col_count_match()`](https://rstudio.github.io/pointblank/reference/col_count_match.md),
[`col_exists()`](https://rstudio.github.io/pointblank/reference/col_exists.md),
[`col_is_character()`](https://rstudio.github.io/pointblank/reference/col_is_character.md),
[`col_is_date()`](https://rstudio.github.io/pointblank/reference/col_is_date.md),
[`col_is_factor()`](https://rstudio.github.io/pointblank/reference/col_is_factor.md),
[`col_is_integer()`](https://rstudio.github.io/pointblank/reference/col_is_integer.md),
[`col_is_logical()`](https://rstudio.github.io/pointblank/reference/col_is_logical.md),
[`col_is_numeric()`](https://rstudio.github.io/pointblank/reference/col_is_numeric.md),
[`col_is_posix()`](https://rstudio.github.io/pointblank/reference/col_is_posix.md),
[`col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md),
[`col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md),
[`col_vals_decreasing()`](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md),
[`col_vals_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_equal.md),
[`col_vals_expr()`](https://rstudio.github.io/pointblank/reference/col_vals_expr.md),
[`col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md),
[`col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md),
[`col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md),
[`col_vals_increasing()`](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md),
[`col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md),
[`col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md),
[`col_vals_make_set()`](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md),
[`col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md),
[`col_vals_not_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md),
[`col_vals_not_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md),
[`col_vals_not_null()`](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md),
[`col_vals_null()`](https://rstudio.github.io/pointblank/reference/col_vals_null.md),
[`col_vals_regex()`](https://rstudio.github.io/pointblank/reference/col_vals_regex.md),
[`col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md),
[`conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md),
[`row_count_match()`](https://rstudio.github.io/pointblank/reference/row_count_match.md),
[`rows_complete()`](https://rstudio.github.io/pointblank/reference/rows_complete.md),
[`rows_distinct()`](https://rstudio.github.io/pointblank/reference/rows_distinct.md),
[`serially()`](https://rstudio.github.io/pointblank/reference/serially.md),
[`specially()`](https://rstudio.github.io/pointblank/reference/specially.md),
[`tbl_match()`](https://rstudio.github.io/pointblank/reference/tbl_match.md)
