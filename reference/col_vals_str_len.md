# Are string lengths in column data within a specified range?

The `col_vals_str_len()` validation function, the
`expect_col_vals_str_len()` expectation function, and the
`test_col_vals_str_len()` test function all check whether string lengths
of column values in a table fall within a specified range defined by
`min` and `max`. The validation function can be used directly on a data
table or with an *agent* object (technically, a `ptblank_agent` object)
whereas the expectation and test functions can only be used with a data
table. Each validation step or expectation will operate over the number
of test units that is equal to the number of rows in the table (after
any `preconditions` have been applied).

## Usage

``` r
col_vals_str_len(
  x,
  columns,
  min = NULL,
  max = NULL,
  na_pass = FALSE,
  preconditions = NULL,
  segments = NULL,
  actions = NULL,
  step_id = NULL,
  label = NULL,
  brief = NULL,
  active = TRUE
)

expect_col_vals_str_len(
  object,
  columns,
  min = NULL,
  max = NULL,
  na_pass = FALSE,
  preconditions = NULL,
  threshold = 1
)

test_col_vals_str_len(
  object,
  columns,
  min = NULL,
  max = NULL,
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

- min:

  *Minimum string length*

  `scalar<integer>` // *default:* `NULL` (`optional`)

  The minimum acceptable string length (inclusive). If `NULL`, no lower
  bound is applied. At least one of `min` or `max` must be provided.

- max:

  *Maximum string length*

  `scalar<integer>` // *default:* `NULL` (`optional`)

  The maximum acceptable string length (inclusive). If `NULL`, no upper
  bound is applied. At least one of `min` or `max` must be provided.

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

## Examples

Create a simple table with a character column.

    tbl <-
      dplyr::tibble(
        id = c("AB", "CDE", "FGHI", "JK"),
        value = c(1, 2, 3, 4)
      )

    tbl
    #> # A tibble: 4 x 2
    #>   id    value
    #>   <chr> <dbl>
    #> 1 AB        1
    #> 2 CDE       2
    #> 3 FGHI      3
    #> 4 JK        4

Validate that string lengths in column `id` are between 2 and 4
characters.

    agent <-
      create_agent(tbl = tbl) %>%
      col_vals_str_len(columns = id, min = 2, max = 4) %>%
      interrogate()

Determine if this validation step passed by using
[`all_passed()`](https://rstudio.github.io/pointblank/reference/all_passed.md).

    all_passed(agent)

    ## [1] TRUE

## Function ID

2-18
