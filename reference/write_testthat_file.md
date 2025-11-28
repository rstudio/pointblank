# Transform a **pointblank** agent to a **testthat** test file

With a **pointblank** *agent*, we can write a **testthat** test file and
opt to place it in the `testthat/tests` if it is available in the
project path (we can specify an alternate path as well). This works by
transforming the validation steps to a series of `expect_*()` calls
inside individual
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
statements.

A major requirement for using `write_testthat_file()` on an agent is the
presence of an expression that can retrieve the target table. Typically,
we might supply a table-prep formula, which is a formula that can be
invoked to obtain the target table (e.g.,
`tbl = ~ pointblank::small_table`). This user-supplied statement will be
used by `write_testthat_file()` to generate a table-loading statement at
the top of the new **testthat** test file so that the target table is
available for each of the
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
statements that follow. If an *agent* was not created using a table-prep
formula set for the `tbl`, it can be modified via the
[`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md)
function.

Thresholds will be obtained from those applied for the `stop` state.
This can be set up for a **pointblank** *agent* by passing an
`action_levels` object to the `actions` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
or the same argument of any included validation function. If `stop`
thresholds are not available, then a threshold value of `1` will be used
for each generated `expect_*()` statement in the resulting **testthat**
test file.

There is no requirement that the **agent** first undergo interrogation
with
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md).
However, it may be useful as a dry run to interactively perform an
interrogation on the target data before generating the **testthat** test
file.

## Usage

``` r
write_testthat_file(
  agent,
  name = NULL,
  path = NULL,
  overwrite = FALSE,
  skips = NULL,
  quiet = FALSE
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

- name:

  *Name for generated testthat file*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional name for for the **testhat** test file. This should be a
  name without extension and without the leading `"test-"` text. If
  nothing is supplied, the name will be derived from the `tbl_name` in
  the agent. If that's not present, a generic name will be used.

- path:

  *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A path can be specified here if there shouldn't be an attempt to place
  the file in `testthat/tests`.

- overwrite:

  *Overwrite a previous file of the same name*

  `scalar<logical>` // *default:* `FALSE`

  Should a **testthat** file of the same name be overwritten?

- skips:

  *Test skipping*

  `vector<character>` // *default:* `NULL` (`optional`)

  This is an optional vector of test-skipping keywords modeled after the
  **testthat** `skip_on_*()` functions. The following keywords can be
  used to include `skip_on_*()` statements: `"cran"`
  ([`testthat::skip_on_cran()`](https://testthat.r-lib.org/reference/skip.html)),
  `"travis"`
  ([`testthat::skip_on_travis()`](https://testthat.r-lib.org/reference/skip_on_travis.html)),
  `"appveyor"`
  ([`testthat::skip_on_appveyor()`](https://testthat.r-lib.org/reference/skip_on_travis.html)),
  `"ci"`
  ([`testthat::skip_on_ci()`](https://testthat.r-lib.org/reference/skip.html)),
  `"covr"`
  ([`testthat::skip_on_covr()`](https://testthat.r-lib.org/reference/skip.html)),
  `"bioc"`
  ([`testthat::skip_on_bioc()`](https://testthat.r-lib.org/reference/skip.html)).
  There are keywords for skipping tests on certain operating systems and
  all of them will insert a specific
  [`testthat::skip_on_os()`](https://testthat.r-lib.org/reference/skip.html)
  call. These are `"windows"` (`skip_on_os("windows")`), `"mac"`
  (`skip_on_os("mac")`), `"linux"` (`skip_on_os("linux")`), and
  `"solaris"` (`skip_on_os("solaris")`). These calls will be placed at
  the top of the generated **testthat** test file.

- quiet:

  *Inform (or not) upon file writing*

  `scalar<logical>` // *default:* `FALSE`

  Should the function *not* inform when the file is written?

## Value

Invisibly returns `TRUE` if the **testthat** file has been written.

## Details

Tests for inactive validation steps will be skipped with a clear message
indicating that the reason for skipping was due to the test not being
active. Any inactive validation steps can be forced into an active state
by using the
[`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md)
on an *agent* (the opposite is possible with the
[`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md)
function).

The **testthat** package comes with a series of `skip_on_*()` functions
which conveniently cause the test file to be skipped entirely if certain
conditions are met. We can quickly set any number of these at the top of
the **testthat** test file by supplying keywords as a vector to the
`skips` option of `write_testthat_file()`. For instance, setting
`skips = c("cran", "windows)` will add the **testthat** `skip_on_cran()`
and `skip_on_os("windows")` statements, meaning that the generated test
file won't run on a CRAN system or if the system OS is Windows.

Here is an example of **testthat** test file output
(`"test-small_table.R"`):

    # Generated by pointblank

    tbl <- small_table

    test_that("column `date_time` exists", {

      expect_col_exists(
        tbl,
        columns = date_time,
        threshold = 1
      )
    })

    test_that("values in `c` should be <= `5`", {

      expect_col_vals_lte(
        tbl,
        columns = c,
        value = 5,
        threshold = 0.25
      )
    })

This was generated by the following set of R statements:

    library(pointblank)

    agent <-
      create_agent(
        tbl = ~ small_table,
        actions = action_levels(stop_at = 0.25)
      ) %>%
      col_exists(date_time) %>%
      col_vals_lte(c, value = 5)

    write_testthat_file(
      agent = agent,
      name = "small_table",
      path = "."
    )

## Examples

### Creating a **testthat** file from an *agent*

Let's walk through a data quality analysis of an extremely small table.
It's actually called `small_table` and we can find it as a dataset in
this package.

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

Creating an `action_levels` object is a common workflow step when
creating a pointblank agent. We designate failure thresholds to the
`warn`, `stop`, and `notify` states using
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md).

    al <-
      action_levels(
        warn_at = 0.10,
        stop_at = 0.25,
        notify_at = 0.35
      )

A pointblank `agent` object is now created and the `al` object is
provided to the agent. The static thresholds provided by the `al` object
make reports a bit more useful after interrogation.

    agent <-
      create_agent(
        tbl = ~ small_table,
        label = "An example.",
        actions = al
      ) %>%
      col_exists(c(date, date_time)) %>%
      col_vals_regex(
        b,
        regex = "[0-9]-[a-z]{3}-[0-9]{3}"
      ) %>%
      col_vals_gt(d, value = 100) %>%
      col_vals_lte(c, value = 5) %>%
      interrogate()

This agent and all of the checks can be transformed into a testthat file
with `write_testthat_file()`. The `stop` thresholds will be ported over
to the `expect_*()` functions in the new file.

    write_testthat_file(
      agent = agent,
      name = "small_table",
      path = "."
    )

The above code will generate a file with the name
`"test-small_table.R"`. The path was specified with `"."` so the file
will be placed in the working directory. If you'd like to easily add
this new file to the `tests/testthat` directory then `path = NULL` (the
default) will makes this possible (this is useful during package
development).

What's in the new file? This:

    # Generated by pointblank

    tbl <- small_table

    test_that("column `date` exists", {

      expect_col_exists(
        tbl,
        columns = date,
        threshold = 1
      )
    })

    test_that("column `date_time` exists", {

      expect_col_exists(
        tbl,
        columns = date_time,
        threshold = 1
      )
    })

    test_that("values in `b` should match the regular expression:
    `[0-9]-[a-z]{3}-[0-9]{3}`", {

      expect_col_vals_regex(
        tbl,
        columns = b,
        regex = "[0-9]-[a-z]{3}-[0-9]{3}",
        threshold = 0.25
      )
    })

    test_that("values in `d` should be > `100`", {

      expect_col_vals_gt(
        tbl,
        columns = d,
        value = 100,
        threshold = 0.25
      )
    })

    test_that("values in `c` should be <= `5`", {

      expect_col_vals_lte(
        tbl,
        columns = c,
        value = 5,
        threshold = 0.25
      )
    })

### Using an *agent* stored on disk as a YAML file

An agent on disk as a YAML file can be made into a **testthat** file.
The `"agent-small_table.yml"` file is available in the **pointblank**
package and the path can be obtained with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

    yml_file <-
      system.file(
        "yaml", "agent-small_table.yml",
        package = "pointblank"
      )

Writing the **testthat** file into the working directory is much the
same as before but we're reading the agent from disk this time. It's a
read and write combo, really. Again, we are choosing to write the file
to the working directory by using `path = "."`.

    write_testthat_file(
      agent = yaml_read_agent(yml_file),
      name = "from_agent_yaml",
      path = "."
    )

## Function ID

8-5

## See also

Other Post-interrogation:
[`all_passed()`](https://rstudio.github.io/pointblank/reference/all_passed.md),
[`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md),
[`get_data_extracts()`](https://rstudio.github.io/pointblank/reference/get_data_extracts.md),
[`get_sundered_data()`](https://rstudio.github.io/pointblank/reference/get_sundered_data.md)
