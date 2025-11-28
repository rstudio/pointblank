# Package index

## Planning and Prep

Should you need to understand your data a bit more, use the
[`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md)
function. It provides a comprehensive report on the data you might be
planning on validating. If going down the road of preparing a data
quality analysis, call on an *agent* to carry out that eventual plan
with
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md).
The agent here is to be told which table is the target, and, we devise a
stepwise validation plan with validation functions. If collecting and
publishing table information is your goal, then begin with the
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
function.

- [![](icons/scan_data.png)](https://rstudio.github.io/pointblank/reference/scan_data.md)
  [`scan_data()`](https://rstudio.github.io/pointblank/reference/scan_data.md)
  : Thoroughly scan a table to better understand it

- [![](icons/create_agent.png)](https://rstudio.github.io/pointblank/reference/create_agent.md)
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
  :

  Create a **pointblank** *agent* object

- [![](icons/create_informant.png)](https://rstudio.github.io/pointblank/reference/create_informant.md)
  [`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
  :

  Create a **pointblank** *informant* object

- [![](icons/validate_rmd.png)](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
  [`validate_rmd()`](https://rstudio.github.io/pointblank/reference/validate_rmd.md)
  :

  Perform **pointblank** validation testing within R Markdown documents

- [![](icons/action_levels.png)](https://rstudio.github.io/pointblank/reference/action_levels.md)
  [`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
  [`warn_on_fail()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
  [`stop_on_fail()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
  : Set action levels: failure thresholds and functions to invoke

- [![](icons/db_tbl.png)](https://rstudio.github.io/pointblank/reference/db_tbl.md)
  [`db_tbl()`](https://rstudio.github.io/pointblank/reference/db_tbl.md)
  : Get a table from a database

- [![](icons/file_tbl.png)](https://rstudio.github.io/pointblank/reference/file_tbl.md)
  [`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md)
  : Get a table from a local or remote file

- [![](icons/tbl_store.png)](https://rstudio.github.io/pointblank/reference/tbl_store.md)
  [`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
  : Define a store of tables with table-prep formulas: a table store

- [![](icons/tbl_source.png)](https://rstudio.github.io/pointblank/reference/tbl_source.md)
  [`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
  : Obtain a table-prep formula from a table store

- [![](icons/tbl_get.png)](https://rstudio.github.io/pointblank/reference/tbl_get.md)
  [`tbl_get()`](https://rstudio.github.io/pointblank/reference/tbl_get.md)
  : Obtain a materialized table via a table store

- [![](icons/draft_validation.png)](https://rstudio.github.io/pointblank/reference/draft_validation.md)
  [`draft_validation()`](https://rstudio.github.io/pointblank/reference/draft_validation.md)
  :

  Draft a starter **pointblank** validation .R/.Rmd file with a data
  table

## Validation, Expectation, and Test Functions

Validation functions are either used with an *agent* object or, more
simply, just with the table of interest. When used with an *agent*, each
validation works to build up a validation plan (which is executed with
the
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
function). If one or more validation functions are used directly on data
(that is, no agent is involved whatsoever), then the data is checked and
passed through if there are no problems. We can fine tune the `warn_*`
and/or `stop_*` thresholds and so that if the level of failed validation
units exceeds those set levels, then we’ll get either a warning or an
error. Each validation function is associated with an expectation
function (of the form `expect_*()`). These expectation functions are
equivalent in usage and behavior to those in the **testthat** package.
Finally, each validation function has an associated test function (of
the form `test_*()`) that always returns a logical value.

- [![](icons/col_vals_lt.png)](https://rstudio.github.io/pointblank/reference/col_vals_lt.md)
  [`col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md)
  [`expect_col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md)
  [`test_col_vals_lt()`](https://rstudio.github.io/pointblank/reference/col_vals_lt.md)
  : Are column data less than a fixed value or data in another column?

- [![](icons/col_vals_lte.png)](https://rstudio.github.io/pointblank/reference/col_vals_lte.md)
  [`col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md)
  [`expect_col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md)
  [`test_col_vals_lte()`](https://rstudio.github.io/pointblank/reference/col_vals_lte.md)
  : Are column data less than or equal to a fixed value or data in
  another column?

- [![](icons/col_vals_equal.png)](https://rstudio.github.io/pointblank/reference/col_vals_equal.md)
  [`col_vals_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_equal.md)
  [`expect_col_vals_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_equal.md)
  [`test_col_vals_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_equal.md)
  : Are column data equal to a fixed value or data in another column?

- [![](icons/col_vals_not_equal.png)](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md)
  [`col_vals_not_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md)
  [`expect_col_vals_not_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md)
  [`test_col_vals_not_equal()`](https://rstudio.github.io/pointblank/reference/col_vals_not_equal.md)
  : Are column data not equal to a fixed value or data in another
  column?

- [![](icons/col_vals_gte.png)](https://rstudio.github.io/pointblank/reference/col_vals_gte.md)
  [`col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md)
  [`expect_col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md)
  [`test_col_vals_gte()`](https://rstudio.github.io/pointblank/reference/col_vals_gte.md)
  : Are column data greater than or equal to a fixed value or data in
  another column?

- [![](icons/col_vals_gt.png)](https://rstudio.github.io/pointblank/reference/col_vals_gt.md)
  [`col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md)
  [`expect_col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md)
  [`test_col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md)
  : Are column data greater than a fixed value or data in another
  column?

- [![](icons/col_vals_between.png)](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
  [`col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
  [`expect_col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
  [`test_col_vals_between()`](https://rstudio.github.io/pointblank/reference/col_vals_between.md)
  : Do column data lie between two specified values or data in other
  columns?

- [![](icons/col_vals_not_between.png)](https://rstudio.github.io/pointblank/reference/col_vals_not_between.md)
  [`col_vals_not_between()`](https://rstudio.github.io/pointblank/reference/col_vals_not_between.md)
  [`expect_col_vals_not_between()`](https://rstudio.github.io/pointblank/reference/col_vals_not_between.md)
  [`test_col_vals_not_between()`](https://rstudio.github.io/pointblank/reference/col_vals_not_between.md)
  : Do column data lie outside of two specified values or data in other
  columns?

- [![](icons/col_vals_in_set.png)](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md)
  [`col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md)
  [`expect_col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md)
  [`test_col_vals_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_in_set.md)
  : Are column data part of a specified set of values?

- [![](icons/col_vals_not_in_set.png)](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md)
  [`col_vals_not_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md)
  [`expect_col_vals_not_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md)
  [`test_col_vals_not_in_set()`](https://rstudio.github.io/pointblank/reference/col_vals_not_in_set.md)
  : Are data not part of a specified set of values?

- [![](icons/col_vals_make_set.png)](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md)
  [`col_vals_make_set()`](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md)
  [`expect_col_vals_make_set()`](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md)
  [`test_col_vals_make_set()`](https://rstudio.github.io/pointblank/reference/col_vals_make_set.md)
  : Is a set of values entirely accounted for in a column of values?

- [![](icons/col_vals_make_subset.png)](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md)
  [`col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md)
  [`expect_col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md)
  [`test_col_vals_make_subset()`](https://rstudio.github.io/pointblank/reference/col_vals_make_subset.md)
  : Is a set of values a subset of a column of values?

- [![](icons/col_vals_increasing.png)](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md)
  [`col_vals_increasing()`](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md)
  [`expect_col_vals_increasing()`](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md)
  [`test_col_vals_increasing()`](https://rstudio.github.io/pointblank/reference/col_vals_increasing.md)
  : Are column data increasing by row?

- [![](icons/col_vals_decreasing.png)](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md)
  [`col_vals_decreasing()`](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md)
  [`expect_col_vals_decreasing()`](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md)
  [`test_col_vals_decreasing()`](https://rstudio.github.io/pointblank/reference/col_vals_decreasing.md)
  : Are column data decreasing by row?

- [![](icons/col_vals_null.png)](https://rstudio.github.io/pointblank/reference/col_vals_null.md)
  [`col_vals_null()`](https://rstudio.github.io/pointblank/reference/col_vals_null.md)
  [`expect_col_vals_null()`](https://rstudio.github.io/pointblank/reference/col_vals_null.md)
  [`test_col_vals_null()`](https://rstudio.github.io/pointblank/reference/col_vals_null.md)
  :

  Are column data `NULL`/`NA`?

- [![](icons/col_vals_not_null.png)](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md)
  [`col_vals_not_null()`](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md)
  [`expect_col_vals_not_null()`](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md)
  [`test_col_vals_not_null()`](https://rstudio.github.io/pointblank/reference/col_vals_not_null.md)
  :

  Are column data not `NULL`/`NA`?

- [![](icons/col_vals_regex.png)](https://rstudio.github.io/pointblank/reference/col_vals_regex.md)
  [`col_vals_regex()`](https://rstudio.github.io/pointblank/reference/col_vals_regex.md)
  [`expect_col_vals_regex()`](https://rstudio.github.io/pointblank/reference/col_vals_regex.md)
  [`test_col_vals_regex()`](https://rstudio.github.io/pointblank/reference/col_vals_regex.md)
  : Do strings in column data match a regex pattern?

- [![](icons/col_vals_within_spec.png)](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md)
  [`col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md)
  [`expect_col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md)
  [`test_col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md)
  : Do values in column data fit within a specification?

- [![](icons/col_vals_expr.png)](https://rstudio.github.io/pointblank/reference/col_vals_expr.md)
  [`col_vals_expr()`](https://rstudio.github.io/pointblank/reference/col_vals_expr.md)
  [`expect_col_vals_expr()`](https://rstudio.github.io/pointblank/reference/col_vals_expr.md)
  [`test_col_vals_expr()`](https://rstudio.github.io/pointblank/reference/col_vals_expr.md)
  : Do column data agree with a predicate expression?

- [![](icons/rows_distinct.png)](https://rstudio.github.io/pointblank/reference/rows_distinct.md)
  [`rows_distinct()`](https://rstudio.github.io/pointblank/reference/rows_distinct.md)
  [`expect_rows_distinct()`](https://rstudio.github.io/pointblank/reference/rows_distinct.md)
  [`test_rows_distinct()`](https://rstudio.github.io/pointblank/reference/rows_distinct.md)
  : Are row data distinct?

- [![](icons/rows_complete.png)](https://rstudio.github.io/pointblank/reference/rows_complete.md)
  [`rows_complete()`](https://rstudio.github.io/pointblank/reference/rows_complete.md)
  [`expect_rows_complete()`](https://rstudio.github.io/pointblank/reference/rows_complete.md)
  [`test_rows_complete()`](https://rstudio.github.io/pointblank/reference/rows_complete.md)
  : Are row data complete?

- [![](icons/col_is_character.png)](https://rstudio.github.io/pointblank/reference/col_is_character.md)
  [`col_is_character()`](https://rstudio.github.io/pointblank/reference/col_is_character.md)
  [`expect_col_is_character()`](https://rstudio.github.io/pointblank/reference/col_is_character.md)
  [`test_col_is_character()`](https://rstudio.github.io/pointblank/reference/col_is_character.md)
  : Do the columns contain character/string data?

- [![](icons/col_is_numeric.png)](https://rstudio.github.io/pointblank/reference/col_is_numeric.md)
  [`col_is_numeric()`](https://rstudio.github.io/pointblank/reference/col_is_numeric.md)
  [`expect_col_is_numeric()`](https://rstudio.github.io/pointblank/reference/col_is_numeric.md)
  [`test_col_is_numeric()`](https://rstudio.github.io/pointblank/reference/col_is_numeric.md)
  : Do the columns contain numeric values?

- [![](icons/col_is_integer.png)](https://rstudio.github.io/pointblank/reference/col_is_integer.md)
  [`col_is_integer()`](https://rstudio.github.io/pointblank/reference/col_is_integer.md)
  [`expect_col_is_integer()`](https://rstudio.github.io/pointblank/reference/col_is_integer.md)
  [`test_col_is_integer()`](https://rstudio.github.io/pointblank/reference/col_is_integer.md)
  : Do the columns contain integer values?

- [![](icons/col_is_logical.png)](https://rstudio.github.io/pointblank/reference/col_is_logical.md)
  [`col_is_logical()`](https://rstudio.github.io/pointblank/reference/col_is_logical.md)
  [`expect_col_is_logical()`](https://rstudio.github.io/pointblank/reference/col_is_logical.md)
  [`test_col_is_logical()`](https://rstudio.github.io/pointblank/reference/col_is_logical.md)
  : Do the columns contain logical values?

- [![](icons/col_is_date.png)](https://rstudio.github.io/pointblank/reference/col_is_date.md)
  [`col_is_date()`](https://rstudio.github.io/pointblank/reference/col_is_date.md)
  [`expect_col_is_date()`](https://rstudio.github.io/pointblank/reference/col_is_date.md)
  [`test_col_is_date()`](https://rstudio.github.io/pointblank/reference/col_is_date.md)
  :

  Do the columns contain R `Date` objects?

- [![](icons/col_is_posix.png)](https://rstudio.github.io/pointblank/reference/col_is_posix.md)
  [`col_is_posix()`](https://rstudio.github.io/pointblank/reference/col_is_posix.md)
  [`expect_col_is_posix()`](https://rstudio.github.io/pointblank/reference/col_is_posix.md)
  [`test_col_is_posix()`](https://rstudio.github.io/pointblank/reference/col_is_posix.md)
  :

  Do the columns contain `POSIXct` dates?

- [![](icons/col_is_factor.png)](https://rstudio.github.io/pointblank/reference/col_is_factor.md)
  [`col_is_factor()`](https://rstudio.github.io/pointblank/reference/col_is_factor.md)
  [`expect_col_is_factor()`](https://rstudio.github.io/pointblank/reference/col_is_factor.md)
  [`test_col_is_factor()`](https://rstudio.github.io/pointblank/reference/col_is_factor.md)
  :

  Do the columns contain R `factor` objects?

- [![](icons/col_exists.png)](https://rstudio.github.io/pointblank/reference/col_exists.md)
  [`col_exists()`](https://rstudio.github.io/pointblank/reference/col_exists.md)
  [`expect_col_exists()`](https://rstudio.github.io/pointblank/reference/col_exists.md)
  [`test_col_exists()`](https://rstudio.github.io/pointblank/reference/col_exists.md)
  : Do one or more columns actually exist?

- [![](icons/col_schema_match.png)](https://rstudio.github.io/pointblank/reference/col_schema_match.md)
  [`col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md)
  [`expect_col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md)
  [`test_col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md)
  : Do columns in the table (and their types) match a predefined schema?

- [![](icons/row_count_match.png)](https://rstudio.github.io/pointblank/reference/row_count_match.md)
  [`row_count_match()`](https://rstudio.github.io/pointblank/reference/row_count_match.md)
  [`expect_row_count_match()`](https://rstudio.github.io/pointblank/reference/row_count_match.md)
  [`test_row_count_match()`](https://rstudio.github.io/pointblank/reference/row_count_match.md)
  : Does the row count match that of a different table?

- [![](icons/col_count_match.png)](https://rstudio.github.io/pointblank/reference/col_count_match.md)
  [`col_count_match()`](https://rstudio.github.io/pointblank/reference/col_count_match.md)
  [`expect_col_count_match()`](https://rstudio.github.io/pointblank/reference/col_count_match.md)
  [`test_col_count_match()`](https://rstudio.github.io/pointblank/reference/col_count_match.md)
  : Does the column count match that of a different table?

- [![](icons/tbl_match.png)](https://rstudio.github.io/pointblank/reference/tbl_match.md)
  [`tbl_match()`](https://rstudio.github.io/pointblank/reference/tbl_match.md)
  [`expect_tbl_match()`](https://rstudio.github.io/pointblank/reference/tbl_match.md)
  [`test_tbl_match()`](https://rstudio.github.io/pointblank/reference/tbl_match.md)
  : Does the target table match a comparison table?

- [![](icons/conjointly.png)](https://rstudio.github.io/pointblank/reference/conjointly.md)
  [`conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md)
  [`expect_conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md)
  [`test_conjointly()`](https://rstudio.github.io/pointblank/reference/conjointly.md)
  : Perform multiple rowwise validations for joint validity

- [![](icons/serially.png)](https://rstudio.github.io/pointblank/reference/serially.md)
  [`serially()`](https://rstudio.github.io/pointblank/reference/serially.md)
  [`expect_serially()`](https://rstudio.github.io/pointblank/reference/serially.md)
  [`test_serially()`](https://rstudio.github.io/pointblank/reference/serially.md)
  : Run several tests and a final validation in a serial manner

- [![](icons/specially.png)](https://rstudio.github.io/pointblank/reference/specially.md)
  [`specially()`](https://rstudio.github.io/pointblank/reference/specially.md)
  [`expect_specially()`](https://rstudio.github.io/pointblank/reference/specially.md)
  [`test_specially()`](https://rstudio.github.io/pointblank/reference/specially.md)
  : Perform a specialized validation with a user-defined function

## Information Functions

We can progressively add information to an *informant* object by using
the collection of `info_*()` functions. We can add more table-based
properties with the
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md)
function, details about the nature of each column with the
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
function, and add sections of our own choosing (and the info that make
sense for those sections) with the
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md)
function. Snippets of information can be gleaned from the target table
by using the
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
function. These bits of information can be incorporated in text defined
by the other `info_*()` functions via
[`{ }`](https://rdrr.io/r/base/Paren.html). Some great `snip_*()`
functions are included to make info-snipping as easy (and useful!) as
can be.

- [![](icons/info_tabular.png)](https://rstudio.github.io/pointblank/reference/info_tabular.md)
  [`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md)
  : Add information that focuses on aspects of the data table as a whole

- [![](icons/info_columns.png)](https://rstudio.github.io/pointblank/reference/info_columns.md)
  [`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
  : Add information that focuses on aspects of a data table's columns

- [![](icons/info_columns_from_tbl.png)](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md)
  [`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md)
  : Add column information from another data table

- [![](icons/info_section.png)](https://rstudio.github.io/pointblank/reference/info_section.md)
  [`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md)
  : Add information that focuses on some key aspect of the data table

- [![](icons/info_snippet.png)](https://rstudio.github.io/pointblank/reference/info_snippet.md)
  [`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
  : Generate a useful text 'snippet' from the target table

- [`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md)
  :

  A `fn` for
  [`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md):
  get a list of column categories

- [`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md)
  :

  A `fn` for
  [`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md):
  get an inline statistical summary

- [`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md)
  :

  A `fn` for
  [`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md):
  get the lowest value from a column

- [`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md)
  :

  A `fn` for
  [`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md):
  get the highest value from a column

## Emailing

Sometimes we want to email a report of a validation because of the
importance of the information contained therein. The
[`email_blast()`](https://rstudio.github.io/pointblank/reference/email_blast.md)
function can be used within the `end_fns` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md),
giving us options to send a customizable message only if specified
conditions are met.

- [![](icons/email_blast.png)](https://rstudio.github.io/pointblank/reference/email_blast.md)
  [`email_blast()`](https://rstudio.github.io/pointblank/reference/email_blast.md)
  : Conditionally send email during interrogation

- [![](icons/email_create.png)](https://rstudio.github.io/pointblank/reference/email_create.md)
  [`email_create()`](https://rstudio.github.io/pointblank/reference/email_create.md)
  :

  Create an email object from a **pointblank** *agent*

- [![](icons/stock_msg_body.png)](https://rstudio.github.io/pointblank/reference/stock_msg_body.md)
  [`stock_msg_body()`](https://rstudio.github.io/pointblank/reference/stock_msg_body.md)
  : Provide simple email message body components: body

- [![](icons/stock_msg_footer.png)](https://rstudio.github.io/pointblank/reference/stock_msg_footer.md)
  [`stock_msg_footer()`](https://rstudio.github.io/pointblank/reference/stock_msg_footer.md)
  : Provide simple email message body components: footer

## Logging

Logging validation failure conditions makes for a good practice during
data quality analysis. The
[`log4r_step()`](https://rstudio.github.io/pointblank/reference/log4r_step.md)
function allows for simple generation of log entries and specification
of logging destinations.

- [![](icons/log4r_step.png)](https://rstudio.github.io/pointblank/reference/log4r_step.md)
  [`log4r_step()`](https://rstudio.github.io/pointblank/reference/log4r_step.md)
  : Enable logging of failure conditions at the validation step level

## Agent: Interrogate and Report

If we have an *agent* object that has a plan (i.e., validation steps),
the
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
function instructs the *agent* to interrogate the target table. The
agent will go to work and also perform specified side-effect functions
at the step level and upon completion (if those are functions are
defined). After interrogation, we can get a report through printing,
however, we can take advantage of more options by using the
[`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md)
function.

- [![](icons/interrogate.png)](https://rstudio.github.io/pointblank/reference/interrogate.md)
  [`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
  : Given an agent that has a validation plan, perform an interrogation
- [![](icons/get_agent_report.png)](https://rstudio.github.io/pointblank/reference/get_agent_report.md)
  [`get_agent_report()`](https://rstudio.github.io/pointblank/reference/get_agent_report.md)
  : Get a summary report from an agent

## Informant: Incorporate and Report

If we have an *informant* object that has been loaded with information
from using the `info_*()` functions, the
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
function works to regenerate snippets and integrate those into the info
text. After refreshing the table information, we can get an information
report through printing, or, by using
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
function.

- [![](icons/incorporate.png)](https://rstudio.github.io/pointblank/reference/incorporate.md)
  [`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
  :

  Given an *informant* object, update and incorporate table snippets

- [![](icons/get_informant_report.png)](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
  [`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
  :

  Get a table information report from an *informant* object

## Post-interrogation

The agent always has a special list called an x-list. Access that by
invoking the
[`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md)
and you’ll then have a smorgasbord of information about how the
validation went down. Table extracts are collected by default for failed
rows (up to a limit) and we can access those with
[`get_data_extracts()`](https://rstudio.github.io/pointblank/reference/get_data_extracts.md).
Table rows can be sundered into ‘pass’ and ‘fail’ pieces. Access those
table fragments with the
[`get_sundered_data()`](https://rstudio.github.io/pointblank/reference/get_sundered_data.md)
function. Want to know if all validation steps have passed with flying
colors? Sometimes that could happen; use the
[`all_passed()`](https://rstudio.github.io/pointblank/reference/all_passed.md)
function to find out.

- [![](icons/get_agent_x_list.png)](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md)
  [`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md)
  :

  Get the agent's **x-list**

- [![](icons/get_data_extracts.png)](https://rstudio.github.io/pointblank/reference/get_data_extracts.md)
  [`get_data_extracts()`](https://rstudio.github.io/pointblank/reference/get_data_extracts.md)
  : Collect data extracts from a validation step

- [![](icons/get_sundered_data.png)](https://rstudio.github.io/pointblank/reference/get_sundered_data.md)
  [`get_sundered_data()`](https://rstudio.github.io/pointblank/reference/get_sundered_data.md)
  : Sunder the data, splitting it into 'pass' and 'fail' pieces

- [![](icons/all_passed.png)](https://rstudio.github.io/pointblank/reference/all_passed.md)
  [`all_passed()`](https://rstudio.github.io/pointblank/reference/all_passed.md)
  :

  Did all of the validations fully *pass*?

- [![](icons/write_testthat_file.png)](https://rstudio.github.io/pointblank/reference/write_testthat_file.md)
  [`write_testthat_file()`](https://rstudio.github.io/pointblank/reference/write_testthat_file.md)
  :

  Transform a **pointblank** agent to a **testthat** test file

## Object Ops

We have options for writing an agent or informant to disk with the
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
function. The on-disk object can be retrieved with the
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
function. You can export a report as an HTML file with
[`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md).
A set of functions are also available for setting a data table to an
existing object, and, for editing an agent’s validation steps.

- [![](icons/x_write_disk.png)](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
  [`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
  :

  Write an *agent*, *informant*, *multiagent*, or table scan to disk

- [![](icons/x_read_disk.png)](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
  [`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
  :

  Read an *agent*, *informant*, *multiagent*, or table scan from disk

- [![](icons/export_report.png)](https://rstudio.github.io/pointblank/reference/export_report.md)
  [`export_report()`](https://rstudio.github.io/pointblank/reference/export_report.md)
  :

  Export an *agent*, *informant*, *multiagent*, or table scan to HTML

- [![](icons/set_tbl.png)](https://rstudio.github.io/pointblank/reference/set_tbl.md)
  [`set_tbl()`](https://rstudio.github.io/pointblank/reference/set_tbl.md)
  :

  Set a data table to an *agent* or an *informant*

- [![](icons/activate_steps.png)](https://rstudio.github.io/pointblank/reference/activate_steps.md)
  [`activate_steps()`](https://rstudio.github.io/pointblank/reference/activate_steps.md)
  :

  Activate one or more of an *agent*'s validation steps

- [![](icons/deactivate_steps.png)](https://rstudio.github.io/pointblank/reference/deactivate_steps.md)
  [`deactivate_steps()`](https://rstudio.github.io/pointblank/reference/deactivate_steps.md)
  :

  Deactivate one or more of an *agent*'s validation steps

- [![](icons/remove_steps.png)](https://rstudio.github.io/pointblank/reference/remove_steps.md)
  [`remove_steps()`](https://rstudio.github.io/pointblank/reference/remove_steps.md)
  :

  Remove one or more of an *agent*'s validation steps

## The Multiagent

The *multiagent* is a group of agents, all bundled together into a
single object. With this grouping, we can generate unified reporting
across the component agents with the
[`get_multiagent_report()`](https://rstudio.github.io/pointblank/reference/get_multiagent_report.md)
function. One style of reporting (`"long"`) provides a serial listing of
agent reports. The other option (`"wide"`) is useful for tracking the
evolution of data quality checks over time since common steps across all
interrogations will form individual rows (and each interrogation will
form a column).

- [![](icons/create_multiagent.png)](https://rstudio.github.io/pointblank/reference/create_multiagent.md)
  [`create_multiagent()`](https://rstudio.github.io/pointblank/reference/create_multiagent.md)
  :

  Create a **pointblank** *multiagent* object

- [![](icons/read_disk_multiagent.png)](https://rstudio.github.io/pointblank/reference/read_disk_multiagent.md)
  [`read_disk_multiagent()`](https://rstudio.github.io/pointblank/reference/read_disk_multiagent.md)
  :

  Read **pointblank** *agents* stored on disk as a *multiagent*

- [![](icons/get_multiagent_report.png)](https://rstudio.github.io/pointblank/reference/get_multiagent_report.md)
  [`get_multiagent_report()`](https://rstudio.github.io/pointblank/reference/get_multiagent_report.md)
  : Get a summary report using multiple agents

## pointblank YAML

YAML files can be used in **pointblank** for two distinct purposes: (1)
to define agents and their validation plans, and (2) to define
information for tables. The
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
function allows us write agent or informant YAML from the namesake
objects. We can read them back from disk by using the
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md)
and
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md)
functions. As a nice shortcut, we read *agent* YAML and interrogate
immediately with
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md);
in a similar manner, we can read *informant* YAML and incorporate table
information with
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md).

- [![](icons/yaml_write.png)](https://rstudio.github.io/pointblank/reference/yaml_write.md)
  [`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
  :

  Write **pointblank** objects to YAML files

- [![](icons/yaml_read_agent.png)](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md)
  [`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md)
  :

  Read a **pointblank** YAML file to create an *agent* object

- [![](icons/yaml_read_informant.png)](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md)
  [`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md)
  :

  Read a **pointblank** YAML file to create an *informant* object

- [![](icons/yaml_agent_interrogate.png)](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)
  [`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)
  :

  Get an *agent* from **pointblank** YAML and
  [`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)

- [![](icons/yaml_agent_string.png)](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md)
  [`yaml_agent_string()`](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md)
  :

  Display **pointblank** YAML using an agent or a YAML file

- [![](icons/yaml_agent_show_exprs.png)](https://rstudio.github.io/pointblank/reference/yaml_agent_show_exprs.md)
  [`yaml_agent_show_exprs()`](https://rstudio.github.io/pointblank/reference/yaml_agent_show_exprs.md)
  :

  Display validation expressions using **pointblank** YAML

- [![](icons/yaml_informant_incorporate.png)](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md)
  [`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md)
  :

  Get an *informant* from **pointblank** YAML and
  [`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)

- [![](icons/yaml_exec.png)](https://rstudio.github.io/pointblank/reference/yaml_exec.md)
  [`yaml_exec()`](https://rstudio.github.io/pointblank/reference/yaml_exec.md)
  : Execute all agent and informant YAML tasks

## Table Transformers

**Table Transformer** functions can radically transform a data table and
either provide a wholly different table (like a summary table or table
properties table) or do some useful filtering in a single step. This can
be useful for preparing the target table for validation, creating a
temporary table for a few validation steps, or even as something used
outside of the **pointblank** workflows. As a nice bonus these
transformer functions will work equally well with data frames, database
tables, and Spark tables.

- [![](icons/tt_summary_stats.png)](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md)
  [`tt_summary_stats()`](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md)
  : Table Transformer: obtain a summary stats table for numeric columns
- [![](icons/tt_string_info.png)](https://rstudio.github.io/pointblank/reference/tt_string_info.md)
  [`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md)
  : Table Transformer: obtain a summary table for string columns
- [![](icons/tt_tbl_dims.png)](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md)
  [`tt_tbl_dims()`](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md)
  : Table Transformer: get the dimensions of a table
- [![](icons/tt_tbl_colnames.png)](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md)
  [`tt_tbl_colnames()`](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md)
  : Table Transformer: get a table's column names
- [![](icons/tt_time_shift.png)](https://rstudio.github.io/pointblank/reference/tt_time_shift.md)
  [`tt_time_shift()`](https://rstudio.github.io/pointblank/reference/tt_time_shift.md)
  : Table Transformer: shift the times of a table
- [![](icons/tt_time_slice.png)](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
  [`tt_time_slice()`](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
  : Table Transformer: slice a table with a slice point on a time column
- [![](icons/get_tt_param.png)](https://rstudio.github.io/pointblank/reference/get_tt_param.md)
  [`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md)
  : Get a parameter value from a summary table

## Utility and Helper Functions

- [![](icons/col_schema.png)](https://rstudio.github.io/pointblank/reference/col_schema.md)
  [`col_schema()`](https://rstudio.github.io/pointblank/reference/col_schema.md)
  : Generate a table column schema manually or with a reference table

- [![](icons/has_columns.png)](https://rstudio.github.io/pointblank/reference/has_columns.md)
  [`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md)
  : Determine if one or more columns exist in a table

- [![](icons/affix_date.png)](https://rstudio.github.io/pointblank/reference/affix_date.md)
  [`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md)
  : Put the current date into a file name

- [![](icons/affix_datetime.png)](https://rstudio.github.io/pointblank/reference/affix_datetime.md)
  [`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md)
  : Put the current datetime into a file name

- [`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)
  :

  A specialized version of
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) for
  **pointblank**:
  [`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)

- [![](icons/from_github.png)](https://rstudio.github.io/pointblank/reference/from_github.md)
  [`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md)
  : Specify a file for download from GitHub

## Datasets

- [`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md)
  : A small table that is useful for testing

- [`small_table_sqlite()`](https://rstudio.github.io/pointblank/reference/small_table_sqlite.md)
  :

  An SQLite version of the `small_table` dataset

- [`specifications`](https://rstudio.github.io/pointblank/reference/specifications.md)
  : A table containing data pertaining to various specifications

- [`game_revenue`](https://rstudio.github.io/pointblank/reference/game_revenue.md)
  : A table with game revenue data

- [`game_revenue_info`](https://rstudio.github.io/pointblank/reference/game_revenue_info.md)
  :

  A table with metadata for the `game_revenue` dataset
