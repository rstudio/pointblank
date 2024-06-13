# pointblank (development version)

## Minor improvements and bug fixes

- `col_vals_expr()` now shows used columns in the agent report

- Fixed a regression in `col_vals_*()` functions, where `vars("col")` was evaluating to the string `"col"`. Behavior of `vars("col")` is now aligned back with `vars(col)` - both evaluate to the column name `col`.

- Warnings/errors arising from comparing `columns` to a `value` of different class (for example, comparing a datetime column to a date value `Sys.Date()` instead of another datetime value `Sys.time()`) are now signalled appropriately at `interrogate()`.

# pointblank 0.12.1

- Ensured that the column string is a symbol before constructing the expression for the `col_vals_*()` functions.

- No longer resolve columns with tidyselect when the target table cannot be materialized.

- Relaxed tests on tidyselect error messages.

# pointblank 0.12.0

## New features

* Complete `{tidyselect}` support for the `columns` argument of *all validation functions*, as well as in `has_columns()` and `info_columns`. The `columns` argument can now take familiar column-selection expressions as one would use inside `dplyr::select()`. This also begins a process of deprecation:
  - `columns = vars(...)` will continue to work, but `c()` now supersedes `vars()`.
  - If passing an *external vector* of column names, it should be wrapped in `all_of()`.

* The `label` argument of validation functions now exposes the following string variables via `{glue}` syntax:

  - `"{.step}"`: The validation step name
  - `"{.col}"`: The current column name
  - `"{.seg_col}"`: The current segment's column name
  - `"{.seg_val}"`: The current segment's value/group
  
  These dynamic values may be useful for validations that get expanded into multiple steps.
  
* `interrogate()` gains two new options for printing progress in the console output:

  - `progress`: Whether interrogation progress should be printed to the console (`TRUE` for interactive sessions, same as before)
  - `show_step_label`: Whether each validation step's label value should be printed alongside the progress.

## Minor improvements and bug fixes

* Fixes issue with rendering reports in Quarto HTML documents.

* When no columns are returned from a `{tidyselect}` expression in `columns`, the agent's report now displays the originally supplied *expression* instead of being simply blank (e.g., in `create_agent(small_table) |> col_vals_null(matches("z"))`).

* Fixes issue with the hashing implementation to improve performance and alignment of validation steps in the multiagent.

# pointblank 0.11.4

* Fixes issue with gt `0.9.0` compatibility.

# pointblank 0.11.3

* Fixes issue with tables not rendering due to interaction with the gt package.

# pointblank 0.11.2

* Internal changes were made to ensure compatibility with an in-development version of R.

# pointblank 0.11.1

* Updated all help files to pass HTML validation.

# pointblank 0.11.0

## New features

* The `row_count_match()` function can now match the count of rows in the target table to a literal value (in addition to comparing row counts to a secondary table).

* The analogous `col_count_match()` function was added to compare column counts in the target table to a secondary table, or, to match on a literal value.

* Substitution syntax has been added to the `tbl_store()` function with `{{ <name> }}`. This is a great way to make table-prep more concise, readable, and less prone to errors.

* The `get_informant_report()` has been enhanced with more `width` options. Aside from the `"standard"` and `"small"` sizes we can now supply any pixel- or percent-based width to precisely size the reporting.

* Added support for validating data in BigQuery tables.

## Documentation

* All functions in the package now have *better* usage examples.

# pointblank 0.10.0

## New features

* The new function `row_count_match()` (plus `expect_row_count_match()` and `test_row_count_match()`) checks for exact matching of rows across two tables (the target table and a comparison table of your choosing). Works equally well for local tables and for database and Spark tables.

* The new `tbl_match()` function (along with `expect_tbl_match()` and `test_tbl_match()`) check for an exact matching of the target table with a comparison table. It will check for a strict match on table schemas, on equivalent row counts, and then exact matches on cell values across the two tables.

## Minor improvements and bug fixes

* The `set_tbl()` function was given the `tbl_name` and `label` arguments to provide an opportunity to set metadata on the new target table.

* Support for `mssql` tables has been restored and works exceedingly well for the majority of validation functions (the few that are incompatible provide messaging about not being supported).

## Documentation

* All functions in the package now have usage examples.

* An RStudio Cloud project has been prepared with .Rmd files that contain explainers and runnable examples for each function in the package. Look at the project README for a link to the project.

## Breaking changes

* The `read_fn` argument in `create_agent()` and `create_informant()` has been deprecated in favor of an enhanced `tbl` argument. Now, we can supply a variety of inputs to `tbl` for associating a target table to an agent or an informant. With `tbl`, it's now possible to provide a table (e.g., `data.frame`, `tbl_df`, `tbl_dbi`, `tbl_spark`, etc.), an expression (a table-prep formula or a function) to read in the table only at interrogation time, or a table source expression to get table preparations from a table store (as an in-memory object or as defined in a YAML file).

* The `set_read_fn()`, `remove_read_fn()`, and `remove_tbl()` functions were removed since the `read_fn` argument has been deprecated (and there's virtually no need to remove a table from an object with `remove_tbl()` now).

# pointblank 0.9.0

## New features

* The new `rows_complete()` validation function (along with the `expect_rows_complete()` and `test_rows_complete()` expectation and test variants) check whether rows contain any `NA`/`NULL` values (optionally constrained to a selection of specified `columns`).

* The new function `serially()` (along with `expect_serially()` and `test_serially()`) allows for a series of tests to run in sequence before either culminating in a final validation step or simply exiting the series. This construction allows for pre-testing that may make sense before a validation step. For example, there may be situations where it's vital to check a column type before performing a validation on the same column.

* The `specially()`/`expect_specially()`/`test_specially()` functions enable custom validations/tests/expectations with a user-defined function. We still have `preconditions` and other common args available for convenience. The great thing about this is that because we require the UDF to return a logical vector of passing/failing test units (or a table where the rightmost column is logical), we can incorporate the results quite easily in the standard **pointblank** reporting.

* The `info_columns_from_tbl()` function is a super-convenient wrapper for the `info_columns()` function. Say you're making a data dictionary with an *informant* and you already have the table metadata somewhere as a table: you can use that here and not have to call `info_columns()` many, many times.

* Added the `game_revenue_info` dataset which contains metadata for the extant `game_revenue` dataset. Both datasets pair nicely together in examples that create a data dictionary with `create_informant()` and `info_columns_from_tbl()`.

* Added the table transformer function `tt_tbl_colnames()` to get a table's column names for validation.

## Minor improvements and bug fixes

* Input data tables with `label` attribute values in their columns will be displayed in the 'Variables' section of the `scan_data()` report. This is useful when scanning imported SAS tables (which often have labeled variables).

* The `all_passed()` function has been improved such that failed validation steps (that return an evaluation error, perhaps because of a missing column) result in `FALSE`; the `i` argument has been added to `all_passed()` to optionally get a subset of validation steps before evaluation.

* For those `expect_*()` functions that can handle multiple columns, **pointblank** now correctly stops at the first failure and provides the correct reporting for that. Passing multiple columns really should mean processing multiple steps in serial, and previously this was handled incorrectly.

# pointblank 0.8.0

## New features

* The new `draft_validation()` function will create a starter validation .R or .Rmd file with just a table as an input. Uses a new 'column roles' feature to develop a starter set of validation steps based on what kind of data the columns contain (e.g., latitude/longitude values, URLs, email addresses, etc.).

* The validation function `col_vals_within_spec()` (and the variants `expect_col_vals_within_spec()` and `test_col_vals_within_spec()`) will test column values against a specification like phone numbers (`"phone"`), VIN numbers (`"VIN"`), URLs (`"url"`), email addresses (`"email"`), and much more (`"isbn"`, `"postal_code[<country_code>]"`, `"credit_card"`, `"iban[<country_code>]"`, `"swift"`, `"ipv4"`, `"ipv6"`, and `"mac"`).

* A large cross section of row-based validation functions can now operate on segments of the target table, so you can run a particular validation with slices (or segments) of the target table. The segmentation is made possible by use of the new `segments` argument, which takes an expression that serves to segment the target table by column values. It can be given in one of two ways: (1) as a single or multiple column names containing keys to segment on, or (2) as a two-sided formula where the LHS holds a column name and the RHS contains the column values to segment on (allowing for a subset of keys for segmentation).

* The default printing of the *multiagent* object is now a stacked display of *agent* reports. The wide report (useful for comparisons of validations targeting the same table over time) is available in the improved `get_multiagent_report()` function (with `display_mode = "wide"`).

* Exporting the reporting is now much easier with the new `export_report()` function. It will export objects such as the *agent* (for validations), the *informant* (for table metadata), and the *multiagent* (a series of validations), and, also those objects containing customized reports (from `scan_data()`, `get_agent_report()`, `get_informant_report()`, and `get_multiagent_report()`). You'll always get a self-contained HTML file of the report from any use of `export_report()`.

* A new family of functions has been added to **pointblank**: Table Transformers! These functions can radically transform a data table and either provide a wholly different table (like a summary table or table properties table) or do some useful filtering in a single step. This can be useful for preparing the target table for validation or when creating temporary tables (through `preconditions`) for a few validation steps (e.g., validating table properties or string lengths). As a nice bonus these transformer functions will work equally well with data frames, database tables, and Spark tables. The included functions are: `tt_summary_stats()`, `tt_string_info()`, `tt_tbl_dims()`, `tt_time_shift()`, and `tt_time_slice()`.

* Two new datasets have been added: `specifications` and `game_revenue`. The former dataset can be used to test out the `col_vals_within_spec()` validation function. The latter dataset (with 2,000 rows) can be used to experiment with the `tt_time_shift()` and `tt_time_slice()` table transformer functions.

## Minor improvements and bug fixes

* Added the Polish (`"pl"`), Danish (`"da"`), Turkish (`"tr"`), Swedish (`"sv"`), and Dutch (`"nl"`) translations.

* The `scan_data()` function is now a bit more performant, testable, and better at communicating progress in generating the report.

* The `preconditions` argument, used to modify the target table in a validation step, is now improved by (1) checking that a table object is returned after evaluation, and (2) correcting the YAML writing of any `preconditions` expression that's provided as a function.

* The `x_write_disk()` and `x_read_disk()` have been extended to allow the writing and reading of `ptblank_tbl_scan` objects (returned by `scan_data()`).

* Print methods received some love in this release. Now, `scan_data()` table scan reports look much better in **R Markdown**. Reporting objects from `get_agent_report()`, `get_informant_report()`, and `get_multiagent_report()` now have print methods and work beautifully in **R Markdown** as a result.

* The `incorporate()` function, when called on an *informant* object, now emits styled messages to the console. And when using `yaml_exec()` to process an arbitrary amount of YAML-based *agent*s and *informant*s, you'll be given information about that progress in the console.

## Documentation

* Many help files were overhauled so that (1) things are clearer, (2) more details are provided (if things are complex), and (3) many ready-to-run examples are present. The functions with improved help in this release are: `all_passed()`, `get_data_extracts()`, `get_multiagent_report()`, `get_sundered_data()`, `has_columns()`, `write_testthat_file()`, `x_write_disk()`, and `yaml_exec()`.

# pointblank 0.7.0

## New features

* New functions for set-based interrogations: `col_vals_make_set()` (+ `expect_col_vals_make_set()` and `test_col_vals_make_set()`) and `col_vals_make_subset()` (+ `expect_col_vals_make_subset()` and `test_col_vals_make_subset()`); they answer the following two questions: (1) is a set of values entirely accounted for in a column of values?, and (2) is a set of values a subset of a column of values?

* New functions for order-based interrogations: `col_vals_increasing()` (+ `expect_col_vals_increasing()` and `test_col_vals_increasing()`) and `col_vals_decreasing()` (+ `expect_col_vals_decreasing()` and `test_col_vals_decreasing()`); they check that column values are either increasing or decreasing and both have options to allow for non-moving values and backtracking (with a threshold).

* Several functions added to facilitate multi-agent workflows: `create_multiagent()`, `read_disk_multiagent()`, and `get_multiagent_report()`; these workflows help to track interrogation results across multiple agents and the reporting scales well from several to dozens of agents.

* The new function `write_testthat_file()` generates a **testthat** test file and puts it in `tests/testthat` if certain conditions are met; this converts an *agent*'s validation plan into separate `expect_*()` statements.

* New functions `tbl_store()`, `tbl_source()`, and `tbl_get()` functions added for centrally managing table-prep formulas.

* Added the `yaml_exec()` function that processes all relevant **pointblank** YAML files in a directory; execution involves interrogation of agents (given YAML agents) and incorporation of informants (given YAML informants), saving all the processed objects to an output directory.

* The new functions `file_tbl()` and helper `from_github()` make it easy to generate a table from a compatible data file; a file could be in the form of `CSV`, `TSV`, `RDA`, or `RDS`.

* Several functions have been added for modifying an *agent*'s validation plan: `activate_steps()`, `deactivate_steps()`, `remove_steps()`.

* Added the `snip_stats()` function for generating an in-line statistical summary in an information report.

* Add sorting options for `snip_list()` so we can choose to sort column items by frequency or sequentially (alphabetically/numerically).

* More improvements were made to `snip_list()` to: (1) have a better default appearance, (2) enable more customization, and (3) include localization options for the supported spoken languages.

* Added several options for customizing the main reporting heading in three reporting objects: the agent report, the information report, and the multiagent report.

* The `active` argument in every validation function can now take an expression that evaluates to a logical; the `has_columns()` has been added to make it easy to express in `active` whether one or more columns are present in the target table (e.g., perform the validation step only if the target column is available).

* Added support for using Arrow tables as target tables for *informant* objects.

## Documentation

* Added information on YAML representations of all validation functions and several other functions that make an appearance in YAML.

* General improvements to function documentation were made to a wide cross section of the exported functions.

## Minor improvements and bug fixes

* Included method for writing an *informant* object to disk (with `x_write_disk()`).

* Many fixes were made and tests added to ensure that *agents* survive the YAML roundtrip (so `agent` %>% `yaml_write()` then `yaml_read_agent()` creates the same `agent` object).

* Updated several internal `dplyr::arrange()` statements used by `scan_data()` so that warnings aren't issued by **dbplyr** (for table scans operating on `tbl_dbi` objects).

* All **tidyselect** expressions used with *agents* are now preserved when the agent is written to YAML.

# pointblank 0.6.0

## Pointblank Information

* The new *information management* workflow is full of features that help you to describe tables and keep on top of changes to them. To make this work well, a new character enters: the *informant*!

* Added the `create_informant()` function to create a `ptblank_informant` object (this function is similar to `create_agent()`). It is meant to hold information (as much as you want, really) for a target table, with reporting features geared toward communication.

* Functions for facilitating entry of *info text* were added because we need them (`info_tabular()`, `info_columns()`, and `info_section()`). These are focused on describing columns, the table proper, and other misc. fields.

* If all that wasn't enough, this release adds `info_snippet()` to round out the collection of `info_*()` functions for this workflow. Oh, hang on, there's also the all-important `incorporate()` function. What? To explain, the idea is to have some methodology for acquiring important bits of data from the target table (that's `info_snippet()`'s job) and then use `incorporate()` to grab those morsels of data and stitch them into the *info text* (via `{ }`).

* Added the `get_informant_report()` function for printing the information report (a **gt** table object!).

* You can also just print the *informant* object to show the information report thanks to a print method for this purpose. 

* The *informant* object can be written to **pointblank** YAML using the revised `yaml_write()` (previously `agent_yaml_write()`) function. We can actually write both the *agent* and the *informant* to the same YAML file which is useful since both objects share the same target table. Reading is done with the `yaml_read_agent()` and `yaml_read_informant()` functions.

* The *informant* can be emailed using the `email_create()` function; this emailing can be done in one of eight languages for the stock message text.

## Translations and Locales

* More text in the agent report is translated now.

* Improved the Spanish (Spain) translation.

* Added the Portuguese (`"pt"`, Brazil), Chinese (`"zh"`, China mainland), and Russian (`"ru"`) translations.

* Added a locale option for reporting; the locale will match the language (using the base locale) unless a different locale is specified. The locale is used to format numeric values according to the locale's rules. This also applies to the reporting offered by the `scan_data()` function.

* All stock email message parts (used when emailing the agent report or the information report) have been translated to the eight supported languages. The language setting in the respective objects is used to determine the language of the stock message parts.

## Breaking changes

* The `yaml_write()` function replaces the `agent_yaml_write()` function. The new function works to write the *agent*, the *informant* object, or both, to YAML.

* The names of more YAML functions have been changed, the final roster now consists of: `yaml_write()`, `yaml_read_agent()`, `yaml_read_informant()`, `yaml_agent_interrogate()`, `yaml_agent_string()`, and `yaml_agent_show_exprs()`.

* The `x_write_disk()` function replaces the `agent_write()` function. The new function works to write the *agent* or the *informant* object to disk.

* The `x_read_disk()` function replaces the `agent_read()` function. The new function works to read both the *agent* or the *informant* objects written to disk.

* The `email_preview()` function has been renamed to `email_create()`.

## New features

* The new `db_tbl()` function makes it ridiculously easy to access a database table from the selection of databases that **pointblank** supports for validation; they are accessible with the supplied keywords `"postgres"` (PostgreSQL), `"mysql"` (MySQL), `"maria"` (MariaDB), `"duckdb"` (DuckDB), `"sqlite"` (SQLite), or, with any driver function you'd like to supply.

* Added the `log4r_step()` function which can be used as an action in an `action_levels()` function call (i.e., a list component for the `fns` list). We can place a call to this function in every condition that should produce a log entry (i.e., `warn`, `stop`, `notify`). 

## Documentation

* Added several articles that explain the different validation workflows (there are six of 'em) and articles that go over the *Information Management* workflow.

* Improved documentation for almost all functions in the package; added more useful examples.

* Added a table to the project `README` that keeps everyone apprised of the project milestones and the issues to be closed for each upcoming release.

## Minor improvements and bug fixes

* Improved appearance of the agent report: (1) more tooltips, (2) the tooltips are much improved (they animate, have larger text, and are snappier than the previous ones), (3) SVGs are now used as symbols for the validation steps instead of blurry PNGs, (4) less confusing glyphs are now used in the `TBL` column, (5) the agent label can be expressed as Markdown and looks nicer in the report, (6) the table type (and name, if supplied as `tbl_name`) is shown in the header, (7) validation threshold levels also shown in the table header, (8) interrogation starting/ending timestamps are shown (along with duration) in the table footer, (9) the table font has been changed to be less default-y, and (10) adjustments to table borders and cell shading were made for better readability.

* The `get_agent_report()` function now has `lang` and `locale` arguments to override any of those values set prior (e.g., in `create_agent()`). This allows for the reporting language to be changed without the need to re-run everything from scratch.

* The `set_tbl()`, `remove_tbl()`, `set_read_fn()`, and `remove_read_fn()` functions can now also be used with an *informant* object.

* The `get_sundered_data()` function is more clear with regard to which validation steps are considered for splitting of the data. Using validation steps with `preconditions` must fulfill the rule that the target table only have a single form across steps.

* The `is_exact` argument is new in the `col_schema_match()`, `expect_col_schema_match()`, and `test_col_schema_match()` functions, further allowing these types of validations to be less stringent. This argument loosens the requirement to include all class names for a column that may have multiple. Also, we can specify `NULL` to entirely skip the checking of a class/type.

* We can now use more combinations of validation functions in `conjointly()`. Those validation functions that intrinsically operate over a single test unit (e.g., all of the `col_is_*()` functions) now work in combination with validation functions that operate over *n* test units (e.g., the `col_vals_*()` functions). This lets us test for a condition where columns are of a certain type *AND* individual test units fulfill the `col_vals_*()` requirements.

* Simplified the `sections` argument of `scan_data()` to be a length-1 character vector containing key characters standing for section names.

* Refactored a large portion of the code that produces the agent report to increase rendering speed.

* Improved printing of errors/warnings (in the tooltips of the `EVAL` column in the agent report) thanks to implementation of HTML escaping.

* The small version of the agent report (perfect for emailing) now has much improved formatting.

# pointblank 0.5.2

* Fixes a performance issue for validations on larger tables.

* Improved formatting of value ranges in the agent report.

# pointblank 0.5.1

* Improved compatibility with validations performed on SQL Server 2019.

* Integrated the `label` argument into all validation functions; this label is available in the agent `x_list` and, more importantly, displayed in the agent report (in the `STEP` column).

* Added the `"combined"` option in the `get_sundered_data()` function (for the `type` argument). This applies a categorical (pass/fail) label (settable in the new `pass_fail` argument of the same function) in a new `.pb_combined` flag column of the output table.

* Made several visual improvements to the agent report.

# pointblank 0.5.0

## New features

* The *agent* can now be given a table-reading function, which is used for reading in the data during an interrogation. If a `tbl` is not provided, then this function will be invoked. However, if both a `tbl` and a `read_fn` is specified, then the supplied `tbl` will take priority (useful for one-shot interrogations with a table in an interactive context). There are two ways to specify a `read_fn`: (1) using a function (e.g., `function() { <table reading code> }`) or, (2) with an R formula expression (e.g., `~ { <table reading code> }`).

* Added a a set of functions for setting and removing an agent's association to a data table (`set_tbl()` and `remove_tbl()`) or a table-reading function (`set_read_fn()` and `remove_read_fn()`).

* All validation functions now have a `step_id` parameter. The use of step IDs serves to distinguish validation steps from each other and provide an opportunity for supplying a more meaningful label compared to the step index. Supplying a `step_id` is optional; **pointblank** will automatically generate the step ID value (based on the step index) if it's not provided.

* Added new functions for reading and writing YAML (here, called **pointblank** YAML). A **pointblank** YAML file can be generated with an agent by using the `agent_yaml_write()` function. You're always free to create **pointblank** YAML by hand, or, you can edit/extend an existing **pointblank** YAML file. An agent can be created from **pointblank** YAML with the `agent_yaml_read()` function. It's also possible to interrogate a target data table right from **pointblank** YAML by using `agent_yaml_interrogate()`.

* The `agent_write()` and `agent_read()` functions were added; they allow for saving the agent to disk and reading the agent back from disk. Saved-to-disk agents still retain their validation plans, intel from interrogations, and their reference to a target table (the `read_fn` value) and even the entire target table (if requested). Reading an agent from disk with `agent_read()` allows us to use post-interrogation functions (e.g., `get_agent_x_list()`, `get_data_extracts()`, `get_agent_report()`, etc.) as though the interrogation had just occurred.

* **pointblank** is now compatible with Spark DataFrames through the **sparklyr** package. Simply use a `tbl_spark` object when specifying the target table in `create_agent()`, `set_tbl()`, or `scan_data()`.

## Minor improvements and bug fixes

* An issue with showing the agent report table in the email message body via the `email_blast()` function has been resolved.

* Resolved issue with using literal character values in comparison-based validation functions (e.g., `col_vals_between()`, `col_vals_gt()`, etc.).

* Completely rewrote the underlying processes for the storage and retrieval of translation text.

* Much improved translations of reporting text the Spanish and German languages. Thanks @pachamaltese and @DavZim for these valuable contributions!

* New **testthat** tests were added that test **pointblank** validations against mock PostgreSQL and MySQL database tables via the **dittodb** package. Thank you @pachamaltese for implementing these tests.

# pointblank 0.4.0

## New R Markdown features

* New R Markdown validation feature allows for validation testing within specialized validation code chunks where the `validate = TRUE` option is set. Using **pointblank** validation functions on data in these marked code chunks will flag overall failure if the stop threshold is exceeded anywhere. All errors are reported in the validation code chunk after rendering the document to HTML, where green or red status buttons indicate whether all validations succeeded or failures occurred. Clicking any such button reveals the otherwise hidden validation statements and their error messages (if any). Using **pointblank** in an R Markdown workflow is enabled by default once the **pointblank** library is loaded. While the framework for such testing is set up by default, the new `validate_rmd()` function offers an opportunity to set UI and logging options.

* Added an R Markdown template for the new R Markdown validation feature (`Pointblank Validation`).

* The new `stop_if_not()` function works well as a standalone, replacement for `stopifnot()` but is also customized for use in validation checks in R Markdown documents where **pointblank** is loaded. Using `stop_if_not()` in a code chunk where the `validate = TRUE` option is set will yield the correct reporting of successes and failures whereas `stopifnot()` *does not*.

* A `knit.print()` method was added to facilitate the printing of the agent report table within an R Markdown code chunk.

## Breaking changes

* The default behavior of using validation step functions (e.g., `col_vals_lt()`) directly on data tables has been changed. Before, a single test unit failure would trigger a warning. Now, a single test unit failing results in an error. Going back to the earlier behavior now requires the use of `actions = warn_on_fail()` (a new helper function, which has a default `warn_at` threshold value of `1`) with each invocation of a validation step function. The `stop_on_fail()` helper function is also new in this release, and has a `stop_at` threshold parameter, also with a default of `1`.

## New features

* Added 24 *expectation* functions (e.g., `expect_col_exists()`, `expect_rows_distinct()`, `expect_col_schema_match()`, etc.) as complements of the 24 validation functions. All of these can be used for **testthat** tests of tabular data with a simplified interface that exposes an easy-to-use failure `threshold` (defaulting to `1`).

* Added 24 *test* functions (e.g., `test_col_exists()`, `test_rows_distinct()`, `test_col_schema_match()`, etc.) to further complement the 24 validation functions. These functions return a logical value: `TRUE` if the threshold (having a default of `1`) is *not* exceeded, `FALSE` otherwise. These `test_*()` functions use the same simplified interface of the `expect_*()` functions.

* Added the `col_vals_expr()`, `expect_col_vals_expr()`, and `test_col_vals_expr()` *validation*, *expectation*, and *test* functions, making it easier for DIY validations. The **dplyr** `expr()`, `case_when()`, and `between()` functions were re-exported for easier accessibility here since they work exceedingly well with the new functions.

* `col_schema_match()` (and its *expect* and *test* analogues) gained new arguments: `complete` and `in_order`. These allow for some relaxation of constraints related to the completeness and ordering of columns defined in a `col_schema` object (created by `col_schema()`).

* The `preconditions` argument available in all *validation*, *expectation*, and *test* functions now accepts both formula and function values (previously, only formula values were accepted).

* The `get_agent_report()` function now has a `size` argument as an option to get the agent report table in the `"standard"` (width: 875px) size or the `"small"` size (width: 575px); previously this option was only accessible through `...`.

* The appearance of the agent report has improved and it's gained some new features: (1) data extracts for failing rows (on row-based validation steps) can be downloaded as CSVs via the new buttons that appear in the `EXT` column, (2) there are useful has tooltips on most fields of the table (e.g., hovering over items in `STEP` will show the brief, `TBL` icons will describe whether any preconditions were applied to the table prior to interrogation, etc.), and (3) there are printing improvements in the `COLUMNS` and `VALUES` columns (e.g., table columns are distinguished from literal values).

* Improved the appearance of the email message generated by `email_blast()` and `email_preview()`. This email message, when using the `stock_msg_body()` and `stock_msg_footer()` as defaults for `msg_body` and `msg_footer`, embeds a `"small"` version of the agent report and provides some introductory text with nicer formatting than before.

## Documentation improvements

* All functions now have revised documentation that is more complete, has more examples, and consistent across the many *validation*, *expectation*, and *test* functions.

* The package `README` now contains better graphics, some reworked examples, and a new section on the package's design goals (with a listing of other **R** packages that also focus on table validation).

## Minor improvements and bug fixes

* Rewrote the internal `stock_stoppage()` and `stock_warning()` functions so that the generated error and warning messages match whether validation functions are used directly on data or *expectation* functions are being used.

* Console status messages when performing an interrogation now only appear in an interactive session. They will no longer appear during R Markdown rendering nor during execution of unattended scripts.

* The `col_vals_regex()` *validation* function (plus the associated *expectation* and *test* functions) can now be used with database tables (on some of the DB types that support regular expressions). This has been tested on MySQL and PostgreSQL, which have differing underlying SQL implementations.

* The `col_schema()` function now allows for either uppercase or lowercase SQL column types (using `.db_col_types = "sql"`). Previously, supplying SQL columns types as uppercase (e.g., "INT", "TINYINT", etc.) would always fail validation because the SQL column types of the target table are captured as lowercase values during the `create_agent()` call.

* Many new tests were added to cover both the new functions and the existing functions. It's important for a validation package that testing be comprehensive and rigorous, so, this will continue to be a focus in forthcoming releases.

* Fixed a duration label bug in the console status messages that appear during interrogation (now consistently has values reported in seconds)

* Added column validity checks inside of internal `interrogate_*()` functions

# pointblank 0.3.1

* Fixed implementation of the `col_vals_between()` and `col_vals_not_between()` step functions to work with `tbl_dbi` objects.

* Added the `scan_data()` function, which thoroughly scans any table data so you can understand it better (giving you an HTML report).

* Added the `get_agent_x_list()` function to provide easy access to agent intel

* Added the `get_agent_report()` function to give fine control over the agent's gt-based reportage; also, the agent's default print method is now that report (with default appearance options)

* Added the `get_sundered_data()` function to split the table data into 'pass' and 'fail' pieces after interrogation

* Added the `col_schema_match()` validation step function; it works in conjunction with a `col_schema` object (generated through the `col_schema()` function) to help determine whether the expected schema matches the target table.

* Added multilingual support to reports generated by agent validations and by those produced through the new `scan_data()` function

* More fully integrates the gt (for tables in reports) and blastula (for email production and delivery) packages

* Numerous fixes to ensure compatibility with tibble 3.0.0

# pointblank 0.3.0

The pointblank package has been changed significantly from the previous version in favor of consistency and simplicity, better reporting, and increased power. The internals have been extensively refactored and the API has accordingly gone through revisions.

## Breaking Changes

* The `focus_on()` function has been removed in favor of directly using a data object. This means that a single use of `create_agent()` can now only work on a single table at a time (`create_agent()` now has a `tbl` argument). Also, the input `tbl` can be a `data.frame`, a `tbl_df`, or a `tbl_dbi` object. 

* The `preconditions` argument has changed and it can now be used to temporarily transform the table (i.e., transforming for a particular validation step). Previously, this option could only filter the input table but now it's possible to do useful things like joining in a table, adding columns, filtering rows, etc. The `preconditions` args now accepts a list of expressions that manipulate the table data.

* The `action_levels()` helper function is introduced to work with the `actions` argument (in every validation step function). This replaces the `warn_count`, `stop_count`, `notify_count`, `warn_fraction`, `stop_fraction`, and `notify_fraction` arguments. The function allows for evaluation of functions (given in the `fns` argument) as a reaction to exceeding thresholds specified in `warn_at`, `stop_at`, and `notify_at`.

* When using validation step functions directly on data (i.e., no use of `create_agent()`), data is now passed straight through after that validation step. The purpose now in that mode is to create warnings or throw errors if the `warn` or `stop` thresholds are exceeded.

* Across all **pointblank** validation step functions, the argument that stands for table columns has been normalized to `columns`.

* The `incl_na` argument, which was implemented in a few validation step functions, has been renamed to `na_pass` to better indicate its purpose (to consider any encountered `NA` values as passing test units), and, its use has been expanded to other relevant functions.

## New Features

* It's now possible to use `vars()` and certain tidyselect select helpers (e.g., `starts_with()`) when defining `columns` in the **pointblank** validation step functions.

* The `conjointly()` function is a new validation step function that allows for multiple rowwise validation steps to be performed for joint validity testing.

# pointblank 0.2.1

* Revisions on account of API changes in **tidyr** `1.0.0`.

* Incorporates corrections related to API changes in **rlang** `0.2.0`.

# pointblank 0.1

* First release.
