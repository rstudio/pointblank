# pointblank (unreleased)

## Pointblank Metadata

* The new metadata workflow is full of features that helps to describe tables and keep on top of changes to them.

* Added the `create_metadata()` function to create a `ptblank_metadata` object (this function is similar to `create_agent()`). It is meant to hold metadata for a target table, with reporting features for geared toward communication. 

* Functions for facilitating entry of metadata elements were added (`meta_table()`, `meta_columns()`, and `meta_section()`). These are focused on describing columns, the table, and other fields.

* If all that wasn't enough, this release adds `meta_snippet()` to round out the collection of `meta_functions()` for this workflow. Oh, and also the all-important `incorporate()` function. What? The idea is to have a method for acquiring important bits of data from the target table (`meta_snippet()`) and then using `incorporate()` to get those morsels of data and stitch them into the metadata strings (via `{ }`).

* Added the `get_metadata_report()` function for printing the metadata to a **gt** table object.

* Printing the *metadata* object shows the metadata report thanks to a new print method for this purpose. 

* The *metadata* object can be written to **pointblank** YAML using the revised `yaml_write()` (previously `agent_yaml_write()`) function. We can actually write both an *agent* and the *metadata* to the same YAML file which is useful since both objects share the same target table.

## Translations and Locales

* More text in the agent report is translated now.

* Improved the Spanish (Spain) translation.

* Added the Portuguese (`"pt"`, Brazil) and Chinese (`"zh"`, China mainland) translations.

* Added a locale option for reporting; the locale will match the language (using the base locale) unless a different locale is specified. The locale is used to format numeric values according to the locale's rules.

## Breaking changes

* The `yaml_write()` replaces the `agent_yaml_write()` function. The new function works to write the *agent*, the *metadata* object, or both, to YAML.

## Minor improvements and bug fixes

* Improved appearance of the agent report: (1) more tooltips, (2) tooltips are much improved: they animate, look great, and are snappier than the previous ones, (3) SVGs now used as symbols for the validation steps, (4) less confusing glyphs used in the `TBL` column, (5) agent label can be expressed as Markdown and looks nicer in the report, (6) table type (and name, if supplied as `tbl_name`) is shown in the header, (7) validation threshold levels also shown in the table header, (8) interrogation starting/ending timestamps shown (along with duration) in the table footer, (9) the table font has been changed, and (10) adjustments to table borders and cell shading were made for better readability.

* Refactored a large portion of the code that produces the agent report to increase rendering speed.

* Improved printing of errors/warnings (in the tooltips of the `EVAL` column in the agent report) thanks to implementation of HTML escaping.

* The small version of the agent report (perfect for emailing) now has improved formatting and better optimization of components.

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
