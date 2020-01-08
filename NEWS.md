# pointblank (development version)

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

* Revisions on account of API changes in **tidyr** `1.0.0`

# pointblank 0.2.0

* Incorporates corrections related to API changes in **rlang** `0.2.0`

# pointblank 0.1

* First release
