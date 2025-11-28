# Specify a file for download from GitHub

The `from_github()` function is helpful for generating a valid URL that
points to a data file in a public GitHub repository. This function can
be used in the `file` argument of the
[`file_tbl()`](https://rstudio.github.io/pointblank/reference/file_tbl.md)
function or anywhere else where GitHub URLs for raw user content are
needed.

## Usage

``` r
from_github(file, repo, subdir = NULL, default_branch = "main")
```

## Arguments

- file:

  The name of the file to target in a GitHub repository. This can be a
  path leading to and including the file. This is combined with any path
  given in `subdir`.

- repo:

  The GitHub repository address in the format
  `username/repo[/subdir][@ref|#pull|@*release]`.

- subdir:

  A path string representing a subdirectory in the GitHub repository.
  This is combined with any path components included in `file`.

- default_branch:

  The name of the default branch for the repo. This is usually `"main"`
  (the default used here).

## Value

A character vector of length 1 that contains a URL.

## Function ID

13-6

## See also

Other Utility and Helper Functions:
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md),
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md),
[`col_schema()`](https://rstudio.github.io/pointblank/reference/col_schema.md),
[`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md),
[`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)

## Examples

``` r
# A valid URL to a data file in GitHub can be
# obtained from the HEAD of the default branch
# from_github(
#   file = "inst/data_files/small_table.csv",
#   repo = "rstudio/pointblank"
# )

# The path to the file location can be supplied
# fully or partially to `subdir`
# from_github(
#   file = "small_table.csv",
#   repo = "rstudio/pointblank",
#   subdir = "inst/data_files"
# )

# We can use the first call in combination with
# `file_tbl()` and `create_agent()`; this
# supplies a table-prep formula that gets
# a CSV file from the GitHub repository for the
# pointblank package 
# agent <- 
#   create_agent(
#     tbl = ~ file_tbl(
#       file = from_github(
#         file = "inst/data_files/small_table.csv",
#         repo = "rstudio/pointblank"
#       ),
#       col_types = "TDdcddlc"
#     )
#   ) %>%
#   col_vals_gt(a, 0) %>%
#   interrogate()

# The `from_github()` helper function is
# pretty powerful and can get at lots of
# different files in a repository

# A data file from GitHub can be obtained from
# a commit at release time
# from_github(
#   file = "inst/extdata/small_table.csv",
#   repo = "rstudio/pointblank@v0.2.1"
# )

# A file may also be obtained from a repo at the
# point in time of a specific commit (partial or
# full SHA-1 hash for the commit can be used)
# from_github(
#   file = "data-raw/small_table.csv",
#   repo = "rstudio/pointblank@e04a71"
# )

# A file may also be obtained from an
# *open* pull request
# from_github(
#   file = "data-raw/small_table.csv",
#   repo = "rstudio/pointblank#248"
# )
```
