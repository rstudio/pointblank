<img src="inst/graphics/pointblank_logo.png">

[![Travis-CI Build Status](https://travis-ci.org/rich-iannone/pointblank.svg?branch=master)](https://travis-ci.org/rich-iannone/pointblank)
[![codecov.io](https://codecov.io/github/rich-iannone/pointblank/coverage.svg?branch=master)](https://codecov.io/github/rich-iannone/pointblank?branch=master) 

## We All Need to Validate our Data Sometimes

Tables can often be trustworthy. All the data seems to be there and we may feel we can count on these 
tables to deliver us the info we need. Still, sometimes, the tables we trust are hiding things from
us. Malformed strings, numbers we don't expect, missing values that ought not to be missing. These
abberations can be hiding almost in plain sight. Such inconsistencies can be downright insidious, and with time all of this makes us ask ourselves, "can we really trust any table?"

Sure, we can sit down with a table during a long interrogation session and rough it up with a little **SQL**. The problem is we have lots of tables, and we usually have a lot of columns in every one of these tables. Makes for long hours with many suspect tables...

We need a tool like **pointblank**. It lets us get up close with tables and unleash a fury of validation checks. Are some tables in remote databases? That's no problem, we'll interrogate them from afar. In essence, your DB tables can get the same line of questioning as your local data frames or those innocent-looking tibbles. Trust me, they'll start to talk and then they'll surely reveal what they're hiding after an intense **pointblank** session.

You don't have to type up a long report either, **pointblank** will take care of the paperwork. At the very least, you can get a `yes` or `no` as to whether everything checked out. With a little bit of planning, a very informative validation report can be regularly produced. We can even fire off emails if things get out of hand.

### Validating Local Data Frames

The **pointblank** package can validate data in local data frames, local tibble objects, in CSV and TSV files, and in database tables (**PostgreSQL** and **MySQL**). First, let's look at local tables with...

<img src="inst/graphics/example_workflow.png">

The above workflow relied on these code blocks:

  (1) Create 2 very simple **R** **tibble** objects:

```r
library(tibble)

tbl_1 <-
  tibble::tribble(
    ~a, ~b,   ~c,
    1,   6,   "h2adb",
    2,   7,   "h2spb",
    3,   8,   "h2df",
    4,   9,   "d3jwb",
    5,  10,   "h2esf")

tbl_2 <-
  tibble::tribble(
    ~d,   ~e,  ~f,
    "a",   0,  32,
    "b",   0,  31,
    "a",   1,  30,
    "a",   1,  32,
    "ae", -1,  39)
```

  (2) Create a **pointblank** pipeline for validating both the `tbl_1` and `tbl_2` tables (ending with `interrogate()`):

```r
library(pointblank)

agent <- 
  create_agent() %>%             # (1)
  focus_on(
    tbl_name = "tbl_1") %>%      # (2)
  col_vals_gt(
    column = a,
    value = 0) %>%               # (3)
  rows_not_duplicated(
    cols = a & b & c) %>%        # (4)
  col_vals_gte(
    column = a + b,
    value = 7) %>%               # (5)
  col_vals_lte(
    column = b,
    value = 10) %>%              # (6)
  col_vals_regex(
    column = c,
    regex = "h2.*") %>%          # (7)
  col_vals_in_set(
    column = substr(c, 0, 2),
    set = c("h2", "d3")) %>%     # (8)
  focus_on(
    tbl_name = "tbl_2") %>%      # (9)
  col_vals_in_set(
    column = d,
    set = c("a", "b")) %>%       # (10)
  col_vals_not_in_set(
    column = d,
    set = c("a", "b")) %>%       # (11)
  col_vals_gte(
    column = e,
    value = 0) %>%               # (12)
  col_vals_null(
    column = f) %>%              # (13)
  col_vals_not_null(
    column = d) %>%              # (14)
  interrogate()                  # (15)
```

We can get a detailed summary of the interrogation, showing how many individual tests in each validation step (one per row) had passed or failed. The validations are classified with an `action` which indicates the type of action to perform based on user-defined thresholds (thresholds can be set globally, or, for each validation step).

```r
library(pointblank)

get_summary(agent)
#> # A tibble: 11 x 11
#>    tbl_name  db_type      assertion_type          column value regex all_passed     n n_passed f_passed action
#>       <chr>    <chr>               <chr>           <chr> <dbl> <chr>      <lgl> <dbl>    <dbl>    <dbl>  <chr>
#>  1    tbl_1 local_df         col_vals_gt               a     0  <NA>       TRUE     5        5      1.0   <NA>
#>  2    tbl_1 local_df rows_not_duplicated         a, b, c    NA  <NA>       TRUE     5        5      1.0   <NA>
#>  3    tbl_1 local_df        col_vals_gte           a + b     7  <NA>       TRUE     5        5      1.0   <NA>
#>  4    tbl_1 local_df        col_vals_lte               b    10  <NA>       TRUE     5        5      1.0   <NA>
#>  5    tbl_1 local_df      col_vals_regex               c    NA  h2.*      FALSE     5        4      0.8   warn
#>  6    tbl_1 local_df     col_vals_in_set substr(c, 0, 2)    NA  <NA>       TRUE     5        5      1.0   <NA>
#>  7    tbl_2 local_df     col_vals_in_set               d    NA  <NA>      FALSE     5        4      0.8   warn
#>  8    tbl_2 local_df col_vals_not_in_set               d    NA  <NA>      FALSE     5        1      0.2   warn
#>  9    tbl_2 local_df        col_vals_gte               e     0  <NA>      FALSE     5        4      0.8   warn
#> 10    tbl_2 local_df       col_vals_null               f    NA  <NA>      FALSE     5        0      0.0   warn
#> 11    tbl_2 local_df   col_vals_not_null               d    NA  <NA>       TRUE     5        5      1.0   <NA>
```

Or a self-contained HTML report can be generated that shows how the validation went.

```r
library(pointblank)

html_summary(agent)
```

### Function Roundup

That last workflow example provided a glimpse of some of the functions available. Just for the sake of completeness, here's the entire set of functions. A veritable smorgasbord of validation functionality, really.

<img src="inst/graphics/pointblank_functions.png">

### Constraining Data in Validation Steps

Every validation function has a common set of options for constraining validations to certain conditions. This can occur through the use of computed columns and also through preconditions that can allow you to target validations on only those rows that satify one or more conditions. 

<img src="inst/graphics/function_options.png">

### Validating Tables in a Database

To validate tables in a database (PostgreSQL and MySQL), we can optionally create a credentials file.

```r
library(pointblank)

create_creds_file(
  file = ".db_creds",
  dbname = ***********,
  host = ***********************,
  port = ***,
  user = ********,
  password = **************)
```

A database table can be treated similarly to a local data frame.

```r
library(pointblank)

agent_db <- 
  create_agent() %>%
  focus_on(
    tbl_name = "table_1",
    db_type = "PostgreSQL",
    creds_file = ".db_creds",
    initial_sql = "WHERE date > '2017-01-15'") %>%
  rows_not_duplicated() %>%
  col_vals_gte(
    column = a,
    value = 2) %>%
  col_vals_between(
    column = b + c + d,
    left = 50,
    right = 100) %>%
  col_vals_not_null(
    column = e,
    preconditions = is.na(d)) %>%
  interrogate()
```

## Installation

**pointblank** is used in an **R** environment. If you don't have an **R** installation, it can be obtained from the [**Comprehensive R Archive Network (CRAN)**](https://cran.r-project.org/).

You can install the development version of **pointblank** from **GitHub** using the **devtools** package.

```r
devtools::install_github("rich-iannone/pointblank")
```

## Code of Conduct

[Contributor Code of Conduct](https://github.com/rich-iannone/pointblank/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

MIT &copy; Richard Iannone
