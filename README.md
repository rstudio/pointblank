----------
pointblank
----------

[![Travis-CI Build Status](https://travis-ci.org/rich-iannone/pointblank.svg?branch=master)](https://travis-ci.org/rich-iannone/pointblank)
[![codecov.io](https://codecov.io/github/rich-iannone/pointblank/coverage.svg?branch=master)](https://codecov.io/github/rich-iannone/pointblank?branch=master) 

## You need to validate tabular data. I need to validate tabular data too.

Tables can often be trustworthy. All the data seems to be there and we may feel we can count on these 
tables to deliver us the info we need. Still, sometimes, the tables we trust are hiding things from
us. Malformed strings, numbers we don't expect, missing values that ought not to be missing. These
abberations can be hiding almost in plain sight. Such inconsistencies can be downright insidious, making us
ask ourselves, "can we really trust any table?"

Sure, we can sit down with a table during a long interrogation session and rough it up with a little SQL. The problem is we have lots of tables, and we usually have a lot of columns in every one of these tables. Makes for long hours with many suspect tables...

We need a tool like pointblank. It lets us get up close with tables and unleash a fury of validation checks. Are some tables remote? That's no problem, we'll interrogate them from afar. In essence, your DB tables can get the same line of questioning as your local data frames or those innocent-looking tibbles. Trust me, they'll start to talk and then they'll surely reveal what they're hiding after a pointblank session.

You don't have to type up a long report either, pointblank will take care of the paperwork. At the very least, you'll get a tidy data frame of the essentials. With a little planning, a very informative sitrep can be regularly produced.

## Examples

Ensure you have the dev version of `pointblank`

```r
devtools::install_github("rich-iannone/pointblank")
```

...then load the necessary packages

```r
library(pointblank)
library(tibble)
```

To illustrate a basic use of pointblank, make 2 very simple local tbls:

```r
tbl_1 <-
  tribble(
    ~a, ~b,   ~c,
    1,   6,   "h2adb",
    2,   7,   "h2spb",
    3,   8,   "h2df",
    4,   9,   "d3jwb",
    5,  10,   "h2esf")

tbl_2 <-
  tribble(
    ~d,   ~e,  ~f,
    "a",   0,  32,
    "b",   0,  31,
    "a",   1,  30,
    "a",   1,  32,
    "ae", -1,  39)
```

Now, perform a series of steps to validate these tables. Here's each step with some details on what each step does:

1. [new agent] create an agent with `create_agent()`
2. [new focus] `tbl_1` using `focus_on()`
3. [add a step] are vals in col `a` > 0?
4. [add a step] are all rows from `a, b, c` distinct?
5. [add a step] are vals in `col(a) + col(b)` >= 7?
6. [add a step] are column vals in `b` <= 10?
7. [add a step] do all `c` vals match regex `h2.*`?
8. [add a step] are all `c` substrings part of a set?
9. [new focus] `tbl_2` using `focus_on()`
10. [add a step] are all values in `d` part of a set?
11. [add a step] are all `d` values not part of a set?
12. [add a step] are column vals in `e` >= 0?
13. [add a step] are all `f` values NULL/NA?
14. [add a step] are all `f` values not NULL/NA?
15. [interrogation] perform all checks with `interrogate()`

```r
agent <- 
  create_agent() %>%             # (1)
  focus_on(
    tbl_name = "tbl_1") %>%      # (2)
  col_vals_gt(
    column = "a",
    value = 0) %>%               # (3)
  rows_not_duplicated(
    cols = "a, b, c") %>%        # (4)
  col_vals_gte(
    column = "a + b",
    value = 7) %>%               # (5)
  col_vals_lte(
    column = "b",
    value = 10) %>%              # (6)
  col_vals_regex(
    column = "c",
    regex = "h2.*") %>%          # (7)
  col_vals_in_set(
    column = "substr(c, 0, 2)",
    set = c("h2", "d3")) %>%     # (8)
  focus_on(
    tbl_name = "tbl_2") %>%      # (9)
  col_vals_in_set(
    column = "d",
    set = c("a", "b")) %>%       # (10)
  col_vals_not_in_set(
    column = "d",
    set = c("a", "b")) %>%       # (11)
  col_vals_gte(
    column = "e",
    value = 0) %>%               # (12)
  col_vals_null(
    column = "f") %>%            # (13)
  col_vals_not_null(
    column = "d") %>%            # (14)
  interrogate()                  # (15)
```

Get a summary of the interrogations, really just the basics. (Note that much more info is available within the `agent` object.)

```r
get_summary(agent)

#> # A tibble: 13 × 11
#>    tbl_name db_type       assertion_type
#>       <chr>   <chr>                <chr>
#> 1     tbl_1   local          col_vals_gt
#> 2     tbl_1   local  rows_not_duplicated
#> 3     tbl_1   local         col_vals_gte
#> 4     tbl_1   local         col_vals_lte
#> 5     tbl_1   local col_vals_not_between
#> 6     tbl_1   local       col_vals_regex
#> 7     tbl_1   local      col_vals_in_set
#> 8     tbl_2   local      col_vals_in_set
#> 9     tbl_2   local  col_vals_not_in_set
#> 10    tbl_2   local         col_vals_gte
#> 11    tbl_2   local        col_vals_null
#> 12    tbl_2   local    col_vals_not_null
#> 13    tbl_2   local     col_vals_between

#>              column value        set regex
#>               <chr> <dbl>     <list> <chr>
#> 1                 a     0     <NULL>  <NA>
#> 2           a, b, c    NA     <NULL>  <NA>
#> 3             a + b     7     <NULL>  <NA>
#> 4                 b    10     <NULL>  <NA>
#> 5                 b    NA <list [1]>  <NA>
#> 6                 c    NA     <NULL>  h2.*
#> 7   substr(c, 0, 2)    NA <list [1]>  <NA>
#> 8                 d    NA <list [1]>  <NA>
#> 9                 d    NA <list [1]>  <NA>
#> 10                e     0     <NULL>  <NA>
#> 11                f    NA     <NULL>  <NA>
#> 12                d    NA     <NULL>  <NA>
#> 13                e    NA <list [1]>  <NA>

#>     all_passed     n n_passed n_failed 
#>          <lgl> <int>    <int>    <dbl> 
#> 1         TRUE     5        5        0 
#> 2         TRUE     5        5        0 
#> 3         TRUE     5        5        0 
#> 4         TRUE     5        5        0 
#> 5        FALSE     5        3        2 
#> 6        FALSE     5        4        1 
#> 7         TRUE     5        5        0 
#> 8        FALSE     5        4        1 
#> 9        FALSE     5        1        4 
#> 10       FALSE     5        4        1 
#> 11       FALSE     5        1        4 
#> 12        TRUE     5        5        0 
#> 13       FALSE     5        4        1 
```

To validate tables in a database, first create a credentials RDS file in the working directory.

```r
create_creds_file(
  file = "pg_redshift_dev.rds",
  dbname = ***********,
  host = ***********************,
  port = ***,
  user = ********,
  password = **************)
```

A database table can be treated similarly to a local data frame. We also have the option to add some preparatory SQL (as statements following a `SELECT * FROM [table]` SQL line)

```r
agent <- 
  create_agent() %>%
  focus_on(
    tbl_name = "table_1",
    db_type = "PostgreSQL",
    creds_file = "pg_redshift_dev.rds",
    initial_sql = "WHERE date > '2017-01-15'") %>%
  rows_not_duplicated() %>%
  col_vals_gte(
    column = "a",
    value = 2) %>%
  col_vals_between(
    column = "b + c + d",
    left = 50,
    right = 100) %>%
  col_vals_not_null(
    column = "e") %>%
  interrogate()
```

Get a summary of the interrogations  

```r
get_summary(agent)
```

As can be seen, pointblank already proves to be an
effective tool in finding out whether your local data
frames or your database tables have unexpected values.
