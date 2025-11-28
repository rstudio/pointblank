# Generate a table column schema manually or with a reference table

A table column schema object, as can be created by `col_schema()`, is
necessary when using the
[`col_schema_match()`](https://rstudio.github.io/pointblank/reference/col_schema_match.md)
validation function (which checks whether the table object under study
matches a known column schema). The `col_schema` object can be made by
carefully supplying the column names and their types as a set of named
arguments, or, we could provide a table object, which could be of the
`data.frame`, `tbl_df`, `tbl_dbi`, or `tbl_spark` varieties. There's an
additional option, which is just for validating the schema of a
`tbl_dbi` or `tbl_spark` object: we can validate the schema based on R
column types (e.g., `"numeric"`, `"character"`, etc.), SQL column types
(e.g., `"double"`, `"varchar"`, etc.), or Spark SQL column types
(`"DoubleType"`, `"StringType"`, etc.). This is great if we want to
validate table column schemas both on the server side and when tabular
data is collected and loaded into R.

## Usage

``` r
col_schema(..., .tbl = NULL, .db_col_types = c("r", "sql"))
```

## Arguments

- ...:

  *Column-by-column schema definition*

  `<multiple expressions>` // **required** (or, use `.tbl`)

  A set of named arguments where the names refer to column names and the
  values are one or more column types.

- .tbl:

  *A data table for defining a schema*

  `obj:<tbl_*>` // **optional**

  An option to use a table object to define the schema. If this is
  provided then any values provided to `...` will be ignored. This can
  either be a table object, a table-prep formula.This can be a table
  object such as a data frame, a tibble, a `tbl_dbi` object, or a
  `tbl_spark` object. Alternatively, a table-prep formula
  (`~ <tbl reading code>`) or a function
  (`function() <tbl reading code>`) can be used to lazily read in the
  table at interrogation time.

- .db_col_types:

  *Use R column types or database column types?*

  `singl-kw:[r|sql]` // *default:* `"r"`

  Determines whether the column types refer to R column types (`"r"`) or
  SQL column types (`"sql"`).

## Examples

Create a simple table with two columns: one `integer` and the other
`character`.

    tbl <-
      dplyr::tibble(
        a = 1:5,
        b = letters[1:5]
      )

    tbl
    #> # A tibble: 5 x 2
    #>       a b
    #>   <int> <chr>
    #> 1     1 a
    #> 2     2 b
    #> 3     3 c
    #> 4     4 d
    #> 5     5 e

Create a column schema object that describes the columns and their types
(in the expected order).

    schema_obj <-
      col_schema(
        a = "integer",
        b = "character"
      )

    schema_obj
    #> $a
    #> [1] "integer"
    #>
    #> $b
    #> [1] "character"
    #>
    #> attr(,"class")
    #> [1] "r_type"     "col_schema"

Validate that the schema object `schema_obj` exactly defines the column
names and column types of the `tbl` table.

    agent <-
      create_agent(tbl = tbl) %>%
      col_schema_match(schema_obj) %>%
      interrogate()

Determine if this validation step passed by using
[`all_passed()`](https://rstudio.github.io/pointblank/reference/all_passed.md).

    all_passed(agent)

    ## [1] TRUE

We can alternatively create a column schema object from a `tbl_df`
object.

    schema_obj <-
      col_schema(
        .tbl = dplyr::tibble(
          a = integer(0),
          b = character(0)
        )
      )

This should provide the same interrogation results as in the previous
example.

    create_agent(tbl = tbl) %>%
      col_schema_match(schema_obj) %>%
      interrogate() %>%
      all_passed()

    ## [1] TRUE

## Function ID

13-1

## See also

Other Utility and Helper Functions:
[`affix_date()`](https://rstudio.github.io/pointblank/reference/affix_date.md),
[`affix_datetime()`](https://rstudio.github.io/pointblank/reference/affix_datetime.md),
[`from_github()`](https://rstudio.github.io/pointblank/reference/from_github.md),
[`has_columns()`](https://rstudio.github.io/pointblank/reference/has_columns.md),
[`stop_if_not()`](https://rstudio.github.io/pointblank/reference/stop_if_not.md)
