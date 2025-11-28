# Table Transformer: slice a table with a slice point on a time column

With any table object containing date, date-time columns, or a mixture
thereof, any one of those columns can be used to effectively slice the
data table in two with a `slice_point`: and you get to choose which of
those slices you want to keep. The slice point can be defined in several
ways. One method involves using a decimal value between `0` and `1`,
which defines the slice point as the time instant somewhere between the
earliest time value (at `0`) and the latest time value (at `1`). Another
way of defining the slice point is by supplying a time value, and the
following input types are accepted: (1) an ISO 8601 formatted time
string (as a date or a date-time), (2) a `POSIXct` time, or (3) a `Date`
object.

## Usage

``` r
tt_time_slice(
  tbl,
  time_column = NULL,
  slice_point = 0,
  keep = c("left", "right"),
  arrange = FALSE
)
```

## Arguments

- tbl:

  *A data table*

  `obj:<tbl_*>` // **required**

  A table object to be used as input for the transformation. This can be
  a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.

- time_column:

  *Column with time data*

  `scalar<character>` // *default:* `NULL` (`optional`)

  The time-based column that will be used as a basis for the slicing. If
  no time column is provided then the first one found will be used.

- slice_point:

  `scalar<numeric|character|POSIXct|Date>` // *default:* `0`

  The location on the `time_column` where the slicing will occur. This
  can either be a decimal value from `0` to `1`, an ISO 8601 formatted
  time string (as a date or a date-time), a `POSIXct` time, or a `Date`
  object.

- keep:

  *Data slice to keep*

  `singl-kw:[left|right]` // *default:* `"left"`

  Which slice should be kept? The `"left"` side (the default) contains
  data rows that are earlier than the `slice_point` and the `"right"`
  side will have rows that are later.

- arrange:

  *Arrange data slice by the time data?*

  `scalar<logical>` // *default:* `FALSE`

  Should the slice be arranged by the `time_column`? This may be useful
  if the input `tbl` isn't ordered by the `time_column`. By default,
  this is `FALSE` and the original ordering is retained.

## Value

A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
depending on what was provided as `tbl`.

## Details

There is the option to `arrange` the table by the date or date-time
values in the `time_column`. This ordering is always done in an
ascending manner. Any `NA`/`NULL` values in the `time_column` will
result in the corresponding rows can being removed (no matter which
slice is retained).

## Examples

Let's use the `game_revenue` dataset, included in the **pointblank**
package, as the input table for the first demo. It has entries in the
first 21 days of 2015 and we'll elect to get all of the records where
the `time` values are strictly for the first 15 days of 2015. The `keep`
argument has a default of `"left"` so all rows where the `time` column
is less than `"2015-01-16 00:00:00"` will be kept.

    tt_time_slice(
      tbl = game_revenue,
      time_column = "time",
      slice_point = "2015-01-16"
    )
    #> # A tibble: 1,208 x 11
    #>    player_id       session_id  session_start       time                item_type
    #>    <chr>           <chr>       <dttm>              <dttm>              <chr>
    #>  1 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 01:31:03 2015-01-01 01:31:27 iap
    #>  2 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 01:31:03 2015-01-01 01:36:57 iap
    #>  3 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 01:31:03 2015-01-01 01:37:45 iap
    #>  4 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 01:31:03 2015-01-01 01:42:33 ad
    #>  5 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 11:50:02 2015-01-01 11:55:20 ad
    #>  6 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 11:50:02 2015-01-01 12:08:56 ad
    #>  7 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 11:50:02 2015-01-01 12:14:08 ad
    #>  8 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 11:50:02 2015-01-01 12:21:44 ad
    #>  9 ECPANOIXLZHF896 ECPANOIXLZ~ 2015-01-01 11:50:02 2015-01-01 12:24:20 ad
    #> 10 FXWUORGYNJAE271 FXWUORGYNJ~ 2015-01-01 15:17:18 2015-01-01 15:19:36 ad
    #> # i 1,198 more rows
    #> # i 6 more variables: item_name <chr>, item_revenue <dbl>,
    #> #   session_duration <dbl>, start_day <date>, acquisition <chr>, country <chr>

Omit the first 25% of records from `small_table`, also included in the
package, with a fractional `slice_point` of `0.25` on the basis of a
timeline that begins at `2016-01-04 11:00:00` and ends at
`2016-01-30 11:23:00`.

    small_table %>%
      tt_time_slice(
        slice_point = 0.25,
        keep = "right"
      )
    #> # A tibble: 8 x 8
    #>   date_time           date           a b             c     d e     f
    #>   <dttm>              <date>     <int> <chr>     <dbl> <dbl> <lgl> <chr>
    #> 1 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4 3291. TRUE  mid
    #> 2 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high
    #> 3 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2 1036. FALSE low
    #> 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    #> 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high
    #> 6 2016-01-26 20:07:00 2016-01-26     4 2-dmx-010     7  834. TRUE  low
    #> 7 2016-01-28 02:51:00 2016-01-28     2 7-dmx-010     8  108. FALSE low
    #> 8 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high

## Function ID

12-6

## See also

Other Table Transformers:
[`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md),
[`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md),
[`tt_summary_stats()`](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md),
[`tt_tbl_colnames()`](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md),
[`tt_tbl_dims()`](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md),
[`tt_time_shift()`](https://rstudio.github.io/pointblank/reference/tt_time_shift.md)
