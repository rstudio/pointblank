# Table Transformer: shift the times of a table

With any table object containing date or date-time columns, these values
can be precisely shifted with `tt_time_shift()` and specification of the
time shift. We can either provide a string with the time shift
components and the shift direction (like `"-4y 10d"`) or a `difftime`
object (which can be created via **lubridate** expressions or by using
the [`base::difftime()`](https://rdrr.io/r/base/difftime.html)
function).

## Usage

``` r
tt_time_shift(tbl, time_shift = "0y 0m 0d 0H 0M 0S")
```

## Arguments

- tbl:

  *A data table*

  `obj:<tbl_*>` // **required**

  A table object to be used as input for the transformation. This can be
  a data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.

- time_shift:

  *Time-shift specification*

  `scalar<character>` // *default:* `"0y 0m 0d 0H 0M 0S"`

  Either a character-based representation that specifies the time
  difference by which all time values in time-based columns will be
  shifted, or, a `difftime` object. The character string is constructed
  in the format `"0y 0m 0d 0H 0M 0S"` and individual time components can
  be omitted (i.e., `"1y 5d"` is a valid specification of shifting time
  values ahead one year and five days). Adding a `"-"` at the beginning
  of the string (e.g., `"-2y"`) will shift time values back.

## Value

A data frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object
depending on what was provided as `tbl`.

## Details

The `time_shift` specification cannot have a higher time granularity
than the least granular time column in the input table. Put in simpler
terms, if there are any date-based based columns (or just a single
date-based column) then the time shifting can only be in terms of years,
months, and days. Using a `time_shift` specification of `"20d 6H"` in
the presence of any dates will result in a truncation to `"20d"`.
Similarly, a `difftime` object will be altered in the same
circumstances, however, the object will resolved to an exact number of
days through rounding.

## Examples

Let's use the `game_revenue` dataset, included in the **pointblank**
package, as the input table for the first demo. It has entries in the
first 21 days of 2015 and we'll move all of the date and date-time
values to the beginning of 2021 with the `tt_time_shift()` function and
the `"6y"` `time_shift` specification.

    tt_time_shift(
      tbl = game_revenue,
      time_shift = "6y"
    )
    #> # A tibble: 2,000 x 11
    #>    player_id       session_id  session_start       time                item_type
    #>    <chr>           <chr>       <dttm>              <dttm>              <chr>
    #>  1 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 01:31:03 2021-01-01 01:31:27 iap
    #>  2 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 01:31:03 2021-01-01 01:36:57 iap
    #>  3 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 01:31:03 2021-01-01 01:37:45 iap
    #>  4 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 01:31:03 2021-01-01 01:42:33 ad
    #>  5 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 11:50:02 2021-01-01 11:55:20 ad
    #>  6 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 11:50:02 2021-01-01 12:08:56 ad
    #>  7 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 11:50:02 2021-01-01 12:14:08 ad
    #>  8 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 11:50:02 2021-01-01 12:21:44 ad
    #>  9 ECPANOIXLZHF896 ECPANOIXLZ~ 2021-01-01 11:50:02 2021-01-01 12:24:20 ad
    #> 10 FXWUORGYNJAE271 FXWUORGYNJ~ 2021-01-01 15:17:18 2021-01-01 15:19:36 ad
    #> # i 1,990 more rows
    #> # i 6 more variables: item_name <chr>, item_revenue <dbl>,
    #> #   session_duration <dbl>, start_day <date>, acquisition <chr>, country <chr>

Keeping only the `date_time` and `a`-`f` columns of `small_table`, also
included in the package, shift the times back 2 days and 12 hours with
the `"-2d 12H"` specification.

    small_table %>%
      dplyr::select(-date) %>%
      tt_time_shift("-2d 12H")
    #> # A tibble: 13 x 7
    #>    date_time               a b             c      d e     f
    #>    <dttm>              <int> <chr>     <dbl>  <dbl> <lgl> <chr>
    #>  1 2016-01-01 23:00:00     2 1-bcd-345     3  3423. TRUE  high
    #>  2 2016-01-01 12:32:00     3 5-egh-163     8 10000. TRUE  low
    #>  3 2016-01-03 01:32:00     6 8-kdg-938     3  2343. TRUE  high
    #>  4 2016-01-04 05:23:00     2 5-jdo-903    NA  3892. FALSE mid
    #>  5 2016-01-07 00:36:00     8 3-ldm-038     7   284. TRUE  low
    #>  6 2016-01-08 18:15:00     4 2-dhe-923     4  3291. TRUE  mid
    #>  7 2016-01-13 06:46:00     7 1-knw-093     3   843. TRUE  high
    #>  8 2016-01-14 23:27:00     4 5-boe-639     2  1036. FALSE low
    #>  9 2016-01-17 16:30:00     3 5-bce-642     9   838. FALSE high
    #> 10 2016-01-17 16:30:00     3 5-bce-642     9   838. FALSE high
    #> 11 2016-01-24 08:07:00     4 2-dmx-010     7   834. TRUE  low
    #> 12 2016-01-25 14:51:00     2 7-dmx-010     8   108. FALSE low
    #> 13 2016-01-27 23:23:00     1 3-dka-303    NA  2230. TRUE  high

## Function ID

12-5

## See also

Other Table Transformers:
[`get_tt_param()`](https://rstudio.github.io/pointblank/reference/get_tt_param.md),
[`tt_string_info()`](https://rstudio.github.io/pointblank/reference/tt_string_info.md),
[`tt_summary_stats()`](https://rstudio.github.io/pointblank/reference/tt_summary_stats.md),
[`tt_tbl_colnames()`](https://rstudio.github.io/pointblank/reference/tt_tbl_colnames.md),
[`tt_tbl_dims()`](https://rstudio.github.io/pointblank/reference/tt_tbl_dims.md),
[`tt_time_slice()`](https://rstudio.github.io/pointblank/reference/tt_time_slice.md)
