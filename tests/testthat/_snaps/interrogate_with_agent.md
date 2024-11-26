# Select validation steps can be `active` or not

    Code
      small_table %>% col_is_character(columns = vars(b), actions = al) %>%
        col_is_numeric(columns = vars(a), actions = al) %>% col_is_posix(columns = vars(
        date_time), actions = al) %>% col_is_date(columns = vars(date), actions = al) %>%
        col_is_integer(columns = vars(a), actions = al) %>% col_is_logical(columns = vars(
        e), actions = al) %>% col_vals_between(columns = vars(d), left = 0, right = 5000,
      actions = al) %>% col_vals_equal(columns = vars(d), value = 283.94, actions = al) %>%
        col_vals_gt(columns = vars(date_time), value = vars(date), actions = al) %>%
        col_vals_gte(columns = vars(date_time), value = vars(date), actions = al) %>%
        col_vals_lt(columns = vars(date_time), value = vars(date), actions = al) %>%
        col_vals_lte(columns = vars(date_time), value = vars(date), actions = al) %>%
        col_vals_in_set(columns = vars(f), set = c("low", "mid", "high"), actions = al) %>%
        col_vals_not_between(columns = vars(d), left = 500, right = 1000, actions = al) %>%
        col_vals_not_equal(columns = vars(d), value = 283.94, actions = al) %>%
        col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher"),
        actions = al) %>% col_vals_not_null(columns = vars(c), actions = al) %>%
        col_vals_null(columns = vars(b), actions = al) %>% col_vals_regex(columns = vars(
        f), regex = "[a-z]{3}", actions = al) %>% rows_distinct(actions = al) %>%
        conjointly(~ col_vals_gt(., columns = vars(a), value = 1), ~ col_vals_lt(.,
          columns = vars(c), value = 10, na_pass = TRUE), ~ col_vals_not_null(.,
          columns = vars(d)), actions = al) %>% serially(~ test_col_vals_gt(.,
        columns = vars(a), value = 0), ~ test_col_vals_lt(., columns = vars(c),
      value = 10, na_pass = TRUE), ~ col_vals_not_null(., columns = vars(d)),
      actions = al) %>% specially(fn = function(x) {
        as.integer(x$date) <= as.integer(x$date_time)
      }, actions = al)
    Condition
      Warning:
      Failure to validate that column `a` is of type: numeric.
      The `col_is_numeric()` validation failed beyond the absolute threshold level (1).
      * failure level (1) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `d` should have been between `0` and `5000`.
      The `col_vals_between()` validation failed beyond the absolute threshold level (1).
      * failure level (1) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `d` should have been == `283.94`.
      The `col_vals_equal()` validation failed beyond the absolute threshold level (1).
      * failure level (12) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `date_time` should have been < `~date`.
      The `col_vals_lt()` validation failed beyond the absolute threshold level (1).
      * failure level (13) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `date_time` should have been <= `~date`.
      The `col_vals_lte()` validation failed beyond the absolute threshold level (1).
      * failure level (13) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `d` should not have been between `500` and `1000`.
      The `col_vals_not_between()` validation failed beyond the absolute threshold level (1).
      * failure level (4) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `d` should have been != `283.94`.
      The `col_vals_not_equal()` validation failed beyond the absolute threshold level (1).
      * failure level (1) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `c` should not have been NULL.
      The `col_vals_not_null()` validation failed beyond the absolute threshold level (1).
      * failure level (2) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where values in `b` should have been NULL.
      The `col_vals_null()` validation failed beyond the absolute threshold level (1).
      * failure level (13) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where there weren't distinct rows across all columns.
      The `rows_distinct()` validation failed beyond the absolute threshold level (1).
      * failure level (2) >= failure threshold (1)
      Warning:
      Exceedance of failed test units where there should have been conjoint 'pass' units across the following expressions: `~col_vals_gt(., columns = vars(a), value = 1)`, `~col_vals_lt(., columns = vars(c), value = 10, na_pass = TRUE)`, `~col_vals_not_null(., columns = vars(d))`.
      The `conjointly()` validation failed beyond the absolute threshold level (1).
      * failure level (1) >= failure threshold (1)
    Output
      # A tibble: 13 x 8
         date_time           date           a b             c      d e     f    
         <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
       1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
       2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
       3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
       4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
       5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
       6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
       7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
       8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
       9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
      10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
      11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-010     7   834. TRUE  low  
      12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-010     8   108. FALSE low  
      13 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA  2230. TRUE  high 

