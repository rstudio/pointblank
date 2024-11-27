# Select validation steps can be `active` or not

    Code
      obj <- small_table %>% col_is_character(columns = vars(b), actions = al) %>%
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

