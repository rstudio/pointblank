
run_snip <- function(snip_call, tbl) {
  (snip_call %>%
     rlang::f_rhs() %>%
     rlang::eval_tidy())(tbl)
}

test_that("the `snip_list()` function works", {
 
  expect_equal(
    run_snip(snip_list(column = "f"), small_table),
    "`\"high\"`, `\"low\"`, and `\"mid\"`"
  )
  expect_equal(
    run_snip(snip_list(column = "f", limit = 5), small_table),
    "`\"high\"`, `\"low\"`, and `\"mid\"`"
  )
  expect_equal(
    run_snip(snip_list(column = "f", limit = Inf), small_table),
    "`\"high\"`, `\"low\"`, and `\"mid\"`"
  )
  expect_equal(
    run_snip(snip_list(column = "f", limit = 2), small_table),
    "`\"high\"`, `\"low\"` (+1 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "f", limit = 1), small_table),
    "`\"high\"` (+2 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "c", limit = 5), small_table),
    "`3`, `8`, `NA`, `7`, `4` (+2 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "c", limit = 2), small_table),
    "`3`, `8` (+5 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "e", limit = 5), small_table),
    "`TRUE` and `FALSE`"
  )
  expect_equal(
    run_snip(snip_list(column = "e", limit = 1), small_table),
    "`TRUE` and `FALSE`"
  )
  expect_equal(
    run_snip(snip_list(column = "date_time", limit = 2), small_table),
    "`2016-01-04 11:00:00`, `2016-01-04 00:32:00` (+10 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "date", limit = 2), small_table),
    "`2016-01-04`, `2016-01-05` (+9 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "f", and_or = "or"), small_table),
    "`\"high\"`, `\"low\"`, or `\"mid\"`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, and_or = "and", lang = "de"),
      small_table
    ),
    "`2016-01-04`, `2016-01-05`, `2016-01-06` (+8 mehr)"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = TRUE),
      small_table
    ),
    "`\"2016-01-04\"`, `\"2016-01-05\"`, `\"2016-01-06\"` (+8 more)"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = FALSE),
      small_table
    ),
    "`2016-01-04`, `2016-01-05`, `2016-01-06` (+8 more)"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = TRUE),
      small_table
    ),
    "\"2016-01-04\", \"2016-01-05\", \"2016-01-06\" (+8 more)"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = FALSE),
      small_table
    ),
    "2016-01-04, 2016-01-05, 2016-01-06 (+8 more)"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = TRUE),
      small_table[2:3, ]
    ),
    "`\"2016-01-04\"` and `\"2016-01-05\"`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = FALSE),
      small_table[2:3, ]
    ),
    "`2016-01-04` and `2016-01-05`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = TRUE),
      small_table[2:3, ]
    ),
    "\"2016-01-04\" and \"2016-01-05\""
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = FALSE),
      small_table[2:3, ]
    ),
    "2016-01-04 and 2016-01-05"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = TRUE, lang = "fr"),
      small_table[2:3, ]
    ),
    "`\"2016-01-04\"` et `\"2016-01-05\"`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = FALSE, lang = "fr"),
      small_table[2:3, ]
    ),
    "`2016-01-04` et `2016-01-05`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = TRUE, lang = "fr"),
      small_table[2:3, ]
    ),
    "\"2016-01-04\" et \"2016-01-05\""
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = FALSE, lang = "fr"),
      small_table[2:3, ]
    ),
    "2016-01-04 et 2016-01-05"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = TRUE, lang = "fr"),
      small_table[2:4, ]
    ),
    "`\"2016-01-04\"`, `\"2016-01-05\"` et `\"2016-01-06\"`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = TRUE, quot_str = FALSE, lang = "fr"),
      small_table[2:4, ]
    ),
    "`2016-01-04`, `2016-01-05` et `2016-01-06`"
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = TRUE, lang = "fr"),
      small_table[2:4, ]
    ),
    "\"2016-01-04\", \"2016-01-05\" et \"2016-01-06\""
  )
  expect_equal(
    run_snip(
      snip_list(column = "date", limit = 3, as_code = FALSE, quot_str = FALSE, lang = "fr"),
      small_table[2:4, ]
    ),
    "2016-01-04, 2016-01-05 et 2016-01-06"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = FALSE, lang = "fr", oxford = TRUE
      ),
      small_table[2:4, ]
    ),
    "2016-01-04, 2016-01-05 et 2016-01-06"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = TRUE
      ),
      small_table[2:4, ]
    ),
    "2016-01-04, 2016-01-05, and 2016-01-06"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = FALSE
      ),
      small_table[2:4, ]
    ),
    "2016-01-04, 2016-01-05 and 2016-01-06"
  )
})

test_that("the `snip_lowest()` function works", {
  
  expect_equal(
    run_snip(snip_lowest(column = "a"), small_table),
    "1"
  )
  expect_equal(
    run_snip(snip_lowest(column = "b"), small_table),
    "1-bcd-345"
  )
  expect_equal(
    run_snip(snip_lowest(column = "c"), small_table),
    "2"
  )
  expect_equal(
    run_snip(snip_lowest(column = "d"), small_table),
    "108.34"
  )
  expect_equal(
    run_snip(snip_lowest(column = "e"), small_table),
    "0"
  )
  expect_equal(
    run_snip(snip_lowest(column = "f"), small_table),
    "high"
  )
})

test_that("the `snip_highest()` function works", {
  
  expect_equal(
    run_snip(snip_highest(column = "a"), small_table),
    "8"
  )
  expect_equal(
    run_snip(snip_highest(column = "b"), small_table),
    "8-kdg-938"
  )
  expect_equal(
    run_snip(snip_highest(column = "c"), small_table),
    "9"
  )
  expect_equal(
    run_snip(snip_highest(column = "d"), small_table),
    "9999.99"
  )
  expect_equal(
    run_snip(snip_highest(column = "e"), small_table),
    "1"
  )
  expect_equal(
    run_snip(snip_highest(column = "f"), small_table),
    "mid"
  )
})
