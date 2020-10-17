
run_snip <- function(snip_call, tbl) {
  (snip_call %>%
     rlang::f_rhs() %>%
     rlang::eval_tidy())(tbl)
}

test_that("the `snip_list()` function works", {
 
  expect_equal(
    run_snip(snip_list(column = "f"), small_table),
    "`\"high\"`, `\"low\"`, and `\"mid\"` "
  )
  expect_equal(
    run_snip(snip_list(column = "f", limit = 5), small_table),
    "`\"high\"`, `\"low\"`, and `\"mid\"` "
  )
  expect_equal(
    run_snip(snip_list(column = "f", limit = Inf), small_table),
    "`\"high\"`, `\"low\"`, and `\"mid\"` "
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
    "`\"3\"`, `\"8\"`, `\"NA\"`, `\"7\"`, `\"4\"` (+2 more)"
  )
  expect_equal(
    run_snip(snip_list(column = "c", limit = 2), small_table),
    "`\"3\"`, `\"8\"` (+5 more)"
  )
  
  expect_equal(
    run_snip(snip_list(column = "e", limit = 5), small_table),
    "`\"TRUE\"` and `\"FALSE\"`"
  )
  expect_equal(
    run_snip(snip_list(column = "e", limit = 1), small_table),
    "`\"TRUE\"` and `\"FALSE\"`"
  )
  
  expect_equal(
    run_snip(snip_list(column = "date_time", limit = 2), small_table),
    "`\"2016-01-04 11:00:00\"`, `\"2016-01-04 00:32:00\"` (+10 more)"
  )
  
  expect_equal(
    run_snip(snip_list(column = "date", limit = 2), small_table),
    "`\"2016-01-04\"`, `\"2016-01-05\"` (+9 more)"
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
