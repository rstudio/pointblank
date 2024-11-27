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
  expect_equal(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = TRUE, sep = ";"
      ),
      small_table[2:4, ]
    ),
    "2016-01-04; 2016-01-05; and 2016-01-06"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = FALSE, sep = ";"
      ),
      small_table[2:4, ]
    ),
    "2016-01-04; 2016-01-05 and 2016-01-06"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = TRUE, sep = "..."
      ),
      small_table
    ),
    "2016-01-04... 2016-01-05... 2016-01-06... 2016-01-09... 2016-01-11 (+6 more)"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", limit = 3, reverse = TRUE, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = FALSE, sep = ";"
      ),
      small_table[2:4, ]
    ),
    "2016-01-06; 2016-01-05 and 2016-01-04"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "date", reverse = TRUE, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = TRUE
      ),
      small_table
    ),
    "2016-01-30, 2016-01-28, 2016-01-26, 2016-01-20, 2016-01-17 (+6 more)"
  )

  # Tests of the `sorting` options
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inorder", reverse = FALSE,
        as_code = FALSE, quot_str = FALSE, oxford = TRUE
      ),
      small_table
    ),
    "2, 3, 6, 8, 4 (+2 more)"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inorder", reverse = TRUE,
        as_code = FALSE, quot_str = FALSE, oxford = TRUE
      ),
      small_table
    ),
    "1, 7, 4, 8, 6 (+2 more)"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "infreq", reverse = FALSE,
        as_code = FALSE, quot_str = FALSE, oxford = TRUE
      ),
      small_table
    ),
    "2, 3, 4, 1, 6 (+2 more)"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "infreq", reverse = TRUE,
        as_code = FALSE, quot_str = FALSE, oxford = TRUE
      ),
      small_table
    ),
    "1, 6, 7, 8, 2 (+2 more)"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inseq", reverse = FALSE,
        as_code = FALSE, quot_str = FALSE, oxford = TRUE
      ),
      small_table
    ),
    "1, 2, 3, 4, 6 (+2 more)"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inseq", reverse = TRUE,
        as_code = FALSE, quot_str = FALSE, oxford = TRUE
      ),
      small_table
    ),
    "8, 7, 6, 4, 3 (+2 more)"
  )

  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inorder"
      ),
      data.frame(a = rep(NA_character_, 5))
    ),
    "`\"NA\"`"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inorder"
      ),
      data.frame(a = rep(NA, 5))
    ),
    "`NA`"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inorder"
      ),
      data.frame(a = rep(NA_real_, 5))
    ),
    "`NA`"
  )

  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inseq"
      ),
      data.frame(a = rep(NA_character_, 5))
    ),
    "---"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inseq"
      ),
      data.frame(a = rep(NA, 5))
    ),
    "---"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "inseq"
      ),
      data.frame(a = rep(NA_real_, 5))
    ),
    "---"
  )

  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "infreq"
      ),
      data.frame(a = rep(NA_character_, 5))
    ),
    "`\"NA\"`"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "infreq"
      ),
      data.frame(a = rep(NA, 5))
    ),
    "`NA`"
  )
  expect_equal(
    run_snip(
      snip_list(
        column = "a", sorting = "infreq"
      ),
      data.frame(a = rep(NA_real_, 5))
    ),
    "`NA`"
  )

  for (lang in reporting_languages) {
    expect_match(
      run_snip(
        snip_list(
          column = "date", limit = 3,
          as_code = FALSE, quot_str = FALSE,
          and_or = "and", lang = lang
        ),
        small_table[2:4,]
      ),
      gsub(" ", "", get_lsv("informant_report/snip_list_and")[[lang]])
    )
  }

  for (lang in reporting_languages) {
    expect_match(
      run_snip(
        snip_list(
          column = "date", limit = 3,
          as_code = FALSE, quot_str = FALSE,
          and_or = "or", lang = lang
        ),
        small_table[2:4,]
      ),
      gsub(" ", "", get_lsv("informant_report/snip_list_or")[[lang]])
    )
  }

  for (lang in reporting_languages) {
    expect_match(
      run_snip(
        snip_list(
          column = "date", limit = 3,
          as_code = FALSE, quot_str = FALSE,
          and_or = "or", lang = lang
        ),
        small_table[2:4,]
      ),
      gsub(" ", "", get_lsv("informant_report/snip_list_or")[[lang]])
    )
  }

  for (lang in reporting_languages) {
    expect_match(
      run_snip(
        snip_list(
          column = "a", sorting = "inseq", reverse = TRUE,
          as_code = FALSE, quot_str = FALSE,
          lang = lang
        ),
        small_table
      ),
      gsub(
        "{n_items}", "2",
        get_lsv("informant_report/snip_list_more")[[lang]],
        fixed = TRUE
      )
    )
  }

  # Expect an error if the `sep` value is not character
  expect_error(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = FALSE, lang = "en", oxford = TRUE, sep = 5
      ),
      small_table[2:4, ]
    )
  )

  # Expect an error if `quot_str` isn't TRUE, FALSE, or NULL
  expect_error(
    run_snip(
      snip_list(
        column = "date", limit = 3, as_code = FALSE,
        quot_str = "~", lang = "fr"
      ),
      small_table[2:4, ]
    )
  )

  # Expect an error if `sorting` isn't `"inorder"`, `"infreq"`, or `"inseq"`
  expect_error(
    run_snip(
      snip_list(
        column = "a", sorting = "frequency"
      ),
      small_table
    )
  )

  # Expect an error if `sorting` isn't of length 1
  expect_error(
    run_snip(
      snip_list(
        column = "a", sorting = c("inorder", "inseq")
      ),
      small_table
    )
  )

  # Expect an error if `lang` isn't one of the supported languages
  expect_error(
    run_snip(
      snip_list(
        column = "a", lang = "aa"
      ),
      small_table
    )
  )

  # na_rm ignores NA values in list
  expect_match(
    run_snip(
      snip_list(column = "f", na_rm = FALSE),
      small_table %>% dplyr::mutate(f = ifelse(f == "high", NA, f))
    ),
    "NA"
  )
  expect_no_match(
    run_snip(
      snip_list(column = "f", na_rm = TRUE),
      small_table %>% dplyr::mutate(f = ifelse(f == "high", NA, f))
    ),
    "NA"
  )
  expect_equal(
    run_snip(
      snip_list(column = "f", na_rm = TRUE),
      small_table %>% dplyr::mutate(f = NA)
    ),
    "---"
  )

})

test_that("the `snip_stats()` function works", {

  expect_match(
    as.character(
      run_snip(snip_stats(column = "a"), small_table)
    ),
    paste0(
      ".*?",
      "Minimum.*?&#10122;.*?1.*?",
      "Q1.*?&#10123;.*?2.*?",
      "Median.*?&#10124;.*?3.*?",
      "Q3.*?&#10125;.*?4.*?",
      "Maximum.*?&#10126;.*?8.*?"
    )
  )

  expect_match(
    as.character(
      run_snip(snip_stats(column = "a", type = "7num"), small_table)
    ),
    paste0(
      ".*?",
      "P2.*?&#10122;.*?1.24.*?",
      "P9.*?&#10123;.*?2.*?",
      "Q1.*?&#10124;.*?2.*?",
      "Median.*?&#10125;.*?3.*?",
      "Q3.*?&#10126;.*?4.*?",
      "P91.*?&#10127;.*?6.92.*?",
      "P98.*?&#10128;.*?7.76.*?"
    )
  )

  expect_match(
    as.character(
      run_snip(snip_stats(column = "a", type = "bowley"), small_table)
    ),
    paste0(
      ".*?",
      "Minimum.*?&#10122;.*?1.*?",
      "P10.*?&#10123;.*?2.*?",
      "Q1.*?&#10124;.*?2.*?",
      "Median.*?&#10125;.*?3.*?",
      "Q3.*?&#10126;.*?4.*?",
      "P90.*?&#10127;.*?6.8.*?",
      "Maximum.*?&#10128;.*?8.*?"
    )
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
