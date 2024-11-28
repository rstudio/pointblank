file_name <- "the_file"
file_name_ext <- "the_file.txt"

test_that("affixing a date to a filename works well", {

  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = TRUE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = FALSE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = FALSE
    ),
    "the_file_[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = FALSE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = TRUE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "--", utc_time = FALSE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "--", utc_time = FALSE
    ),
    "the_file--[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "--", utc_time = FALSE
    ),
    "the_file--[0-9]{2}-[0-9]{2}-20[0-9]{2}.txt"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "end", format = "%Y-%m-%d",
      delimiter = "--", utc_time = TRUE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "end", format = "%m-%d",
      delimiter = "--", utc_time = TRUE
    ),
    "the_file--[0-9]{2}-[0-9]{2}"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "end", format = "%m-%d-%Y",
      delimiter = "--", utc_time = TRUE
    ),
    "the_file--[0-9]{2}-[0-9]{2}-20[0-9]{2}"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "start", format = "%Y-%m-%d",
      delimiter = "--", utc_time = FALSE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}--the_file.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "start", format = "%m-%d",
      delimiter = "--", utc_time = FALSE
    ),
    "[0-9]{2}-[0-9]{2}--the_file.txt"
  )
  expect_match(
    affix_date(
      filename = file_name_ext, position = "start", format = "%m-%d-%Y",
      delimiter = "--", utc_time = FALSE
    ),
    "[0-9]{2}-[0-9]{2}-20[0-9]{2}--the_file.txt"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "start", format = "%Y-%m-%d",
      delimiter = "--", utc_time = TRUE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}--the_file"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "start", format = "%m-%d",
      delimiter = "--", utc_time = TRUE
    ),
    "[0-9]{2}-[0-9]{2}--the_file"
  )
  expect_match(
    affix_date(
      filename = file_name, position = "start", format = "%m-%d-%Y",
      delimiter = "--", utc_time = TRUE
    ),
    "[0-9]{2}-[0-9]{2}-20[0-9]{2}--the_file"
  )
})

test_that("affixing a datetime to a filename works well", {

  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%dT%H:%M:%S",
      delimiter = "_", utc_time = TRUE, add_tz = TRUE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = TRUE, add_tz = TRUE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}Z.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = TRUE, add_tz = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}Z.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = TRUE, add_tz = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}Z.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%dT%H:%M:%S",
      delimiter = "_", utc_time = FALSE, add_tz = TRUE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}((+|-)[0-9]{4}|Z).txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = FALSE, add_tz = TRUE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}((+|-)[0-9]{4}|Z).txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = FALSE, add_tz = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}((+|-)[0-9]{4}|Z).txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = FALSE, add_tz = TRUE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}((+|-)[0-9]{4}|Z).txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end",
      delimiter = "--", utc_time = TRUE, add_tz = TRUE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}Z.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end",
      delimiter = "--", utc_time = FALSE, add_tz = TRUE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}((+|-)[0-9]{4}|Z).txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "start",
      delimiter = "--", utc_time = TRUE, add_tz = TRUE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}Z--the_file.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "start",
      delimiter = "--", utc_time = FALSE, add_tz = TRUE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}((+|-)[0-9]{4}|Z)--the_file.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "end",
      delimiter = "--", utc_time = TRUE, add_tz = TRUE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}Z"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "end",
      delimiter = "--", utc_time = FALSE, add_tz = TRUE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}((+|-)[0-9]{4}|Z)"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "start",
      delimiter = "--", utc_time = TRUE, add_tz = TRUE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}Z--the_file"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "start",
      delimiter = "--", utc_time = FALSE, add_tz = TRUE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}((+|-)[0-9]{4}|Z)--the_file"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%dT%H:%M:%S",
      delimiter = "_", utc_time = TRUE, add_tz = FALSE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = TRUE, add_tz = FALSE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = TRUE, add_tz = FALSE
    ),
    "the_file_[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = TRUE, add_tz = FALSE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%dT%H:%M:%S",
      delimiter = "_", utc_time = FALSE, add_tz = FALSE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%Y-%m-%d",
      delimiter = "_", utc_time = FALSE, add_tz = FALSE
    ),
    "the_file_20[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d",
      delimiter = "_", utc_time = FALSE, add_tz = FALSE
    ),
    "the_file_[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end", format = "%m-%d-%Y",
      delimiter = "_", utc_time = FALSE, add_tz = FALSE
    ),
    "the_file_[0-9]{2}-[0-9]{2}-20[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end",
      delimiter = "--", utc_time = TRUE, add_tz = FALSE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "end",
      delimiter = "--", utc_time = FALSE, add_tz = FALSE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "start",
      delimiter = "--", utc_time = TRUE, add_tz = FALSE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}--the_file.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name_ext, position = "start",
      delimiter = "--", utc_time = FALSE, add_tz = FALSE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}--the_file.txt"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "end",
      delimiter = "--", utc_time = TRUE, add_tz = FALSE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "end",
      delimiter = "--", utc_time = FALSE, add_tz = FALSE
    ),
    "the_file--20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "start",
      delimiter = "--", utc_time = TRUE, add_tz = FALSE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}--the_file"
  )
  expect_match(
    affix_datetime(
      filename = file_name, position = "start",
      delimiter = "--", utc_time = FALSE, add_tz = FALSE
    ),
    "20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}--the_file"
  )
})
