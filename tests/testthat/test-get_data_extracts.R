test_that("Getting data extracts is possible", {

  # Use two validation step functions to create
  # two validation steps and then `interrogate()`
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate()

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(7, 7))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_equal(as.character(1:2))
  get_data_extracts(agent) %>% length() %>% expect_equal(2)
  get_data_extracts(agent) %>%
    vapply(
      FUN.VALUE = character(1), USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    ) %>%
    expect_equal(rep("tbl_df", 2))

  # Expect that using `get_data_extracts()` when
  # specifying `i` gives us the data extracts as a tbl
  get_data_extracts(agent, i = 1) %>% expect_s3_class("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_s3_class("tbl_df")

  # Expect an error if the `i` value isn't valid
  expect_error(get_data_extracts(agent, i = 3))

  # Use a single validation step function that doesn't
  # produce a data extract during `interrogate()`
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gte(columns = vars(a), value = 1) %>%
    interrogate()

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # no data extracts available
  expect_equal(summary$extract, NA_integer_)

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us an empty list in this case
  get_data_extracts(agent) %>% expect_equal(list())

  # Expect an error if the `i` value is valid but doesn't
  # have an associated data extract
  expect_error(get_data_extracts(agent, i = 1))

  # Expect an error if trying to get a data extract
  # from an agent with no intel
  expect_error(
    create_agent(tbl = small_table) %>%
      col_vals_gt(columns = vars(c), value = 5) %>%
      col_vals_lt(columns = vars(d), value = 1000) %>%
      get_data_extracts()
  )
})

test_that("Data extracts of different sizes are possible to create", {

  # Get only the first data extract value
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate(get_first_n = 1)

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(1, 1))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_equal(as.character(1:2))
  get_data_extracts(agent) %>% length() %>% expect_equal(2)
  get_data_extracts(agent) %>%
    vapply(
      FUN.VALUE = character(1), USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    ) %>%
    expect_equal(rep("tbl_df", 2))

  # Expect that using `get_data_extracts()` when
  # specifying `i` gives us the data extracts as a tbl
  get_data_extracts(agent, i = 1) %>% expect_s3_class("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_s3_class("tbl_df")

  # Expect only single rows in each of the extracts
  get_data_extracts(agent, i = 1) %>% nrow() %>% expect_equal(1)
  get_data_extracts(agent, i = 2) %>% nrow() %>% expect_equal(1)

  # Impose an overall sample row limit on each data extract;
  # this won't have any effect by itself
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate(sample_limit = 2)

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(7, 7))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_equal(as.character(1:2))
  get_data_extracts(agent) %>% length() %>% expect_equal(2)
  get_data_extracts(agent) %>%
    vapply(
      FUN.VALUE = character(1), USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    ) %>%
    expect_equal(rep("tbl_df", 2))

  # Expect that using `get_data_extracts()` when
  # specifying `i` gives us the data extracts as a tbl
  get_data_extracts(agent, i = 1) %>% expect_s3_class("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_s3_class("tbl_df")

  # Expect seven rows in each of the extracts
  get_data_extracts(agent, i = 1) %>% nrow() %>% expect_equal(7)
  get_data_extracts(agent, i = 2) %>% nrow() %>% expect_equal(7)

  # Use the `sample_n` option for each data extract,
  # expect that the `sample_limit` won't have any effect
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate(sample_n = 5, sample_limit = 2)

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(5, 5))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_equal(as.character(1:2))
  get_data_extracts(agent) %>% length() %>% expect_equal(2)
  get_data_extracts(agent) %>%
    vapply(
      FUN.VALUE = character(1), USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    ) %>%
    expect_equal(rep("tbl_df", 2))

  # Expect that using `get_data_extracts()` when
  # specifying `i` gives us the data extracts as a tbl
  get_data_extracts(agent, i = 1) %>% expect_s3_class("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_s3_class("tbl_df")

  # Expect seven rows in each of the extracts
  get_data_extracts(agent, i = 1) %>% nrow() %>% expect_equal(5)
  get_data_extracts(agent, i = 2) %>% nrow() %>% expect_equal(5)

  # Use the `sample_frac` option for each data extract,
  # expect that the `sample_limit` will impose a limit
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate(sample_frac = 0.75, sample_limit = 3)

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(3, 3))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_equal(as.character(1:2))
  get_data_extracts(agent) %>% length() %>% expect_equal(2)
  get_data_extracts(agent) %>%
    vapply(
      FUN.VALUE = character(1), USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    ) %>%
    expect_equal(rep("tbl_df", 2))

  # Expect that using `get_data_extracts()` when
  # specifying `i` gives us the data extracts as a tbl
  get_data_extracts(agent, i = 1) %>% expect_s3_class("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_s3_class("tbl_df")

  # Expect seven rows in each of the extracts
  get_data_extracts(agent, i = 1) %>% nrow() %>% expect_equal(3)
  get_data_extracts(agent, i = 2) %>% nrow() %>% expect_equal(3)

  # Use the `sample_frac` option for each data extract
  # with the default `sample_limit` value of 5000
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate(sample_frac = 0.75)

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(5, 5))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_equal(as.character(1:2))
  get_data_extracts(agent) %>% length() %>% expect_equal(2)
  get_data_extracts(agent) %>%
    vapply(
      FUN.VALUE = character(1), USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    ) %>%
    expect_equal(rep("tbl_df", 2))

  # Expect that using `get_data_extracts()` when
  # specifying `i` gives us the data extracts as a tbl
  get_data_extracts(agent, i = 1) %>% expect_s3_class("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_s3_class("tbl_df")

  # Expect seven rows in each of the extracts
  get_data_extracts(agent, i = 1) %>% nrow() %>% expect_equal(5)
  get_data_extracts(agent, i = 2) %>% nrow() %>% expect_equal(5)

  # Get no extracts (using `extract_failed = FALSE`)
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate(extract_failed = FALSE)

  # Get a summary of the interrogation
  summary <- get_agent_report(agent, display_table = FALSE)

  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(NA_integer_, NA_integer_))

  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_type("list")
  get_data_extracts(agent) %>% names() %>% expect_null()
  get_data_extracts(agent) %>% length() %>% expect_equal(0)
})

test_that("`rows_distinct()` extracts all columns", {

  agent <- create_agent(small_table) %>%
    rows_distinct(c(date, date_time)) %>%
    interrogate()

  expect_identical(
    colnames(get_data_extracts(agent)[[1]]),
    colnames(small_table)
  )

})
