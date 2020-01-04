context("Getting data extracts")

test_that("Getting data extracts is possible", {
  
  small_table <-
    readr::read_csv(
      system.file("extdata", "small_table.csv", package = "pointblank"),
      col_types = "TDicddlc")
  
  # Use two validation step functions to create
  # two validation steps and then `interrogate()`
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000) %>%
    interrogate()
  
  # Get a summary of the interrogation
  summary <- get_agent_report(agent)
  
  # Expect that the summary shows there are
  # two data extracts available
  expect_equal(summary$extract, c(TRUE, TRUE))
  
  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us a list of all data extracts
  get_data_extracts(agent) %>% expect_is("list")
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
  get_data_extracts(agent, i = 1) %>% expect_is("tbl_df")
  get_data_extracts(agent, i = 2) %>% expect_is("tbl_df")
  
  # Expect an error if the `i` value isn't valid
  expect_error(get_data_extracts(agent, i = 3))

  
  # Use a single validation step function that doesn't
  # produce a data extract during `interrogate()`
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gte(columns = vars(a), value = 1) %>%
    interrogate()
  
  # Get a summary of the interrogation
  summary <- get_agent_report(agent)
  
  # Expect that the summary shows there are
  # no data extracts available
  expect_equal(summary$extract, FALSE)
  
  # Expect that using `get_data_extracts()` without
  # specifying `i` gives us an empty list in this case
  get_data_extracts(agent) %>% expect_equivalent(list())
  
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
