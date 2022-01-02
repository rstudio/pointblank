library(RSQLite)

temp_dir <- tempdir()

test_that("The `x_write_disk()` and `x_read_disk()` functions works as expected", {
  
  # Create an agent with a single validation step, using
  # `small_table` as the `tbl`; interrogate the data
  agent <-
    create_agent(tbl = small_table) %>%
    rows_distinct() %>%
    interrogate()
  
  # Expect `extracts` to contain entries in the agent object
  expect_length(agent$extracts, 1)
  expect_equal(names(agent$extracts), "1")
  expect_equal(
    names(agent$extracts[[1]]),
    c("date_time", "date", "a", "b", "c", "d", "e", "f")
  )
  
  # Expect the first entry of the `tbl_checked` column in the `validation_set`
  # tibble to itself be a tibble
  expect_s3_class(
    agent %>% 
      unclass() %>% 
      .$validation_set %>% 
      .$tbl_checked %>% 
      .[[1]] %>% 
      .[[1]],
    "tbl_df"
  )
  
  # Write the agent to disk, don't elect to keep the table
  agent %>% x_write_disk(filename = "agent_test_1", path = temp_dir)
  
  # Expect that the file was written to the temp directory
  expect_true("agent_test_1" %in% list.files(path = temp_dir))
  
  # Read the agent back with `x_read_disk()`
  agent_test_1 <- x_read_disk(filename = file.path(temp_dir, "agent_test_1"))
  
  # Expect a `ptblank_agent` object
  expect_s3_class(agent_test_1, "ptblank_agent")
  expect_s3_class(agent_test_1, "has_intel")
  
  # Don't expect the `tbl` data to be in the agent object
  expect_null(agent_test_1$tbl)
  
  # Set the table as `small_table` and then expect the `tbl`
  # data to be present again
  agent_test_1 <- agent_test_1 %>% set_tbl(small_table)
  expect_s3_class(agent_test_1$tbl, "tbl_df")
  
  # Don't expect the `extracts` to be in the agent object
  expect_null(agent_test_1$extracts)
  
  # Expect the first entry of the `tbl_checked` column in the `validation_set`
  # tibble to be NULL
  expect_null(
    agent_test_1 %>% 
      unclass() %>% 
      .$validation_set %>% 
      .$tbl_checked %>% 
      .[[1]]
  )
  
  # Write the agent to disk again, but choose to keep the table
  agent %>% 
    x_write_disk(filename = "agent_test_2", path = temp_dir, keep_tbl = TRUE)
  
  # Expect that the file was written to the temp directory
  expect_true("agent_test_2" %in% list.files(path = temp_dir))
  
  # Read the agent back with `x_read_disk()`
  agent_test_2 <- x_read_disk(filename = file.path(temp_dir, "agent_test_2"))
  
  # Expect a `ptblank_agent` object
  expect_s3_class(agent_test_2, "ptblank_agent")
  expect_s3_class(agent_test_2, "has_intel")
  
  # Expect the `tbl` data to be in the agent object
  expect_s3_class(agent_test_2$tbl, "tbl_df")
  
  # Expect the `extracts` list to be available in the agent object
  # TODO: expect it to be empty
  expect_type(agent_test_2$extracts, "list")
  
  # Create an agent with a single validation step, using
  # the `small_table` sqlite table  as the `tbl`; interrogate
  # the data
  agent <-
    create_agent(tbl = small_table_sqlite()) %>%
    rows_distinct() %>%
    interrogate()
  
  # Write the agent to disk, don't elect to keep the table
  agent %>% x_write_disk(filename = "agent_test_3", path = temp_dir)
  
  # Expect that the file was written to the temp directory
  expect_true("agent_test_3" %in% list.files(path = temp_dir))
  
  # Read the agent back with `x_read_disk()`
  agent_test_3 <- x_read_disk(filename = file.path(temp_dir, "agent_test_3"))
  
  # Expect a `ptblank_agent` object
  expect_s3_class(agent_test_3, "ptblank_agent")
  expect_s3_class(agent_test_3, "has_intel")
  
  # Don't expect the `tbl` data to be in the agent object
  expect_null(agent_test_3$tbl)
  
  # Set the table as the `small_table` sqlite table and then
  # expect the `tbl` data to be present again
  agent_test_3 <- agent_test_3 %>% set_tbl(small_table_sqlite())
  expect_s3_class(agent_test_3$tbl, "tbl_dbi")
  
  # Don't expect the `extracts` to be in the agent object
  expect_null(agent_test_3$extracts)
  
  # Write the agent to disk again, but choose to keep the table;
  # expect a warning since we can't directly keep `tbl_dbi` data
  # even if `keep_tbl = TRUE`
  expect_warning(
    agent %>% 
      x_write_disk(filename = "agent_test_4", path = temp_dir, keep_tbl = TRUE)
  )
  
  # Expect that the file was written to the temp directory
  expect_true("agent_test_4" %in% list.files(path = temp_dir))
  
  # Read the agent back with `x_read_disk()`
  agent_test_4 <- x_read_disk(filename = file.path(temp_dir, "agent_test_4"))
  
  # Expect a `ptblank_agent` object
  expect_s3_class(agent_test_4, "ptblank_agent")
  expect_s3_class(agent_test_4, "has_intel")
  
  # Don't expect the `tbl` data to be in the agent object
  expect_null(agent_test_4$tbl)
  
  # Expect the `extracts` list to be available in the agent object
  expect_type(agent_test_4$extracts, "list")
  
  # Add the `small_table` `tbl` with `set_tbl()`
  agent_test_4 <- agent_test_4 %>% set_tbl(tbl = small_table)
  
  # Expect the new `tbl` data to be in the agent object
  expect_s3_class(agent_test_4$tbl, "tbl_df")
  
  # Expect the `tbl_name` to remain as `small_table_sqlite()`
  expect_equal(agent_test_4$tbl_name, "small_table_sqlite()")
  
  # Expect the `db_tbl_name` to be NA
  expect_equal(agent_test_4$db_tbl_name, NA_character_)
  
  # Expect the `tbl_src` to be "tbl_df"
  expect_equal(agent_test_4$tbl_src, "tbl_df")
  
  # Expect the `tbl_src_details` to be NA
  expect_equal(agent_test_4$tbl_src_details, NA_character_)
  
  # Remove the data table from the agent with `remove_tbl()`
  agent_test_4 <- agent_test_4 %>% remove_tbl()
  
  # Don't expect the `tbl` data to be in the agent object
  expect_null(agent_test_4$tbl)
  
  # Set the table in the agent to once again be `small_table`
  # but in the table to `set_tbl()`
  agent_test_4 <- small_table %>% set_tbl(x = agent_test_4, tbl = .)
  
  # Expect the new `tbl` data to be in the agent object
  expect_s3_class(agent_test_4$tbl, "tbl_df")
  
  # Expect the `tbl_name` to still be `small_table_sqlite()`
  expect_equal(agent_test_4$tbl_name, "small_table_sqlite()")
  
  # Expect the `db_tbl_name` to be NA
  expect_equal(agent_test_4$db_tbl_name, NA_character_)
  
  # Expect the `tbl_src` to be "tbl_df"
  expect_equal(agent_test_4$tbl_src, "tbl_df")
  
  # Expect the `tbl_src_details` to be NA
  expect_equal(agent_test_4$tbl_src_details, NA_character_)
  
  # Set a table-prep formula and remove the associated table
  agent_test_4 <-
    agent_test_4 %>%
    remove_tbl() %>%
    set_read_fn(read_fn = ~ small_table)
  
  # Don't expect the `tbl` data to be in the agent object
  expect_null(agent_test_4$tbl)
  
  # But do expect the `read_fn` list element to be present as a formula
  expect_true(rlang::is_formula(agent_test_4$read_fn))
  expect_true(rlang::is_bare_formula(agent_test_4$read_fn))
  
  # Remove the table-prep formula from the agent with `remove_read_fn()`
  agent_test_4 <- agent_test_4 %>% remove_read_fn()
  
  # Don't expect the `read_fn` element to be in the agent object
  expect_null(agent_test_4$read_fn)
})

test_that("The `set_tbl()` function works as expected", {
  
  #
  # Tests with an agent
  #
  
  # Create an agent and supply it with the `specifications` table
  agent <- create_agent(tbl = specifications)
  
  expect_null(agent$read_fn)
  expect_equal(agent$tbl_name, "specifications")
  expect_match(agent$label, "\\[.*?\\]")
  expect_equal(agent$col_names, colnames(specifications))
  
  # Replace the table in the agent with `game_revenue`
  agent_replace_1 <- agent %>% set_tbl(tbl = game_revenue)
  
  expect_null(agent_replace_1$read_fn)
  expect_equal(agent_replace_1$tbl_name, "specifications")
  expect_equal(agent_replace_1$label, agent$label)
  expect_equal(agent_replace_1$col_names, colnames(game_revenue))
  
  # Replace the table in the agent with `game_revenue` and change the
  # table name and label
  agent_replace_2 <- 
    agent %>% 
    set_tbl(
      tbl = game_revenue,
      tbl_name = "game_revenue",
      label = "Checking the game revenue table."
    )
  
  expect_null(agent_replace_2$read_fn)
  expect_equal(agent_replace_2$tbl_name, "game_revenue")
  expect_match(agent_replace_2$label, "Checking the game revenue table.")
  expect_equal(agent_replace_2$col_names, colnames(game_revenue))
})
