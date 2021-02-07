work_path <- "./generated_testthat_files"

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

fs::dir_create(path = work_path)

test_that("creating a multiagent object from files on disk works", {
  
  # TABLE 1
  tbl <<- dplyr::tibble(a = c(NA, NA, 11, NA, 17, NA, 15, NA, 8, NA, NA, NA))
  
  # Creation of agent and transformation to YAML file
  agent <- 
    create_agent(
      read_fn = ~tbl,
      tbl_name = "table_test",
      label = "DQ Check Over Time",
      actions = action_levels(warn_at = 0.1, stop_at = 0.2)
    ) %>%
    col_vals_not_null(vars(a)) %>%
    col_vals_lte(vars(a), value = 10, na_pass = FALSE) %>%
    col_vals_gt(vars(a), 0)
  
  yaml_write(
    agent = agent,
    filename = "agent-table_test.yaml",
    path = work_path
  )
  
  # 1st
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-01.rds",
      path = work_path
    )
  
  # TABLE 2
  tbl <<- dplyr::tibble(a = c(19, 3, NA, 15, 27, NA, 15, 13, 9, 24, 99, NA))
  
  # 2nd
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-02.rds",
      path = work_path
    )
  
  # TABLE 3
  tbl <<- dplyr::tibble(a = c(8, NA, 1, 15, 7, 13, 8, NA, 9, 14, 8, 5))
  
  # 3rd
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-03.rds",
      path = work_path
    )
  
  # 4th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-04.rds",
      path = work_path
    )
  
  # TABLE 4
  tbl <<- 
    tbl %>%
    dplyr::mutate(b = a + 15/4) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        a = c(4, 2, 7, NA, 6, 1, 7, 1, 14, 13, NA, 10),
        b = c(11.5, 8.78, 9.23, 10.26, 1.25, 5.36, 7.97, 2.6, 7.37, 1.47, 11.4, NA)
      )
    )
  
  # 5th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-05.rds",
      path = work_path
    )
  
  # TABLE 5
  tbl <<- 
    tbl %>%
    dplyr::bind_rows(
      dplyr::tibble(
        a = c(NA, 3, NA, NA, NA, 3, NA, 5, NA, 23, NA, 6),
        b = c(6.2, 2.5, 2.98, 2.46, 2.15, 0.35, 3.24, 3.1, 3.62, 3.90, NA, 3.23)
      )
    )
  
  # 6th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-06.rds",
      path = work_path
    )
  
  # TABLE 6
  tbl <<- 
    tbl %>%
    dplyr::bind_rows(
      dplyr::tibble(
        a = c(3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        b = c(8.6, 4.5, 3.98, 8.43, 3.54, 4.15, 3.64, 6.64, 2.96, 1.50, 4.23, 7.56)
      )
    )
  
  # 7th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-07.rds",
      path = work_path
    )
  
  # TABLE 7
  tbl <<- 
    tbl %>%
    dplyr::bind_rows(
      dplyr::tibble(
        a = c(7, 3, 8, 1, 8, 3, 4, 2),
        b = c(2.6, 7.5, 7.2, 2.0, 8.3, 2.5, 2.64, 2.74)
      )
    )
  
  # 8th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-08.rds",
      path = work_path
    )
  
  # TABLE 8
  tbl <<- 
    tbl %>%
    dplyr::bind_rows(
      dplyr::tibble(
        a = c(NA, 7, 2),
        b = c(6.7, 6.2, NA)
      )
    )
  
  # 9th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-09.rds",
      path = work_path
    )
  
  # TABLE 9
  tbl <<- 
    tbl %>%
    dplyr::bind_rows(
      dplyr::tibble(
        a = c(8, 2, 7, 5, 8),
        b = c(1.3, 6.7, 3.6, 1.4, 8.3)
      )
    )
  
  # 10th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-10.rds",
      path = work_path
    )
  
  # 11th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-11.rds",
      path = work_path
    )
  
  # 12th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-12.rds",
      path = work_path
    )
  
  # 13th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-13.rds",
      path = work_path
    )
  
  # 14th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-14.rds",
      path = work_path
    )
  
  # 15th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-15.rds",
      path = work_path
    )
  
  # 16th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-16.rds",
      path = work_path
    )
  
  # 17th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-17.rds",
      path = work_path
    )
  
  # 18th
  yaml_agent_interrogate(
    file = "agent-table_test.yaml",
    path = work_path
  ) %>%
    x_write_disk(
      filename = "agent-table_test-18.rds",
      path = work_path
    )
  
  # Display results of the different interrogations
  multiagent <-
    read_disk_multiagent(
      pattern = ".*rds",
      path = work_path
    )
  
  # Expect a multiagent object of class `ptblank_multiagent`
  expect_is(multiagent, "ptblank_multiagent")
  
  # Expect that names in a multiagent object match a
  # prescribed set of names
  expect_true(all(names(multiagent) == c("overview_tbl", "agents")))
  
  # Expect that `multiagent$agents` has a length of 18 (one for each agent)
  expect_equal(length(multiagent$agents), 18)
  
  for (i in seq_len(length(multiagent$agents))) {
    
    agent_i <- multiagent$agents[[i]]
    
    # Expect that the `validation_set` component is a `tbl_df`
    expect_is(agent_i$validation_set, "tbl_df")
    
    # Expect certain classes for the different `ptblank_agent` components
    expect_null(agent_i$tbl)
    expect_is(agent_i$read_fn, "formula")
    expect_is(agent_i$tbl_name, "character")
    expect_is(agent_i$label, "character")
    expect_is(agent_i$tbl_src, "character")
    expect_is(agent_i$tbl_src_details, "character")
    expect_is(agent_i$col_names, "character")
    expect_is(agent_i$col_types, "character")
    expect_is(agent_i$db_col_types, "character")
    expect_is(agent_i$actions, "action_levels")
    expect_is(agent_i$end_fns, "list")
    expect_is(agent_i$embed_report, "logical")
    expect_is(agent_i$lang, "character")
    expect_is(agent_i$time_start, "POSIXct")
    expect_is(agent_i$time_end, "POSIXct")
    expect_is(agent_i$validation_set$i, "integer")
    expect_is(agent_i$validation_set$assertion_type, "character")
    expect_is(agent_i$validation_set$column, "list")
    expect_is(agent_i$validation_set$values, "list")
    expect_is(agent_i$validation_set$na_pass, "logical")
    expect_is(agent_i$validation_set$preconditions, "list")
    expect_is(agent_i$validation_set$actions, "list")
    expect_is(agent_i$validation_set$brief, "character")
    expect_is(agent_i$validation_set$active, "list")
    expect_is(agent_i$validation_set$eval_active, "logical")
    expect_is(agent_i$validation_set$eval_error, "logical")
    expect_is(agent_i$validation_set$eval_warning, "logical")
    expect_is(agent_i$validation_set$capture_stack, "list")
    expect_is(agent_i$validation_set$all_passed, "logical")
    expect_is(agent_i$validation_set$n, "numeric")
    expect_is(agent_i$validation_set$n_passed, "numeric")
    expect_is(agent_i$validation_set$n_failed, "numeric")
    expect_is(agent_i$validation_set$f_passed, "numeric")
    expect_is(agent_i$validation_set$f_failed, "numeric")
    expect_is(agent_i$validation_set$warn, "logical")
    expect_is(agent_i$validation_set$notify, "logical")
    expect_is(agent_i$validation_set$stop, "logical")
    expect_is(agent_i$validation_set$row_sample, "numeric")
    expect_is(agent_i$validation_set$tbl_checked, "list")
    expect_is(agent_i$validation_set$time_processed, "POSIXct")
    expect_is(agent_i$validation_set$proc_duration_s, "numeric")
    expect_is(agent_i$extracts, "list")
  }
  
  # Expect that the `read_disk_multiagent()` function will
  # stop if the file list is empty
  expect_error(
    read_disk_multiagent(
      pattern = ".*rda",
      path = work_path
    )
  )
  
  # Expect that not supplying a path will mean the path is
  # the current working directory (which is temporarily
  # changed for this test)
  current_path <- getwd()
  setwd(work_path)
  expect_is(
    read_disk_multiagent(
      pattern = ".*rds"
    ),
    "ptblank_multiagent"
  )
  setwd(current_path)
})

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}
