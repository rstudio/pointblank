context("Creation of `agent` objects")

test_that("Creating a valid `agent` object is possible", {
  
  # Create an agent object
  agent <- create_agent()
  
  # Expect that names in an agent object match a
  # prescribed set of names
  expect_true(
    all(
      names(agent) ==
        c("validation_name", "validation_time",
          "focal_tbl_name", "focal_file_name",
          "focal_db_type", "focal_col_names",
          "focal_col_types", "focal_db_cred_file_path",
          "focal_db_env_vars", "focal_init_sql", "email_creds_file_path",
          "email_notification_recipients", "email_notifications_active",
          "slack_webhook_url", "slack_channel", "slack_username", 
          "slack_author_name", "slack_title", "slack_report_url", 
          "slack_footer_thumb_url", "slack_footer_text", 
          "slack_notifications_active", "logical_plan",
          "validation_set", "sets", "preconditions")))
  
  # Expect an agent object of class `dgr_graph`
  expect_is(agent, "ptblank_agent")
  
  # Expect that the `logical_plan` component is
  # a `tbl_df`
  expect_is(agent$logical_plan, "tbl_df")
  
  # Expect that the `validation_set` component is
  # a `tbl_df`
  expect_is(agent$validation_set, "tbl_df")
  
  # Expect that the `sets` component is a list
  expect_is(agent$sets, "list")
  
  # Expect that the `preconditions` component is a list
  expect_is(agent$preconditions, "list")
  
  # Expect certain classes for the different
  # `agent` components
  expect_is(agent$validation_name, "character")
  expect_is(agent$validation_time, "POSIXct")
  expect_is(agent$focal_tbl_name, "character")
  expect_is(agent$focal_file_name, "character")
  expect_is(agent$focal_db_type, "character")
  expect_is(agent$focal_col_names, "character")
  expect_is(agent$focal_col_types, "character")
  expect_is(agent$focal_db_cred_file_path, "character")
  expect_is(agent$focal_init_sql, "character")
  expect_is(agent$email_creds_file_path, "character")
  expect_is(agent$email_notification_recipients, "character")
  expect_is(agent$email_notifications_active, "logical")
  expect_is(agent$slack_webhook_url, "character")
  expect_is(agent$slack_channel, "character")
  expect_is(agent$slack_username, "character")
  expect_is(agent$slack_author_name, "character")
  expect_is(agent$slack_title, "character")
  expect_is(agent$slack_report_url, "character")
  expect_is(agent$slack_footer_thumb_url, "character")
  expect_is(agent$slack_footer_text, "character")
  expect_is(agent$slack_notifications_active, "logical")
  expect_is(agent$validation_set$tbl_name, "character")
  expect_is(agent$validation_set$db_type, "character")
  expect_is(agent$validation_set$assertion_type, "character")
  expect_is(agent$validation_set$column, "character")
  expect_is(agent$validation_set$value, "numeric")
  expect_is(agent$validation_set$regex, "character")
  expect_is(agent$validation_set$n, "integer")
  expect_is(agent$validation_set$n_passed, "integer")
  expect_is(agent$validation_set$n_failed, "integer")
  expect_is(agent$validation_set$f_passed, "numeric")
  expect_is(agent$validation_set$f_failed, "numeric")
  expect_is(agent$validation_set$warn_count, "numeric")
  expect_is(agent$validation_set$notify_count, "numeric")
  expect_is(agent$validation_set$warn_fraction, "numeric")
  expect_is(agent$validation_set$notify_fraction, "numeric")
  expect_is(agent$validation_set$warn, "logical")
  expect_is(agent$validation_set$notify, "logical")
  expect_is(agent$validation_set$init_sql, "character")
  expect_is(agent$validation_set$db_cred_file_path, "character")
  expect_is(agent$validation_set$file_path, "character")
  expect_is(agent$validation_set$col_types, "character")
  expect_is(agent$validation_set$time_processed, "POSIXct")
  expect_is(agent$validation_set$proc_duration_s, "numeric")
  
  # Create an agent with a name
  agent_name <- create_agent(validation_name = "test")
  
  # Expect that the agent name has been set
  expect_equivalent(agent_name$validation_name, "test")
})
