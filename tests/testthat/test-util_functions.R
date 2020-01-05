context("The utility functions work as expected")

test_that("Utility functions won't fail us", {
  
  small_table <-
    readr::read_csv(
      system.file("extdata", "small_table.csv", package = "pointblank"),
      col_types = "TDicddlc")
  
  # Use two validation step functions to create
  # an agent with two validation steps 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000)
  
  is_ptblank_agent(x = agent) %>% expect_true()
  is_ptblank_agent(x = small_table) %>% expect_false()
  
  agent %>% get_tbl_object() %>% expect_is("tbl_df")
  agent %>% get_tbl_object() %>% expect_equivalent(small_table)
  
  agent %>% has_agent_intel() %>% expect_false()
  agent %>% interrogate() %>% has_agent_intel() %>% expect_true()
  
  agent %>% is_agent_empty() %>% expect_false()
  small_table %>% is_agent_empty() %>% expect_false()
  create_agent(tbl = small_table) %>% is_agent_empty() %>% expect_true()
  
  agent %>% interrogate() %>% interrogation_time() %>% expect_is("POSIXct")
  agent %>% interrogate() %>% interrogation_time() %>% length() %>% expect_equal(1)
  agent %>% interrogation_time() %>% expect_equal(NA)
  
  agent %>% number_of_validation_steps() %>% expect_equal(2)
  small_table %>% number_of_validation_steps() %>% expect_equal(NA)

  agent %>% get_assertion_type_at_idx(idx = 1) %>% expect_equal("col_vals_gt")
  agent %>% get_assertion_type_at_idx(idx = 2) %>% expect_equal("col_vals_lt")

  agent %>% get_column_as_sym_at_idx(idx = 1) %>% expect_is("name")
  agent %>% get_column_as_sym_at_idx(idx = 1) %>% as.character() %>% expect_equal("c")
  agent %>% get_column_as_sym_at_idx(idx = 2) %>% expect_is("name")
  agent %>% get_column_as_sym_at_idx(idx = 2) %>% as.character() %>% expect_equal("d")
  
  agent %>% get_column_value_at_idx(idx = 1) %>% expect_is("numeric")
  agent %>% get_column_value_at_idx(idx = 1) %>% expect_equal(5)
  agent %>% get_column_value_at_idx(idx = 2) %>% expect_is("numeric")
  agent %>% get_column_value_at_idx(idx = 2) %>% expect_equal(1000)
  
  agent %>% get_all_cols() %>% expect_is("character")
  agent %>% get_all_cols() %>%
    expect_equal(c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  
  # Use set-based validation step functions to create
  # an agent with two validation steps 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = vars(e), set = c(TRUE, FALSE)) %>%
    col_vals_not_in_set(columns = vars(f), set = c("medium", "floor"))
  
  agent %>% get_column_set_values_at_idx(idx = 1) %>% expect_is("logical")
  agent %>% get_column_set_values_at_idx(idx = 1) %>% expect_equal(c(TRUE, FALSE))
  agent %>% get_column_set_values_at_idx(idx = 2) %>% expect_is("character")
  agent %>% get_column_set_values_at_idx(idx = 2) %>% expect_equal(c("medium", "floor"))
  
  # Use the `col_vals_regex()` validation step
  # function to create an agent with one validation step 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(columns = vars(b), regex = "[0-9]-[a-z]*?-[0-9]*?")
  
  agent %>% get_column_regex_at_idx(idx = 1) %>% expect_is("character")
  agent %>% get_column_regex_at_idx(idx = 1) %>% expect_equal("[0-9]-[a-z]*?-[0-9]*?")
  
  # Use range-based validation step functions to create
  # an agent with two validation steps 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_between(columns = vars(c), left = 2, right = 5, na_pass = TRUE) %>%
    col_vals_not_between(columns = vars(c), left = 2, right = 5, na_pass = FALSE)
  
  agent %>% get_column_na_pass_at_idx(idx = 1) %>% expect_is("logical")
  agent %>% get_column_na_pass_at_idx(idx = 1) %>% expect_equal(TRUE)
  agent %>% get_column_na_pass_at_idx(idx = 2) %>% expect_is("logical")
  agent %>% get_column_na_pass_at_idx(idx = 2) %>% expect_equal(FALSE)
})
