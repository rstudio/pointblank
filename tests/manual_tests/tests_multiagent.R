library(tidyverse)
library(pointblank)
library(here)

here::i_am(path = "tests/manual_tests/tests_multiagent.R")
rel_path_outfiles <- here::here("tests/manual_tests/saved_agents")

# Create an initial table with poor data quality:
#  - lots of NA values
#  - values above 10
tbl <- tibble(a = c(NA, NA, 1, 5, 7, NA, 15, NA, 9, NA, 99, NA))

agent <- 
  create_agent(
    read_fn = ~tbl,
    tbl_name = "table_test",
    label = "DQ Check Over Time"
  ) %>%
  col_vals_not_null(vars(a)) %>%
  col_vals_lte(vars(a), value = 10, na_pass = TRUE)

yaml_write(
  agent = agent,
  filename = "agent-table_test.yaml",
  path = rel_path_outfiles
)

yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

tbl <- tibble(a = c(19, 3, 1, 5, 7, NA, 15, 3, 9, 24, 99, NA))

yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

tbl <- tibble(a = c(8, NA, 1, 5, 7, 3, 8, NA, 9, 4, 8, 5))


yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Read the saved agents into a series
multiagent <- 
  read_disk_multiagent(
    pattern = ".*rds",
    path = rel_path_outfiles
  )

multiagent

multiagent_tbl <- 
  get_multiagent_report(multiagent, display_table = FALSE)

multiagent_tbl


# Add a new step to the agent YAML

agent <- 
  create_agent(
    read_fn = ~tbl,
    tbl_name = "table_test",
    label = "DQ Check Over Time"
  ) %>%
  col_vals_not_null(vars(a)) %>%
  col_vals_lte(vars(a), value = 10, na_pass = TRUE) %>%
  col_vals_gt(vars(a), 0)

yaml_write(
  agent = agent,
  filename = "agent-table_test.yaml",
  path = rel_path_outfiles
)

yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Read the saved agents into a series
multiagent <- 
  read_disk_multiagent(
    pattern = ".*rds",
    path = rel_path_outfiles
  )

multiagent

#unpack(multiagent_tbl, cols = starts_with("i")) %>% select(i)