library(tidyverse)
library(pointblank)
library(here)

here::i_am(path = "tests/manual_tests/tests_multiagent.R")
rel_path_outfiles <- here::here("tests/manual_tests/saved_agents")

unlink(file.path(rel_path_outfiles, "*"))

# Create an initial table with poor data quality:
#  - lots of NA values
#  - values above 10

# TABLE 1
tbl <- tibble(a = c(NA, NA, 1, 5, 7, NA, 15, NA, 9, NA, 99, NA))

# Creation of agent and transformation to YAML file
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

# TABLE 2
tbl <- tibble(a = c(19, 3, 1, 5, 7, NA, 15, 3, 9, 24, 99, NA))

yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# TABLE 3
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

# Display results of three different interrogations
multiagent

# Add a new step to the agent YAML
agent <- 
  yaml_read_agent(
    file = "agent-table_test.yaml",
    path = rel_path_outfiles
  ) %>%
  col_vals_gt(vars(a), 0)

yaml_write(
  agent = agent,
  filename = "agent-table_test-2.yaml",
  path = rel_path_outfiles
)

yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
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

# Display results of four different interrogations
multiagent

# TABLE 4
tbl <- 
  tbl %>%
  dplyr::mutate(b = a + 15/4) %>%
  dplyr::bind_rows(
    dplyr::tibble(
      a = c(4, 2, 7, NA, 6, 1, 7, 1, 14, 13, NA, 10),
      b = c(11.5, 8.78, 9.23, 10.26, 1.25, 5.36, 7.97, 2.6, 7.37, 1.47, 11.4, NA)
    )
  )

yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
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

# Display results of four different interrogations
multiagent
