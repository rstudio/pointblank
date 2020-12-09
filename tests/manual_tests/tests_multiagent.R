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
tbl <- tibble(a = c(NA, NA, 11, NA, 17, NA, 15, NA, 8, NA, NA, NA))

# Creation of agent and transformation to YAML file
agent <- 
  create_agent(
    read_fn = ~tbl,
    tbl_name = "table_test",
    label = "DQ Check Over Time",
    actions = action_levels(warn_at = 0.1, stop_at = 0.2)
  ) %>%
  col_vals_not_null(vars(a)) %>%
  col_vals_lte(vars(a), value = 10, na_pass = FALSE)

yaml_write(
  agent = agent,
  filename = "agent-table_test.yaml",
  path = rel_path_outfiles
)

# 1st
yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# TABLE 2
tbl <- tibble(a = c(19, 3, NA, 15, 27, NA, 15, 13, 9, 24, 99, NA))

# 2nd
yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# TABLE 3
tbl <- tibble(a = c(8, NA, 1, 15, 7, 13, 8, NA, 9, 14, 8, 5))

# 3rd
yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)

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

# 4th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)

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

# 5th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)


# TABLE 5
tbl <- 
  tbl %>%
  dplyr::bind_rows(
    dplyr::tibble(
      a = c(NA, 3, NA, NA, NA, 3, NA, 5, NA, 23, NA, 6),
      b = c(6.2, 2.5, 2.98, 2.46, 2.15, 0.35, 3.24, 3.1, 3.62, 3.90, NA, 3.23)
    )
  )

# 6th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# TABLE 6
tbl <- 
  tbl %>%
  dplyr::bind_rows(
    dplyr::tibble(
      a = c(3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
      b = c(8.6, 4.5, 3.98, 8.43, 3.54, 4.15, 3.64, 6.64, 2.96, 1.50, 4.23, 7.56)
    )
  )

# 7th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)

# TABLE 7
tbl <- 
  tbl %>%
  dplyr::bind_rows(
    dplyr::tibble(
      a = c(7, 3, 8, 1, 8, 3, 4, 2),
      b = c(2.6, 7.5, 7.2, 2.0, 8.3, 2.5, 2.64, 2.74)
    )
  )

# 8th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)

# TABLE 8
tbl <- 
  tbl %>%
  dplyr::bind_rows(
    dplyr::tibble(
      a = c(NA, 7, 2),
      b = c(6.7, 6.2, NA)
    )
  )

# 9th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)

# TABLE 9
tbl <- 
  tbl %>%
  dplyr::bind_rows(
    dplyr::tibble(
      a = c(8, 2, 7, 5, 8),
      b = c(1.3, 6.7, 3.6, 1.4, 8.3)
    )
  )

# 10th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 11th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 12th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 13th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 14th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 15th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 16th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)

# 17th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# 18th
yaml_agent_interrogate(
  file = "agent-table_test-2.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = affix_datetime("agent-table_test.rds"),
    path = rel_path_outfiles
  )

# Display results of the different interrogations
read_disk_multiagent(
  pattern = ".*rds",
  path = rel_path_outfiles
)
