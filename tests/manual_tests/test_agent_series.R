library(tidyverse)
library(pointblank)
library(here)

here::i_am(path = "tests/manual_tests/test_agent_series.R")
rel_path_outfiles <- here::here("tests/manual_tests/agent_series")

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
  filename = "agent-table_test.yaml", # TODO: add function to append current date/datetime
  path = rel_path_outfiles
)

yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = "agent-table_test-1.rds",
    path = rel_path_outfiles
  )

tbl <- tibble(a = c(19, 3, 1, 5, 7, NA, 15, 3, 9, 24, 99, NA))

yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = "agent-table_test-2.rds",
    path = rel_path_outfiles
  )

tbl <- tibble(a = c(8, NA, 1, 5, 7, 3, 8, NA, 9, 4, 8, 5))


yaml_agent_interrogate(
  file = "agent-table_test.yaml",
  path = rel_path_outfiles
) %>%
  x_write_disk(
    filename = "agent-table_test-3.rds",
    path = rel_path_outfiles
  )

x_read_disk(
  filename = "agent-table_test-1.rds",
  path = rel_path_outfiles     
)
x_read_disk(
  filename = "agent-table_test-2.rds",
  path = rel_path_outfiles     
)
x_read_disk(
  filename = "agent-table_test-3.rds",
  path = rel_path_outfiles     
)


