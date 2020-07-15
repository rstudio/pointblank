library(pointblank)
library(tidyverse)

diff_table <- small_table %>% select(a, b, c, d, e)
al <- action_levels(warn_at = 0.1, stop_at = 0.2)

#
# FLOW 1: using a tbl, writing and reading agent, setting a new tbl
#

agent_1 <-
  create_agent(tbl = small_table, actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

agent_1

agent_1 %>% agent_write(filename = "agent_1.rds")

agent_1new <- agent_read("agent_1.rds")

# This should error: the data was removed before writing
agent_1new %>% interrogate()

# Set the agent `tbl` with `set_tbl()`
agent_1new <- agent_1new %>% set_tbl(diff_table)

# This shows the previous agent report
agent_1new

# Using `interrogate()` again to re-interrogate the data:
agent_1new <- agent_1new %>% interrogate()

# This is the revised agent report
agent_1new


#
# FLOW 2: using a tbl and a read_fn, writing and reading agent, letting the
#         read_fn reset the table
#

agent_2 <-
  create_agent(tbl = small_table, read_fn = ~ diff_table, actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

agent_2

agent_2 %>% agent_write(filename = "agent_2.rds")

agent_2new <- agent_read("agent_2.rds")

# Interrogating again should work! There is because there is `read_fn`
# defined, which isn't lost upon writing the agent to disk like
# the `tbl` is by default
agent_2new %>% interrogate()

agent_2new <- agent_2new %>% set_tbl(small_table)

# This shows the previous agent report
agent_2new

# Using `interrogate()` again to reinterrogate the data:
agent_2new <- agent_2new %>% interrogate()

# This is the revised agent report
agent_2new


#
# FLOW 3: using just a read_fn, writing and reading agent, letting
#         `read_fn` get the table at each interrogation
#

agent_3 <-
  create_agent(read_fn = ~ diff_table, actions = al) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

agent_3

agent_3 %>% agent_write(filename = "agent_3.rds")

agent_3new <- agent_read("agent_3.rds")

# Change the `diff_table` data
diff_table <- dplyr::bind_rows(diff_table, diff_table)

# Interrogating again should work! There is because there is `read_fn`
# defined, which isn't lost upon writing the agent to disk like
# the `tbl` is by default
agent_3new %>% interrogate()

agent_3new <- agent_3new %>% set_tbl(diff_table)

# This shows the previous agent report
agent_3new

# Using `interrogate()` again to re-interrogate the data:
agent_3new <- agent_3new %>% interrogate()

# This is the revised agent report
agent_3new

