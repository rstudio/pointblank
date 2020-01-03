library(pointblank)

df <-
  data.frame(
    a = c(5, 7, 6, 5, 8, 7),
    b = c(3, 4, 6, 8, 9, 11),
    c = c(2, 6, 8, NA, 3, 8)
  )

# Validate that values in column
# `a` are always greater than 4
agent <-
  create_agent(tbl = df) %>%
  conjointly(
    ~ col_vals_gt(., columns = vars(a), value = 5),
    ~ col_vals_lt(., columns = vars(b), value = 10),
    ~ col_vals_not_null(., columns = vars(c))
  ) %>%
  col_vals_gt(columns = vars(a), value = 7) %>%
  col_vals_lt(., columns = vars(b), value = 12) %>%
  # conjointly(
  #   ~ col_vals_gt(., columns = vars(a), value = 5),
  #   ~ col_vals_lt(., columns = vars(b), value = 10),
  #   ~ col_vals_not_null(., columns = vars(c))
  # ) %>%
  interrogate()

get_agent_report(agent)

get_data_extracts(agent)

