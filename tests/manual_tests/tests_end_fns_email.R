library(pointblank)

# Define action levels
al <- action_levels(warn_at = 0.1, stop_at = 0.2, notify_at = 0.3)

# Create an agent with a validation plan (no `interrogate()` statement
# here); the use of `end_fns` allows for a function to be executed at the
# conclusion of the interrogation, based on agent intel available in the
# x-list (the send condition uses `x$notify` the the predicate statement
# means that if any `notify` states are TRUE, then the one function--the
# `email_blast` one--will be executed)
agent <-
  create_agent(
    read_fn = ~small_table,
    actions = al,
    end_fns = list(
      ~ email_blast(
        x,
        to = "riannone@me.com",
        from = "rich@randr.rocks",
        msg_subject = "Validation of the table.",
        credentials = blastula::creds_envvar(
          user = "rstudio.com",
          pass_envvar = "SMTP2GOPASS",
          host = "mail.smtp2go.com",
          port = 465,
          use_ssl = TRUE
        ),
        send_condition = ~any(x$notify)
      ),
      ~ print(Sys.time())
    )
  ) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  col_vals_regex(vars(b), "[1-9]-[a-z]{3}-[0-9]{3}") %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE)

# Write the agent to a pointblank YAML file
agent_yaml_write(agent, filename = "test.yaml")

# Show the pointblank expressions derived from the YAML file
agent_yaml_show_exprs(path = "test.yaml")

# Interrogate the data (defined by the `read_fn` which is `small_table`)
# directly from the pointblank YAML file; this'll send me email
agent_yaml_interrogate(path = "test.yaml")
