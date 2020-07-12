library(pointblank)

al <- action_levels(warn_at = 0.1, stop_at = 0.2, notify_at = 0.3)

agent <-
  small_table %>%
  create_agent(
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
      )
    )
  ) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  col_vals_regex(vars(b), "[1-9]-[a-z]{3}-[0-9]{3}") %>%
  rows_distinct() %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

agent

