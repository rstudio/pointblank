# Conditionally send email during interrogation

The `email_blast()` function is useful for sending an email message that
explains the result of a **pointblank** validation. It is powered by the
**blastula** and **glue** packages. This function should be invoked as
part of the `end_fns` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md).
It's also possible to invoke `email_blast()` as part of the `fns`
argument of the
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
function (i.e., to send multiple email messages at the granularity of
different validation steps exceeding failure thresholds).

To better get a handle on emailing with `email_blast()`, the analogous
[`email_create()`](https://rstudio.github.io/pointblank/reference/email_create.md)
function can be used with a **pointblank** agent object.

## Usage

``` r
email_blast(
  x,
  to,
  from,
  credentials = NULL,
  msg_subject = NULL,
  msg_header = NULL,
  msg_body = stock_msg_body(),
  msg_footer = stock_msg_footer(),
  send_condition = ~TRUE %in% x$notify
)
```

## Arguments

- x:

  A reference to the x-list object prepared internally by the agent.
  This version of the x-list is the same as that generated via
  `get_agent_x_list(<agent>)` except this version is internally
  generated and hence only available in an internal evaluation context.

- to, from:

  The email addresses for the recipients and of the sender.

- credentials:

  A credentials list object that is produced by either of the
  [`blastula::creds()`](https://rstudio.github.io/blastula/reference/credential_helpers.html),
  [`blastula::creds_anonymous()`](https://rstudio.github.io/blastula/reference/credential_helpers.html),
  [`blastula::creds_key()`](https://rstudio.github.io/blastula/reference/credential_helpers.html),
  or
  [`blastula::creds_file()`](https://rstudio.github.io/blastula/reference/credential_helpers.html)
  functions. Please refer to the **blastula** documentation for
  information on how to use these functions.

- msg_subject:

  The subject line of the email message.

- msg_header, msg_body, msg_footer:

  Content for the header, body, and footer components of the HTML email
  message.

- send_condition:

  An expression that should evaluate to a logical vector of length 1. If
  evaluated as `TRUE` then the email will be sent, if `FALSE` then that
  won't happen. The expression can use x-list variables (e.g.,
  `x$notify`, `x$type`, etc.) and all of those variables can be explored
  using the
  [`get_agent_x_list()`](https://rstudio.github.io/pointblank/reference/get_agent_x_list.md)
  function. The default expression is `~ TRUE %in% x$notify`, which
  results in `TRUE` if there are any `TRUE` values in the `x$notify`
  logical vector (i.e., any validation step that results in a 'notify'
  state).

## Value

Nothing is returned. The end result is the side-effect of email-sending
if certain conditions are met.

## YAML

A **pointblank** agent can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used to regenerate an agent (with
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md))
or interrogate the target table (via
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)).
Here is an example of how the use of `email_blast()` inside the
`end_fns` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
is expressed in R code and in the corresponding YAML representation.

R statement:

    create_agent(
      tbl = ~ small_table,
      tbl_name = "small_table",
      label = "An example.",
      actions = al,
      end_fns = list(
        ~ email_blast(
          x,
          to = "joe_public@example.com",
          from = "pb_notif@example.com",
          msg_subject = "Table Validation",
          credentials = blastula::creds_key(
            id = "smtp2go"
          )
        )
      )
    ) %>%
      col_vals_gt(a, 1) %>%
      col_vals_lt(a, 7)

YAML representation:

    type: agent
    tbl: ~small_table
    tbl_name: small_table
    label: An example.
    lang: en
    locale: en
    actions:
      warn_count: 1.0
      notify_count: 2.0
    end_fns: ~email_blast(x, to = "joe_public@example.com",
      from = "pb_notif@example.com", msg_subject = "Table Validation",
      credentials = blastula::creds_key(id = "smtp2go"),
      )
    embed_report: true
    steps:
    - col_vals_gt:
        columns: c(a)
        value: 1.0
    - col_vals_lt:
        columns: c(a)
        value: 7.0

## Examples

For the example provided here, we'll use the included `small_table`
dataset. We are also going to create an
[`action_levels()`](https://rstudio.github.io/pointblank/reference/action_levels.md)
list object since this is useful for demonstrating an emailing scenario.
It will have absolute values for the `warn` and `notify` states (with
thresholds of `1` and `2` 'fail' units, respectively, for the two
states).

    al <-
      action_levels(
        warn_at = 1,
        notify_at = 2
      )

Validate that values in column `a` from `small_tbl` are always greater
than `1` (with the
[`col_vals_gt()`](https://rstudio.github.io/pointblank/reference/col_vals_gt.md)
validation function), and, that values in `a` or are always less than
`7`.

The `email_blast()` function call is used in a list given to the
`end_fns` argument of
[`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md).
The `email_blast()` call itself has a `send_condition` argument that
determines whether or not an email will be sent. By default this is set
to `~ TRUE %in% x$notify`. Let's unpack this a bit. The variable `x` is
a list (we call it an x-list) and it will be populated with elements
pertaining to the agent. After interrogation, and only if action levels
were set for the `notify` state, `x$notify` will be present as a logical
vector where the length corresponds to the number of validation steps.
Thus, if any of those steps entered the `notify` state (here, it would
take two or more failing test units, per step, for that to happen), then
the statement as a whole is `TRUE` and the email of the interrogation
report will be sent. Here is the complete set of statements for the
creation of an *agent*, the addition of validation steps, and the
interrogation of data in `small_table`:

    agent <-
      create_agent(
        tbl = small_table,
        tbl_name = "small_table",
        label = "An example.",
        actions = al,
        end_fns = list(
          ~ email_blast(
            x,
            to =   "a_person@example.com",
            from = "pb_notif@example.com",
            msg_subject = "Table Validation",
            credentials = blastula::creds_key(id = "smtp2go"),
            send_condition = ~ TRUE %in% x$notify
          )
        )
      ) %>%
      col_vals_gt(a, value = 1) %>%
      col_vals_lt(a, value = 7) %>%
      interrogate()

The reason for the `~` present in the statements:

- `~ email_blast(...)` and

- `~ TRUE %in% x$notify`

is because this defers evocation of the emailing functionality (and also
defers evaluation of the `send_condition` value) until interrogation is
complete (with
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)).

## Function ID

4-1

## See also

Other Emailing:
[`email_create()`](https://rstudio.github.io/pointblank/reference/email_create.md),
[`stock_msg_body()`](https://rstudio.github.io/pointblank/reference/stock_msg_body.md),
[`stock_msg_footer()`](https://rstudio.github.io/pointblank/reference/stock_msg_footer.md)
