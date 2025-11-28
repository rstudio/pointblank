# Create an email object from a **pointblank** *agent*

The `email_create()` function produces an email message object that
could be sent using the **blastula** package. By supplying a
**pointblank** agent, a **blastula** `email_message` message object will
be created and printing it will make the HTML email message appear in
the Viewer.

## Usage

``` r
email_create(
  x,
  msg_header = NULL,
  msg_body = stock_msg_body(),
  msg_footer = stock_msg_footer()
)
```

## Arguments

- x:

  *The pointblank agent object*

  `obj:<ptblank_agent>` // **required**

  A **pointblank** *agent* object that is commonly created through the
  use of the
  [`create_agent()`](https://rstudio.github.io/pointblank/reference/create_agent.md)
  function.

- msg_header, msg_body, msg_footer:

  Content for the header, body, and footer components of the HTML email
  message.

## Value

A **blastula** `email_message` object.

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

In a workflow that involves an `agent` object, we can make use of the
`end_fns` argument and programmatically email the report with the
[`email_blast()`](https://rstudio.github.io/pointblank/reference/email_blast.md)
function. However, an alternate workflow that is demonstrated here is to
produce the email object directly. This provides the flexibility to send
the email outside of the **pointblank** API. The `email_create()`
function lets us do this with an `agent` object. We can then view the
HTML email just by printing `email_object`. It should appear in the
Viewer.

    email_object <-
      create_agent(
        tbl = small_table,
        tbl_name = "small_table",
        label = "An example.",
        actions = al
      ) %>%
      col_vals_gt(a, value = 1) %>%
      col_vals_lt(a, value = 7) %>%
      interrogate() %>%
      email_create()

    email_object

![This image was generated from the first code example in the
\`email_create()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_email_create_1.png)

## Function ID

4-2

## See also

Other Emailing:
[`email_blast()`](https://rstudio.github.io/pointblank/reference/email_blast.md),
[`stock_msg_body()`](https://rstudio.github.io/pointblank/reference/stock_msg_body.md),
[`stock_msg_footer()`](https://rstudio.github.io/pointblank/reference/stock_msg_footer.md)
