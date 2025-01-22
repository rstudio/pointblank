#------------------------------------------------------------------------------#
#
#                 _         _    _      _                _
#                (_)       | |  | |    | |              | |
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   <
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |
#  |_|
#
#  This file is part of the 'rstudio/pointblank' project.
#
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


#' Conditionally send email during interrogation
#'
#' @description
#'
#' The `email_blast()` function is useful for sending an email message that
#' explains the result of a **pointblank** validation. It is powered by the
#' **blastula** and **glue** packages. This function should be invoked as part
#' of the `end_fns` argument of [create_agent()]. It's also possible to invoke
#' `email_blast()` as part of the `fns` argument of the [action_levels()]
#' function (i.e., to send multiple email messages at the granularity of
#' different validation steps exceeding failure thresholds).
#'
#' To better get a handle on emailing with `email_blast()`, the analogous
#' [email_create()] function can be used with a **pointblank** agent object.
#'
#' @param x A reference to the x-list object prepared internally by the agent.
#'   This version of the x-list is the same as that generated via
#'   `get_agent_x_list(<agent>)` except this version is internally generated and
#'   hence only available in an internal evaluation context.
#'
#' @param to,from The email addresses for the recipients and of the sender.
#'
#' @param credentials A credentials list object that is produced by either of
#'   the [blastula::creds()], [blastula::creds_anonymous()],
#'   [blastula::creds_key()], or [blastula::creds_file()] functions. Please
#'   refer to the **blastula** documentation for information on how to use these
#'   functions.
#'
#' @param msg_subject The subject line of the email message.
#'
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#'
#' @param send_condition An expression that should evaluate to a logical vector
#'   of length 1. If evaluated as `TRUE` then the email will be sent, if `FALSE`
#'   then that won't happen. The expression can use x-list variables (e.g.,
#'   `x$notify`, `x$type`, etc.) and all of those variables can be explored
#'   using the [get_agent_x_list()] function. The default expression is `~ TRUE
#'   %in% x$notify`, which results in `TRUE` if there are any `TRUE` values in
#'   the `x$notify` logical vector (i.e., any validation step that results in a
#'   'notify' state).
#'
#' @return Nothing is returned. The end result is the side-effect of
#'   email-sending if certain conditions are met.
#'
#' @section YAML:
#'
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). Here is an
#' example of how the use of `email_blast()` inside the `end_fns` argument of
#' [create_agent()] is expressed in R code and in the corresponding YAML
#' representation.
#'
#' R statement:
#'
#' ```r
#' create_agent(
#'   tbl = ~ small_table,
#'   tbl_name = "small_table",
#'   label = "An example.",
#'   actions = al,
#'   end_fns = list(
#'     ~ email_blast(
#'       x,
#'       to = "joe_public@example.com",
#'       from = "pb_notif@example.com",
#'       msg_subject = "Table Validation",
#'       credentials = blastula::creds_key(
#'         id = "smtp2go"
#'       )
#'     )
#'   )
#' ) %>%
#'   col_vals_gt(a, 1) %>%
#'   col_vals_lt(a, 7)
#' ```
#'
#' YAML representation:
#'
#' ```yaml
#' type: agent
#' tbl: ~small_table
#' tbl_name: small_table
#' label: An example.
#' lang: en
#' locale: en
#' actions:
#'   warn_count: 1.0
#'   notify_count: 2.0
#' end_fns: ~email_blast(x, to = "joe_public@example.com",
#'   from = "pb_notif@example.com", msg_subject = "Table Validation",
#'   credentials = blastula::creds_key(id = "smtp2go"),
#'   )
#' embed_report: true
#' steps:
#' - col_vals_gt:
#'     columns: c(a)
#'     value: 1.0
#' - col_vals_lt:
#'     columns: c(a)
#'     value: 7.0
#' ```
#'
#' @section Examples:
#'
#' For the example provided here, we'll use the included `small_table` dataset.
#' We are also going to create an `action_levels()` list object since this is
#' useful for demonstrating an emailing scenario. It will have absolute values
#' for the `warn` and `notify` states (with thresholds of `1` and `2` 'fail'
#' units, respectively, for the two states).
#'
#' ```r
#' al <-
#'   action_levels(
#'     warn_at = 1,
#'     notify_at = 2
#'   )
#' ```
#'
#' Validate that values in column `a` from `small_tbl` are always greater than
#' `1` (with the `col_vals_gt()` validation function), and, that values in `a`
#' or are always less than `7`.
#'
#' The `email_blast()` function call is used in a list given to the `end_fns`
#' argument of `create_agent()`. The `email_blast()` call itself has a
#' `send_condition` argument that determines whether or not an email will be
#' sent. By default this is set to `~ TRUE %in% x$notify`. Let's unpack this a
#' bit. The variable `x` is a list (we call it an x-list) and it will be
#' populated with elements pertaining to the agent. After interrogation, and
#' only if action levels were set for the `notify` state, `x$notify` will be
#' present as a logical vector where the length corresponds to the number of
#' validation steps. Thus, if any of those steps entered the `notify` state
#' (here, it would take two or more failing test units, per step, for that to
#' happen), then the statement as a whole is `TRUE` and the email of the
#' interrogation report will be sent. Here is the complete set of statements for
#' the creation of an *agent*, the addition of validation steps, and the
#' interrogation of data in `small_table`:
#'
#' ```r
#' agent <-
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al,
#'     end_fns = list(
#'       ~ email_blast(
#'         x,
#'         to =   "a_person@example.com",
#'         from = "pb_notif@example.com",
#'         msg_subject = "Table Validation",
#'         credentials = blastula::creds_key(id = "smtp2go"),
#'         send_condition = ~ TRUE %in% x$notify
#'       )
#'     )
#'   ) %>%
#'   col_vals_gt(a, value = 1) %>%
#'   col_vals_lt(a, value = 7) %>%
#'   interrogate()
#' ```
#'
#' The reason for the `~` present in the statements:
#'
#' - `~ email_blast(...)` and
#' - `~ TRUE %in% x$notify`
#'
#' is because this defers evocation of the emailing functionality (and also
#' defers evaluation of the `send_condition` value) until interrogation is
#' complete (with [interrogate()]).
#'
#' @family Emailing
#' @section Function ID:
#' 4-1
#'
#' @export
email_blast <- function(
    x,
    to,
    from,
    credentials = NULL,
    msg_subject = NULL,
    msg_header = NULL,
    msg_body = stock_msg_body(),
    msg_footer = stock_msg_footer(),
    send_condition = ~TRUE %in% x$notify
) {

  # nocov start

  # Evaluate condition for sending email
  condition_result <- rlang::f_rhs(send_condition) %>% rlang::eval_tidy()

  if (!is.logical(condition_result)) {
    warning("The `send_condition` expression must resolve to a logical value",
            call. = FALSE)
    return()
  }

  if (is.logical(condition_result) && condition_result) {

    check_msg_components_all_null(msg_header, msg_body, msg_footer)

    # Preparation of the message
    blastula_message <-
      blastula::compose_email(
        header = glue::glue(msg_header) %>% blastula::md(),
        body = glue::glue(msg_body) %>% blastula::md(),
        footer = glue::glue(msg_footer) %>% blastula::md(),
      )

    # Sending of the message
    blastula::smtp_send(
      email = blastula_message,
      to = to,
      from = from,
      subject = msg_subject,
      credentials = credentials
    )
  }
}

#' Create an email object from a **pointblank** *agent*
#'
#' @description
#'
#' The `email_create()` function produces an email message object that could be
#' sent using the **blastula** package. By supplying a **pointblank** agent, a
#' **blastula** `email_message` message object will be created and printing it
#' will make the HTML email message appear in the Viewer.
#'
#' @param x *The pointblank agent object*
#'
#'   `obj:<ptblank_agent>` // **required**
#'
#'   A **pointblank** *agent* object that is commonly created through the use of
#'   the [create_agent()] function.
#'
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#'
#' @return A **blastula** `email_message` object.
#'
#' @section Examples:
#'
#' For the example provided here, we'll use the included `small_table` dataset.
#' We are also going to create an `action_levels()` list object since this is
#' useful for demonstrating an emailing scenario. It will have absolute values
#' for the `warn` and `notify` states (with thresholds of `1` and `2` 'fail'
#' units, respectively, for the two states).
#'
#' ```r
#' al <-
#'   action_levels(
#'     warn_at = 1,
#'     notify_at = 2
#'   )
#' ```
#'
#' In a workflow that involves an `agent` object, we can make use of the
#' `end_fns` argument and programmatically email the report with the
#' [email_blast()] function. However, an alternate workflow that is demonstrated
#' here is to produce the email object directly. This provides the flexibility
#' to send the email outside of the **pointblank** API. The `email_create()`
#' function lets us do this with an `agent` object. We can then view the HTML
#' email just by printing `email_object`. It should appear in the Viewer.
#'
#' ```r
#' email_object <-
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(a, value = 1) %>%
#'   col_vals_lt(a, value = 7) %>%
#'   interrogate() %>%
#'   email_create()
#'
#' email_object
#' ```
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_email_create_1.png")`
#' }
#' }
#'
#'
#' @family Emailing
#' @section Function ID:
#' 4-2
#'
#' @export
email_create <- function(
    x,
    msg_header = NULL,
    msg_body = stock_msg_body(),
    msg_footer = stock_msg_footer()
) {

  if (!is_ptblank_agent(x)) {

    stop(
      "Email creation requires a pointblank agent.",
      call. = FALSE
    )
  }

  x <- get_agent_x_list(agent = x)

  return(
    blastula::compose_email(
      header = blastula::md(glue::glue(msg_header)),
      body = blastula::md(glue::glue(glue::glue(msg_body))),
      footer = blastula::md(glue::glue(glue::glue(msg_footer)))
    )
  )
}

check_msg_components_all_null <- function(msg_header, msg_body, msg_footer) {

  if (is.null(msg_header) && is.null(msg_body) && is.null(msg_footer)) {
    warning("There is no content provided for the email message")
  }
}

#' Provide simple email message body components: body
#'
#' The `stock_msg_body()` function simply provides some stock text for an email
#' message sent via [email_blast()] or obtained as a standalone object through
#' [email_create()].
#'
#' @return Text suitable for the `msg_body` argument of [email_blast()] and
#'   [email_create()].
#'
#' @family Emailing
#' @section Function ID:
#' 4-3
#'
#' @export
stock_msg_body <- function() {

  htmltools::tagList(
    htmltools::HTML("<!-- pointblank stock-msg-body -->"),
    htmltools::HTML(
      blastula::add_image(
        system.file("img", "pointblank_logo.png", package = "pointblank"),
        width = 150
      )
    ),
    htmltools::tags$br(),
    htmltools::tags$div(
      style = htmltools::css(
        `text-align` = "center",
        `font-size` = "larger"
      ),
      htmltools::HTML("{get_lsv('email/agent_body')[[x$lang]]}")
    ),
    htmltools::tags$br(),
    htmltools::tags$br(),
    htmltools::HTML("{x$report_html_small}"),
    htmltools::tags$br(),
    htmltools::tags$div(
      style = htmltools::css(
        `text-align` = "center",
        `font-size` = "larger"
      ),
      htmltools::HTML("&#9678;")
    )
  ) %>%
    as.character()
}

#' Provide simple email message body components: footer
#'
#' The `stock_msg_footer()` function simply provides some stock text for an
#' email message sent via [email_blast()] or obtained as a standalone object
#' through [email_create()].
#'
#' @return Text suitable for the `msg_footer` argument of [email_blast()] and
#'   [email_create()].
#'
#' @family Emailing
#' @section Function ID:
#' 4-4
#'
#' @export
stock_msg_footer <- function() {

  htmltools::tagList(
    htmltools::HTML("<!-- pointblank stock-msg-footer -->"),
    htmltools::tags$br(),
    htmltools::HTML("{get_lsv('email/footer_1')[[x$lang]]}"),
    htmltools::tags$br(),
    htmltools::tags$br(),
    htmltools::tags$br(),
    htmltools::tags$div(
      htmltools::tags$a(
        style = htmltools::css(
          `background-color` = "#999999",
          color = "white",
          padding = "1em 1.5em",
          position = "relative",
          `text-decoration` = "none",
          `text-transform` = "uppercase",
          cursor = "pointer"
        ),
        href = "https://rstudio.github.io/pointblank/",
        htmltools::HTML("{get_lsv('email/footer_2')[[x$lang]]}")
      )
    )
  ) %>%
    as.character()
}

# nocov end
