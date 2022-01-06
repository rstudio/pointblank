#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Send email at a validation step or at the end of an interrogation
#' 
#' @description
#' The `email_blast()` function is useful for sending an email message that
#' explains the result of a **pointblank** validation. It is powered by the
#' **blastula** and **glue** packages. This function should be invoked as part
#' of the `end_fns` argument of [create_agent()]. It's also possible to invoke
#' `email_blast()` as part of the `fns` argument of the [action_levels()]
#' function (i.e., to send multiple email messages at the granularity of
#' different validation steps exceeding failure thresholds).
#'
#' To better get a handle on emailing with `email_blast()`, the analogous
#' [email_create()] function can be used with a **pointblank** agent object or
#' an x-list obtained from using the [get_agent_x_list()] function.
#' 
#' @section YAML: 
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). Here is an
#' example of how the use of `email_blast()` inside the `end_fns` argument of
#' [create_agent()] is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' ```
#' # R statement
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
#'       ),
#'     )
#'   )
#' ) %>%
#'   col_vals_gt(vars(a), 1) %>%
#'   col_vals_lt(vars(a), 7) 
#' 
#' # YAML representation
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
#'   - col_vals_gt:
#'     columns: vars(a)
#'     value: 1.0
#'   - col_vals_lt:
#'     columns: vars(a)
#'     value: 7.0
#' ```
#' 
#' @param x A reference to the x-list object prepared internally by the agent.
#'   This version of the x-list is the same as that generated via
#'   `get_agent_x_list(<agent>)` except this version is internally generated and
#'   hence only available in an internal evaluation context.
#' @param to,from The email addresses for the recipients and of the sender.
#' @param credentials A credentials list object that is produced by either of
#'   the [blastula::creds()], [blastula::creds_anonymous()],
#'   [blastula::creds_key()], or [blastula::creds_file()] functions. Please
#'   refer to the **blastula** documentation for information on how to use these
#'   functions.
#' @param msg_subject The subject line of the email message.
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#' @param send_condition An expression that should evaluate to a logical vector
#'   of length 1. If evaluated as `TRUE` then the email will be sent, if `FALSE`
#'   then that won't happen. The expression can use x-list variables (e.g.,
#'   `x$notify`, `x$type`, etc.) and all of those variables can be explored
#'   using the [get_agent_x_list()] function. The default expression is `~TRUE
#'   %in% x$notify`, which results in `TRUE` if there are any `TRUE` values in
#'   the `x$notify` logical vector (i.e., any validation step results in a
#'   'notify' condition).
#'   
#' @examples
#' # Create an `action_levels()` list
#' # with absolute values for the
#' # `warn`, and `notify` states (with
#' # thresholds of 1 and 2 'fail' units)
#' al <- 
#'   action_levels(
#'     warn_at = 1,
#'     notify_at = 2
#'   )
#'   
#' if (interactive()) {
#' 
#' # Validate that values in column
#' # `a` from `small_tbl` are always > 1
#' # and that they are always < 7; first,
#' # apply the `actions_levels()`
#' # directive to `actions` and set up
#' # an `email_blast()` as one of the
#' # `end_fns` (by default, the email
#' # will be sent if there is a single
#' # 'notify' state across all
#' # validation steps)
#' agent <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al,
#'     end_fns = list(
#'       ~ email_blast(
#'         x,
#'         to = "joe_public@example.com",
#'         from = "pb_notif@example.com",
#'         msg_subject = "Table Validation",
#'         credentials = blastula::creds_key(
#'           id = "smtp2go"
#'         ),
#'       )
#'     )
#'   ) %>%
#'   col_vals_gt(vars(a), value = 1) %>%
#'   col_vals_lt(vars(a), value = 7) %>%
#'   interrogate()
#' 
#' }
#' 
#' # The above example was intentionally
#' # not run because email credentials
#' # aren't available and the `to`
#' # and `from` email addresses are
#' # nonexistent
#' 
#' # To get a blastula email object
#' # instead of eagerly sending the
#' # message, we can use the 
#' # `email_create()` function
#' email_object <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 5) %>%
#'   col_vals_lt(vars(a), value = 7) %>%
#'   interrogate() %>%
#'   email_create()
#'   
#' @family Emailing
#' @section Function ID:
#' 4-1
#' 
#' @export 
email_blast <- function(x,
                        to,
                        from,
                        credentials = NULL,
                        msg_subject = NULL,
                        msg_header = NULL,
                        msg_body = stock_msg_body(),
                        msg_footer = stock_msg_footer(),
                        send_condition = ~TRUE %in% x$notify) {

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

#' Create an email object from a **pointblank** *agent* or *informant*
#' 
#' @description
#' The `email_create()` function produces an email message object that could be
#' sent using the **blastula** package. The `x` that we need for this could
#' either be a **pointblank** agent, the *agent* x-list (produced from the
#' *agent* with the [get_agent_x_list()] function), or a **pointblank**
#' *informant*. In all cases, the email message will appear in the Viewer and a
#' **blastula** `email_message` object will be returned.
#'
#' @param x A **pointblank** *agent*, an *agent* x-list, or a **pointblank**
#'   *informant*. The x-list object can be created with the [get_agent_x_list()]
#'   function. It is recommended that the option `i = NULL` be used with
#'   [get_agent_x_list()] if supplying an x-list as `x`. Furthermore, The option
#'   `generate_report = TRUE` could be used with [create_agent()] so that the
#'   agent report is available within the email.
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#'   
#' @return A **blastula** `email_message` object.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # Create an `action_levels()` list
#' # with absolute values for the
#' # `warn`, and `notify` states (with
#' # thresholds of 1 and 2 'fail' units)
#' al <- 
#'   action_levels(
#'     warn_at = 1,
#'     notify_at = 2
#'   )
#' 
#' # In a workflow that involves an
#' # `agent` object, we can make use of
#' # the `end_fns` argument and
#' # programmatically email the report
#' # with the `email_blast()` function,
#' # however, an alternate workflow is to
#' # produce the email object and choose
#' # to send outside of the pointblank API;
#' # the `email_create()` function lets
#' # us do this with an `agent` object
#' email_object_1 <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 1) %>%
#'   col_vals_lt(vars(a), value = 7) %>%
#'   interrogate() %>%
#'   email_create()
#' 
#' # We can view the HTML email just
#' # by printing `email_object`; it
#' # should appear in the Viewer
#' 
#' # The `email_create()` function can
#' # also be used on an agent x-list to
#' # get the same email message object
#' email_object_2 <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(vars(a), value = 5) %>%
#'   col_vals_lt(vars(b), value = 5) %>%
#'   interrogate() %>%
#'   get_agent_x_list() %>%
#'   email_create()
#' 
#' # An information report that's
#' # produced by the informant can
#' # made into an email message object;
#' # let's create an informant and use
#' # `email_create()`
#' email_object_3 <-
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>%
#'   info_tabular(
#'     info = "A simple table in the
#'     *Examples* section of the function
#'     called `email_create()`."
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "Numbers. On the high side."
#'   ) %>%
#'   info_columns(
#'     columns = vars(b),
#'     info = "Lower numbers. Zeroes, even."
#'   ) %>%
#'   incorporate() %>%
#'   email_create()
#' 
#' }
#' 
#' @family Emailing
#' @section Function ID:
#' 4-2
#' 
#' @export 
email_create <- function(x,
                         msg_header = NULL,
                         msg_body = stock_msg_body(),
                         msg_footer = stock_msg_footer()) {
  
  if (!is_ptblank_agent(x) &&
      !is_ptblank_x_list(x) &&
      !is_ptblank_informant(x)) {
    
    stop(
      "Email creation requires either:\n",
      "* a pointblank agent\n",
      "* an agent x-list, or\n",
      "* a pointblank informant",
      call. = FALSE
    )
  }
  
  if (is_ptblank_informant(x)) {
    
    if (grepl("<!-- pointblank stock-msg-body", msg_body, fixed = TRUE)) {
      msg_body <- gsub("email/agent_body", "email/informant_body", msg_body)
    }
    
    if (grepl("<!-- pointblank stock-msg-footer", msg_footer, fixed = TRUE)) {
      msg_footer <- gsub("email/footer_1", "email/footer_i", msg_footer)
    }
    
    x$report_html_small <- get_informant_report(x, size = "small")

    x$report_html_small[["_source_notes"]] <- list()

    x$report_html_small <-
      x$report_html_small %>%
      gt::as_raw_html()
    
    x$time_start <- Sys.time()

    return(
      blastula::compose_email(
        header = blastula::md(glue::glue(msg_header)),
        body = blastula::md(glue::glue(glue::glue(msg_body))),
        footer = blastula::md(glue::glue(glue::glue(msg_footer)))
      )
    )
  }
  
  if (is_ptblank_agent(x)) {
    x <- get_agent_x_list(agent = x)
    
    return(
      blastula::compose_email(
        header = blastula::md(glue::glue(msg_header)),
        body = blastula::md(glue::glue(glue::glue(msg_body))),
        footer = blastula::md(glue::glue(glue::glue(msg_footer)))
      )
    )
  }
}

check_msg_components_all_null <- function(msg_header, msg_body, msg_footer) {
  
  if (is.null(msg_header) & is.null(msg_body) & is.null(msg_footer)) {
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
        href = "https://rich-iannone.github.io/pointblank/",
        htmltools::HTML("{get_lsv('email/footer_2')[[x$lang]]}")
      )
    )
  ) %>%
    as.character()
}

# nocov end
