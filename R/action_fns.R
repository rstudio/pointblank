# nocov start

#' Send email at a step or at the end of an interrogation
#' 
#' The `email_blast()` function is useful for sending an email message that
#' explains the result of a **pointblank** validation. It is powered by the
#' **blastula** and **glue** packages (please install those packages through
#' `install.packages()`). This function should be invoked as part of the
#' `end_fns` argument of [create_agent()]. It's also possible to invoke
#' `email_blast()` as part of the `fns` argument of the [action_levels()]
#' function (to possibly send an email message at one or more steps). To better
#' get a handle on emailing with `email_blast()`, the analogous
#' [email_blast_preview()] can be used with a **pointblank** agent object or the
#' output obtained from using the [get_agent_x_list()] function.
#' 
#' @param x A reference to list object prepared by the agent. It's only
#'   available in an internal evaluation context.
#' @param to,from The email addresses for the recipients and the sender.
#' @param subject The subject line of the email message.
#' @param credentials A credentials list object that is produced by either of
#'   the [blastula::creds()], [blastula::creds_anonymous()],
#'   [blastula::creds_key()], or [blastula::creds_file()] functions. Please
#'   refer to the **blastula** documentation for details on each of these helper
#'   functions.
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#' @param send_condition An expression that should evaluate to a logical vector
#'   of length 1. If `TRUE` then the email will be sent, if `FALSE` then that
#'   won't happen.
#' 
#' @export 
email_blast <- function(x,
                        to,
                        from,
                        subject = NULL,
                        credentials = NULL,
                        msg_header = NULL,
                        msg_body = stock_msg_body(),
                        msg_footer = stock_msg_footer(),
                        send_condition = ~TRUE %in% x$notify) {

  # Evaluate condition for sending email
  condition_result <- rlang::f_rhs(send_condition) %>% rlang::eval_tidy()
  
  if (!is.logical(condition_result)) {
    warning("The `send_condition` expression must resolve to a logical value",
            call. = FALSE)
    return()
  }
  
  if (is.logical(condition_result) && condition_result) {
    
    if (!requireNamespace("blastula", quietly = TRUE) &&
        !requireNamespace("glue", quietly = TRUE)) {
      
      warning("Sending an email message with `email_blast()` requires both the blastula and glue packages:\n",
              " * Install them with `install.packages(\"blastula\")` and `install.packages(\"glue\")`.",
              call. = FALSE)
      return()
    }
    
    check_msg_components_all_null(msg_header, msg_body, msg_footer)
    
    if (requireNamespace("blastula", quietly = TRUE) &&
        requireNamespace("glue", quietly = TRUE)) {
      
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
        subject = subject,
        credentials = credentials
      )
    }
  }
}

#' Get a preview of an email before actually sending that email
#' 
#' The `email_blast_preview()` function provides a preview of an email that
#' would normally be produced and sent through the [email_blast()] function. The
#' `x` that we need for this is the agent x-list that is produced by the
#' [get_agent_x_list()] function. Or, we can supply an agent object. In both
#' cases, the email message with appear in the Viewer and a **blastula**
#' `email_message` object will be returned.
#'
#' @param x A pointblank agent or an agent x-list. The x-list object can be
#'   created with the [get_agent_x_list()] function. It is recommended that the
#'   `i = NULL` and `generate_report = TRUE` so that the agent report is
#'   available within the email preview.
#' @param msg_header,msg_body,msg_footer Content for the header, body, and
#'   footer components of the HTML email message.
#'   
#' @return A **blastula** `email_message` object.
#' 
#' @export 
email_blast_preview <- function(x,
                                msg_header = NULL,
                                msg_body = stock_msg_body(),
                                msg_footer = stock_msg_footer()) {
  
  if (inherits(x, "ptblank_agent")) {
    x <- get_agent_x_list(agent = x)
  }
  
  if (!requireNamespace("blastula", quietly = TRUE) &&
      !requireNamespace("glue", quietly = TRUE)) {
    
    warning("Sending an email message with `email_blast()` requires both the blastula and glue packages:\n",
            " * Install them with `install.packages(\"blastula\")` and `install.packages(\"glue\")`.",
            call. = FALSE)
    
    return()
  }
  
  if (requireNamespace("blastula", quietly = TRUE) &&
      requireNamespace("glue", quietly = TRUE)) {
    
    blastula::compose_email(
      header = glue::glue(msg_header) %>% blastula::md(),
      body = glue::glue(msg_body) %>% blastula::md(),
      footer = glue::glue(msg_footer) %>% blastula::md(),
    )
  }
}

check_msg_components_all_null <- function(msg_header, msg_body, msg_footer) {
  
  if (is.null(msg_header) & is.null(msg_body) & is.null(msg_footer)) {
    warning("There is no content provided for the email message")
  }
}

# nocov end


#' Provide simple email message body components
#' 
#' The `stock_msg_body()` and `stock_msg_footer()` functions simply provide some
#' stock text for an email message sent via [email_blast()] or previewed through
#' [email_blast_preview()].
#' 
#' @name stock_msg_parts
#' @return Text suitable for the `msg_body` and `msg_footer` arguments of
#' [email_blast()] and [email_blast_preview()].
NULL

#' @rdname stock_msg_parts
#' @export
stock_msg_body <- function() {
  "
Here is **pointblank** validation report that was initiated at {x$time}.

{x$report_html_email}
"
}

#' @rdname stock_msg_parts
#' @export
stock_msg_footer <- function() {
  "
Validation performed via the `pointblank` **R** package.
  
[Information and package documentation](https://rich-iannone.github.io/pointblank/)
"
}
