% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emailing.R
\name{email_blast}
\alias{email_blast}
\title{Conditionally send email during interrogation}
\usage{
email_blast(
  x,
  to,
  from,
  credentials = NULL,
  msg_subject = NULL,
  msg_header = NULL,
  msg_body = stock_msg_body(),
  msg_footer = stock_msg_footer(),
  send_condition = ~TRUE \%in\% x$notify
)
}
\arguments{
\item{x}{A reference to the x-list object prepared internally by the agent.
This version of the x-list is the same as that generated via
\verb{get_agent_x_list(<agent>)} except this version is internally generated and
hence only available in an internal evaluation context.}

\item{to, from}{The email addresses for the recipients and of the sender.}

\item{credentials}{A credentials list object that is produced by either of
the \code{\link[blastula:credential_helpers]{blastula::creds()}}, \code{\link[blastula:credential_helpers]{blastula::creds_anonymous()}},
\code{\link[blastula:credential_helpers]{blastula::creds_key()}}, or \code{\link[blastula:credential_helpers]{blastula::creds_file()}} functions. Please
refer to the \strong{blastula} documentation for information on how to use these
functions.}

\item{msg_subject}{The subject line of the email message.}

\item{msg_header, msg_body, msg_footer}{Content for the header, body, and
footer components of the HTML email message.}

\item{send_condition}{An expression that should evaluate to a logical vector
of length 1. If evaluated as \code{TRUE} then the email will be sent, if \code{FALSE}
then that won't happen. The expression can use x-list variables (e.g.,
\code{x$notify}, \code{x$type}, etc.) and all of those variables can be explored
using the \code{\link[=get_agent_x_list]{get_agent_x_list()}} function. The default expression is \code{~ TRUE \%in\% x$notify}, which results in \code{TRUE} if there are any \code{TRUE} values in
the \code{x$notify} logical vector (i.e., any validation step that results in a
'notify' state).}
}
\value{
Nothing is returned. The end result is the side-effect of
email-sending if certain conditions are met.
}
\description{
The \code{email_blast()} function is useful for sending an email message that
explains the result of a \strong{pointblank} validation. It is powered by the
\strong{blastula} and \strong{glue} packages. This function should be invoked as part
of the \code{end_fns} argument of \code{\link[=create_agent]{create_agent()}}. It's also possible to invoke
\code{email_blast()} as part of the \code{fns} argument of the \code{\link[=action_levels]{action_levels()}}
function (i.e., to send multiple email messages at the granularity of
different validation steps exceeding failure thresholds).

To better get a handle on emailing with \code{email_blast()}, the analogous
\code{\link[=email_create]{email_create()}} function can be used with a \strong{pointblank} agent object.
}
\section{YAML}{


A \strong{pointblank} agent can be written to YAML with \code{\link[=yaml_write]{yaml_write()}} and the
resulting YAML can be used to regenerate an agent (with \code{\link[=yaml_read_agent]{yaml_read_agent()}})
or interrogate the target table (via \code{\link[=yaml_agent_interrogate]{yaml_agent_interrogate()}}). Here is an
example of how the use of \code{email_blast()} inside the \code{end_fns} argument of
\code{\link[=create_agent]{create_agent()}} is expressed in R code and in the corresponding YAML
representation.

R statement:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{create_agent(
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
) \%>\%
  col_vals_gt(a, 1) \%>\%
  col_vals_lt(a, 7) 
}\if{html}{\out{</div>}}

YAML representation:

\if{html}{\out{<div class="sourceCode yaml">}}\preformatted{type: agent
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
}\if{html}{\out{</div>}}
}

\section{Examples}{


For the example provided here, we'll use the included \code{small_table} dataset.
We are also going to create an \code{action_levels()} list object since this is
useful for demonstrating an emailing scenario. It will have absolute values
for the \code{warn} and \code{notify} states (with thresholds of \code{1} and \code{2} 'fail'
units, respectively, for the two states).

\if{html}{\out{<div class="sourceCode r">}}\preformatted{al <- 
  action_levels(
    warn_at = 1,
    notify_at = 2
  )
}\if{html}{\out{</div>}}

Validate that values in column \code{a} from \code{small_tbl} are always greater than
\code{1} (with the \code{col_vals_gt()} validation function), and, that values in \code{a}
or are always less than \code{7}.

The \code{email_blast()} function call is used in a list given to the \code{end_fns}
argument of \code{create_agent()}. The \code{email_blast()} call itself has a
\code{send_condition} argument that determines whether or not an email will be
sent. By default this is set to \code{~ TRUE \%in\% x$notify}. Let's unpack this a
bit. The variable \code{x} is a list (we call it an x-list) and it will be
populated with elements pertaining to the agent. After interrogation, and
only if action levels were set for the \code{notify} state, \code{x$notify} will be
present as a logical vector where the length corresponds to the number of
validation steps. Thus, if any of those steps entered the \code{notify} state
(here, it would take two or more failing test units, per step, for that to
happen), then the statement as a whole is \code{TRUE} and the email of the
interrogation report will be sent. Here is the complete set of statements for
the creation of an \emph{agent}, the addition of validation steps, and the
interrogation of data in \code{small_table}:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{agent <-
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
        send_condition = ~ TRUE \%in\% x$notify
      )
    )
  ) \%>\%
  col_vals_gt(a, value = 1) \%>\%
  col_vals_lt(a, value = 7) \%>\%
  interrogate()
}\if{html}{\out{</div>}}

The reason for the \code{~} present in the statements:
\itemize{
\item \code{~ email_blast(...)} and
\item \code{~ TRUE \%in\% x$notify}
}

is because this defers evocation of the emailing functionality (and also
defers evaluation of the \code{send_condition} value) until interrogation is
complete (with \code{\link[=interrogate]{interrogate()}}).
}

\section{Function ID}{

4-1
}

\seealso{
Other Emailing: 
\code{\link{email_create}()},
\code{\link{stock_msg_body}()},
\code{\link{stock_msg_footer}()}
}
\concept{Emailing}
