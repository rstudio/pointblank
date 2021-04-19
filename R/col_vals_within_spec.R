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


#' Do values in column data fit within a specification?
#' 
#' @description
#' The `col_vals_within_spec()` validation function, the
#' `expect_col_vals_within_spec()` expectation function, and the
#' `test_col_vals_within_spec()` test function all check whether column values
#' in a table correspond to a specification (`spec`) type (details of which are
#' available in the *Specifications* section). The validation step function
#' can be used directly on a data table or with an *agent* object (technically,
#' a `ptblank_agent` object) whereas the expectation and test functions can only
#' be used with a data table. The types of data tables that can be used include
#' data frames, tibbles, database tables (`tbl_dbi`), and Spark DataFrames
#' (`tbl_spark`). Each validation step or expectation will operate over the
#' number of test units that is equal to the number of rows in the table (after
#' any `preconditions` have been applied).
#' 
#' @section Specifications:
#' A specification type must be used with the `spec` argument. This is a
#' character-based keyword that corresponds to the type of data in the specified
#' `columns`. The following keywords can be used:
#' 
#' - `"isbn"`: The International Standard Book Number (ISBN) is a unique
#' numerical identifier for books, pamphletes, educational kits, microforms, and
#' digital/electronic publications. The specification has been formalized in
#' ISO 2108. This keyword can be used to validate 10- or 13-digit ISBNs.
#' - `"VIN"`: A vehicle identification number (VIN) is a unique code (which
#' includes a serial number) used by the automotive industry to identify
#' individual motor vehicles, motorcycles, scooters, and mopeds as stipulated
#' by ISO 3779 and ISO 4030.
#' - `"postal_code[<country_code>]"`: A postal code (also known as postcodes,
#' PIN, or ZIP codes, depending on region) is a series of letters, digits, or
#' both (sometimes including spaces/punctuation) included in a postal address to
#' aid in sorting mail. Because the coding varies by country, a country code in
#' either the 2- (ISO 3166-1 alpha-2) or 3-letter (ISO 3166-1 alpha-3) formats
#' needs to be supplied along with the keywords (e.g., for postal codes in
#' Germany, `"postal_code[DE]"` or `"postal_code[DEU]"` can be used). The
#' keyword alias `"zip"` can be used for US ZIP codes.
#' - `"credit_card"`: A credit card number can be validated and this check works
#' across a large variety of credit type issuers (where card numbers are
#' allocated in accordance with ISO/IEC 7812). Numbers can be of various lengths
#' (typically, they are of 14-19 digits) and the key validation performed here
#' is the usage of the Luhn algorithm.
#' - `"iban[<country_code>]"`: The International Bank Account Number (IBAN) is a
#' system of identifying bank accounts across different countries for the
#' purpose of improving cross-border transactions. IBAN values are validated
#' through conversion to integer values and performing a basic mod-97 operation
#' (as described in ISO 7064) on them. Because the length and coding varies by
#' country, a country code in either the 2- (ISO 3166-1 alpha-2) or 3-letter
#' (ISO 3166-1 alpha-3) formats needs to be supplied along with the keywords
#' (e.g., for IBANs in Germany, `"iban[DE]"` or `"iban[DEU]"` can be used).
#' - `"swift"`: Business Identifier Codes (also known as SWIFT-BIC, BIC,
#' or SWIFT code) are defined in a standard format as described by ISO 9362.
#' These codes are unique identifiers for both financial and non-financial
#' institutions. SWIFT stands for the Society for Worldwide Interbank Financial
#' Telecommunication. These numbers are used when transferring money between
#' banks, especially important for international wire transfers.
#' - `"phone"`, `"email"`, `"url"`, `"ipv4"`, `"ipv6"`, `"mac"`: Phone numbers,
#' email addresses, Internet URLs, IPv4 or IPv6 addresses, and MAC addresses can
#' be validated with their respective keywords. These validations use
#' regex-based matching to determine validity.
#' 
#' Only a single `spec` value should be provided per function call.
#'
#' @section Column Names:
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names in
#' quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
#'
#' @section Missing Values:
#' This validation function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of `na_pass =
#' FALSE` means that any `NA`s encountered will accumulate failing test units.
#' 
#' @section Preconditions:
#' Having table `preconditions` means **pointblank** will mutate the table just
#' before interrogation. Such a table mutation is isolated in scope to the
#' validation step(s) produced by the validation function call. Using
#' **dplyr** code is suggested here since the statements can be translated to
#' SQL if necessary. The code is most easily supplied as a one-sided **R**
#' formula (using a leading `~`). In the formula representation, the `.` serves
#' as the input data table to be transformed (e.g., 
#' `~ . %>% dplyr::mutate(col_a = col_b + 10)`). Alternatively, a function could
#' instead be supplied (e.g., 
#' `function(x) dplyr::mutate(x, col_a = col_b + 10)`).
#' 
#' @section Actions:
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction of
#' test units failed, or, an absolute value), often using the `warn_at`
#' argument. This is especially true when `x` is a table object because,
#' otherwise, nothing happens. For the `col_vals_*()`-type functions, using 
#' `action_levels(warn_at = 0.25)` or `action_levels(stop_at = 0.25)` are good
#' choices depending on the situation (the first produces a warning when a
#' quarter of the total test units fails, the other `stop()`s at the same
#' threshold level).
#' 
#' @section Briefs:
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#' 
#' @section YAML:
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). When
#' `col_vals_within_spec()` is represented in YAML (under the top-level `steps`
#' key as a list member), the syntax closely follows the signature of the
#' validation function. Here is an example of how a complex call of
#' `col_vals_within_spec()` as a validation step is expressed in R code and in
#' the corresponding YAML representation.
#' 
#' ```
#' # R statement
#' agent %>% 
#'   col_vals_within_spec(
#'     columns = vars(a),
#'     spec = "email",
#'     na_pass = TRUE,
#'     preconditions = ~ . %>% dplyr::filter(b < 10),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2),
#'     label = "The `col_vals_within_spec()` step.",
#'     active = FALSE
#'   )
#' 
#' # YAML representation
#' steps:
#' - col_vals_within_spec:
#'     columns: vars(a)
#'     spec: email
#'     na_pass: true
#'     preconditions: ~. %>% dplyr::filter(b < 10)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `col_vals_within_spec()` step.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the `columns` and
#' `spec` arguments require values. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'
#' @inheritParams col_vals_gt
#' @param spec A specification string. Examples are `"email"`, `"url"`, and
#'   `"postal[USA]"`. All options are explained in the *Specifications* section.
#' 
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#' 
#' 
#' @family validation functions
#' @section Function ID:
#' 2-18
#' 
#' @name col_vals_within_spec
NULL

#' @rdname col_vals_within_spec
#' @import rlang
#' @export
col_vals_within_spec <- function(x,
                                 columns,
                                 spec,
                                 na_pass = FALSE,
                                 preconditions = NULL,
                                 actions = NULL,
                                 step_id = NULL,
                                 label = NULL,
                                 brief = NULL,
                                 active = TRUE) {
  
  # Get `columns` as a label
  columns_expr <- 
    rlang::as_label(rlang::quo(!!enquo(columns))) %>%
    gsub("^\"|\"$", "", .)
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  # Perform checks of `spec` value
  spec <- normalize_spec(spec = spec)
  
  if (is_a_table_object(x)) {
    
    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      col_vals_within_spec(
        columns = columns,
        spec = spec,
        na_pass = na_pass,
        preconditions = preconditions,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()
    
    return(x)
  }
  
  agent <- x

  if (is.null(brief)) {
    brief <- 
      generate_autobriefs(
        agent,
        columns,
        preconditions,
        values = spec,
        "col_vals_within_spec"
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns, agent)
  
  # Get the next step number for the `validation_set` tibble
  i_o <- get_next_validation_set_row(agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_within_spec",
        i_o = i_o,
        columns_expr = columns_expr,
        column = columns[i],
        values = spec,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = covert_actions(actions, agent),
        step_id = step_id[i],
        label = label,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_vals_within_spec
#' @import rlang
#' @export
expect_col_vals_within_spec <- function(object,
                                        columns,
                                        spec,
                                        na_pass = FALSE,
                                        preconditions = NULL,
                                        threshold = 1) {
  
  fn_name <- "expect_col_vals_within_spec"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_within_spec(
      columns = {{ columns }},
      spec = {{ spec }},
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  x <- vs$notify %>% all()
  
  threshold_type <- get_threshold_type(threshold = threshold)
  
  if (threshold_type == "proportional") {
    failed_amount <- vs$f_failed
  } else {
    failed_amount <- vs$n_failed
  }
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  column_text <- prep_column_text(vs$column[[1]])
  values_text <- prep_values_text(values = vs$values, limit = 3, lang = "en")
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(
      failure_message_gluestring(
        fn_name = fn_name, lang = "en"
      )
    )
  )
  
  act$val <- object
  
  invisible(act$val)
}

#' @rdname col_vals_within_spec
#' @import rlang
#' @export
test_col_vals_within_spec <- function(object,
                                      columns,
                                      spec,
                                      na_pass = FALSE,
                                      preconditions = NULL,
                                      threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_within_spec(
      columns = {{ columns }},
      spec = {{ spec }},
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  all(!vs$notify)
}

country_has_postal_code_fmt <- function(country) {
  
  code_tbl <- 
    dplyr::select(countries, alpha_2, alpha_3, postal_code_format) %>%
    dplyr::filter(!is.na(postal_code_format))
  
  country_codes <-
    c(dplyr::pull(code_tbl, alpha_2), dplyr::pull(code_tbl, alpha_3))
  
  tolower(country) %in% tolower(country_codes)
}

country_has_iban_fmt <- function(country) {
  
  country_codes <-
    c(
      "AL", "ALB",
      "AD", "AND",
      "AE", "ARE",
      "AT", "AUT",
      "BE", "BEL",
      "BG", "BGR",
      "BA", "BIH",
      "BR", "BRA",
      "CH", "CHE",
      "CI", "CIV",
      "CY", "CYP",
      "CZ", "CZE",
      "DE", "DEU",
      "DK", "DNK",
      "ES", "ESP",
      "EE", "EST",
      "FI", "FIN",
      "FR", "FRA",
      "PF", "PYF",
      "TF", "ATF",
      "GP", "GLP",
      "MQ", "MTQ",
      "YT", "MYT",
      "NC", "NCL",
      "RE", "REU",
      "BL", "BLM",
      "MF", "MAF",
      "PM", "SPM",
      "WF", "WLF",
      "FO", "FRO",
      "GB", "GBR",
      "GE", "GEO",
      "GI", "GIB",
      "GR", "GRC",
      "GL", "GRL",
      "HR", "HRV",
      "HU", "HUN",
      "IE", "IRL",
      "IS", "ISL",
      "IL", "ISR",
      "IT", "ITA",
      "KZ", "KAZ",
      "KW", "KWT",
      "LB", "LBN",
      "LI", "LIE",
      "LT", "LTU",
      "LU", "LUX",
      "LV", "LVA",
      "MC", "MCO",
      "MK", "MKD",
      "MT", "MLT",
      "ME", "MNE",
      "MR", "MRT",
      "MU", "MUS",
      "NL", "NLD",
      "NO", "NOR",
      "PL", "POL",
      "PT", "PRT",
      "RO", "ROU",
      "SA", "SAU",
      "SM", "SMR",
      "RS", "SRB",
      "SK", "SVK",
      "SI", "SVN",
      "SE", "SWE",
      "TN", "TUN",
      "TR", "TUR"
    )
  
  tolower(country) %in% tolower(country_codes)
}

normalize_spec <- function(spec) {
  
  spec <- tolower(spec)
  
  # nolint start
  
  spec_correct <- 
    grepl("(iban|postal|zip|phone|cc|credit_?card|vin|isbn.*|swift.*?|email|url|ipv4|ipv6|mac.*?|)", spec) 
  
  # nolint end
  
  if (!spec_correct) {
    stop(
      "The value for `spec` doesn't correspond to a specification type:\n",
      "* Keywords such as 'email', 'url', 'isbn10', etc. can be used",
      call. = FALSE
    )
  }
  
  if (grepl("iban", spec)) {
    
    if (!grepl("iban\\[[a-z]{2,3}\\]", spec)) {
      stop(
        "The IBAN code specification must include a country identifier:\n",
        "* The accepted form is: `\"iban[<country>]\"`\n",
        "* The <country> identifier can be a 2- or 3-letter country code",
        call. = FALSE
      )
    }
    
    country <- gsub("(iban\\[|\\])", "", spec)
    
    if (!country_has_iban_fmt(country = country)) {
      stop(
        "The country supplied in the IBAN `spec` is not supported.",
        call. = FALSE
      )
    }
  }
  
  if (grepl("^zip$", spec)) {
    spec <- gsub("zip", "postal[usa]", spec)
  }
  if (grepl("postal", spec)) {
    
    if (!grepl("postal\\[[a-z]{2,3}\\]", spec)) {
      stop(
        "The postal code specification must include a country identifier:\n",
        "* The accepted form is: `\"postal[<country>]\"`\n",
        "* The <country> identifier can be a 2- or 3-letter country code\n",
        "* The short form `\"zip\"` will resolve to `\"postal[usa]\"`",
        call. = FALSE
      )
    }
    
    country <- gsub("(postal\\[|\\])", "", spec)
    
    if (!country_has_postal_code_fmt(country = country)) {
      stop(
        "The country supplied in the postal code `spec` is not supported.",
        call. = FALSE
      )
    }
  }
  
  if (grepl("(cc|credit_?card)", spec)) {
    spec <- "creditcard"
  }
  
  if (grepl("swift.*?", spec)) {
    spec <- "swift"
  }
  
  if (grepl("isbn.*?", spec)) {
    spec <- "isbn"
  }
  
  if (grepl("mac.*?", spec)) {
    spec <- "mac"
  }
  
  spec
}
