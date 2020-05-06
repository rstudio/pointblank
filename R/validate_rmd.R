test_options <- new.env(parent = emptyenv())

#' Perform **pointblank** validation testing within R Markdown documents
#' 
#' The `validate_rmd()` function sets up a framework for validation testing
#' within specialized validation code chunks inside an R Markdown document. To
#' enable this functionality, `validate_rmd()` should be called early within an
#' R Markdown document code chunk (preferably in the `setup` chunk) to signal
#' that validation should occur within specific code chunks. The validation code
#' chunks require the `validate = TRUE` option to be set. Using **pointblank**
#' validation step functions on data in these marked code chunks will flag
#' overall failure if the stop threshold is exceeded anywhere. All errors are
#' reported in the validation code chunk after rendering the document to HTML,
#' where a centered status button either indicates success or the number of
#' overall failures. Clicking the button reveals the otherwise hidden validation
#' statements and their error messages (if any).
#'
#' @param summary If `TRUE` (the default), then there will be a leading summary
#'   of all validations in the rendered R Markdown document. With `FALSE`, this
#'   element is not shown.
#' @param log_to_file An option to log errors to a text file. By default, no
#'   logging is done but `TRUE` will write log entries to
#'   `"validation_errors.log"` in the working directory. To both enable logging
#'   and to specify a file name, include a path to a log file of the desired
#'   name.
#' @param theme A choice of which theme to use. Currently, the only available
#'   option is `"default"`.
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-3
#'
#' @export
validate_rmd <- function(summary = TRUE,
                         log_to_file = NULL,
                         theme = "default") {
  
  theme <- match.arg(theme)
  
  knitr::opts_hooks$set(
    error = function(options) {
      if (isTRUE(options$validate)) {
        options$error = TRUE
      }
      options
    }
  )
  
  error <- knitr_error_hook(knitr::knit_hooks$get("error"))
  document <- knitr_document_hook(knitr::knit_hooks$get("document"))
  
  # TODO: this isn't currently used (but may be useful later)
  # evaluate <- knitr_evaluate_hook(knitr::knit_hooks$get("evaluate"))
  
  knitr::knit_hooks$set(
    chunk = knitr_chunk_hook,
    error = error,
    document = document
  )
  
  reset_doc_counts()
  
  # Store the `summary` and `theme` values to `test_options`
  test_options$summary <- summary
  test_options$theme <- theme
  
  # Determine whether file logging with `log4r` is to be done and also
  # determine the filename of the logging file
  if (!is.null(log_to_file)) {
    
    if (isTRUE(log_to_file)) {
      
      # TODO: consider appending the date and time to the
      # generic `validation_errors.log` filename
      test_options$perform_logging <- TRUE
      test_options$log_to_file <- file.path(getwd(), "validation_errors.log")
      
    } else if (is.character(log_to_file)) {
      
      test_options$perform_logging <- TRUE
      test_options$log_to_file <- log_to_file
      
    } else if (!isTRUE(log_to_file) && is.logical(log_to_file)) {
      test_options$perform_logging <- FALSE
    }
  } else {
    test_options$perform_logging <- FALSE
  }
  
  if (test_options$perform_logging) {
    
    # Create a log4r `logger` object and store it in `test_options`
    test_options$logger <- 
      log4r::logger(
        threshold = "ERROR",
        appenders = log4r::file_appender(test_options$log_to_file)
      )
  }
  
  validate_rmd_dependencies()
}

log4r_error <- function(message) {
  
  if (test_options$perform_logging) {
    log4r::error(logger = test_options$logger, message = message)
  }
}

increment_count <- function(type) {
  (current_chunk_counts[[type]] <- current_chunk_counts[[type]] + 1L)
  (current_doc_counts[[type]] <- current_doc_counts[[type]] + 1L)
}
reset_chunk_counts <- function() {
  current_chunk_counts$error <- 0
  current_chunk_counts$pass <- 0
}
reset_doc_counts <- function() {
  reset_chunk_counts()
  current_doc_counts$error <- 0
  current_doc_counts$pass <- 0
}
get_chunk_count <- function(type) {
  current_chunk_counts[[type]]
}
get_doc_count <- function(type) {
  current_doc_counts[[type]]
}

current_chunk_counts <- new.env(parent = emptyenv())
current_doc_counts <- new.env(parent = emptyenv())
reset_doc_counts()

validate_rmd_dependencies <- function() {
  
  htmltools::attachDependencies(
    htmltools::tagList(),
    htmltools::htmlDependency(
      name = "rmd_integration",
      version = utils::packageVersion("pointblank"),
      src = system.file("css", package = "pointblank"),
      stylesheet = "rmd_styles.css"
    )
  )
}

render_template <- function(template_name, data) {
  
  theme <- test_options$theme
  
  path <- 
    system.file(
      "templates", theme, paste0(template_name, ".html"),
      package = "pointblank"
    )
  
  if (!nzchar(path)) {
    stop("The template `", template_name, "` was not found.")
  }
  
  template <- paste(readLines(path, warn = FALSE), collapse = "\n")
  
  if (template_name == "chunk") {
    
    text <- pb_glue_data(data, "{error_count} {noun} failed")
    
    if (data$pass) {
      
      state <- "info"  
      text <- "All validations passed."
      
    } else {
      
      state <- "danger"
      text <- pb_glue_data(data, "{error_count} {noun} failed.")
    }
    
    rendered <- pb_glue_data(c(data, list(state = state, text = text)), template)
    
  } else if (template_name == "document") {
    
    if (!data$pass) {
      
      alert <- 
        pb_glue_data(
          data, 
          htmltools::htmlPreserve(
            htmltools::tags$div(
              class = "alert alert-danger",
              htmltools::tags$strong("Warning:"),
              "this document contains {error_count} failing {noun}."
            )
          )
        )
      
    } else {
      alert <- ""
    }
    
    rendered <- pb_glue_data(c(data, list(alert = alert)), template)
  }
  
  rendered
}

knitr_error_hook <- function(previous_hook) {
  force(previous_hook)
  
  function(x, options) {
    
    if (isTRUE(options$validate)) {

      increment_count("error")
      
      # TODO: Integrate the errored code with the error message 
      # error_code <- options$code
      
      error_message <- x %>% tidy_gsub("##", "") %>% tidy_gsub("\n", "")
      log4r_error(message = error_message)
    }
    
    previous_hook(x, options)
  }
}

knitr_document_hook <- function(previous_hook) {
  force(previous_hook)
  
  function(x) {
    if (!isTRUE(test_options$summary)) {
      return(previous_hook(x))
    }
    
    content <- previous_hook(x)
    content <- paste(content, collapse = "\n")
    
    matches <- regexec("^(.*)\r?\n---\r?\n(.*)$", content)
    matches <- regmatches(content, matches)
    
    header <- matches[[1]][2]
    body <- matches[[1]][3]
    
    error_count <- get_doc_count("error")
    pass <- error_count == 0
    
    data <- 
      list(
        content = body,
        pass = pass,
        error_count = error_count,
        noun = if (error_count == 1) "validation" else "validations"
      )
    
    c(header, "---", render_template("document", data))
  }
}

knitr_evaluate_hook <- function(previous_hook) {
  force(previous_hook)
  
  function(...) {
    withCallingHandlers(
      previous_hook(...),
      expectation_success = function(e) {
        increment_count("pass")
      }
    )
  }
}

knitr_chunk_hook <- function(x, options) {
  
  if (!isTRUE(options$validate)) {
    return(x)
  }
  
  on.exit(reset_chunk_counts(), add = TRUE)
  
  if (options$eval == FALSE) {
    return("")
  }
  
  error_count <- get_chunk_count("error")
  pass <- error_count == 0
  
  data <- 
    list(
      chunk_id = sprintf("chunk-%07d", sample.int(9999999, 1)),
      button_class = "default",
      bootstrap_class = if (pass) "info" else "danger",
      status = if (pass) "pass" else "fail",
      pass = pass,
      pass_count = get_chunk_count("pass"),
      error_count = error_count,
      content = paste(x, collapse = "\n"),
      noun = if (error_count == 1) "validation" else "validations"
    )
  
  render_template("chunk", data)
}
