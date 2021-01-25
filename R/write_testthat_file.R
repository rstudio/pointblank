#' Transform a **pointblank** agent to a testthat test file
#' 
#' With a **pointblank** *agent*, we can write a **testthat** test file and opt
#' to place it in the `testthat/tests` if it is available in the project path
#' (we can specify an alternate path as well). This works by transforming the
#' validation steps to a series of `expect_*()` calls inside individual
#' [testthat::test_that()] statements. A hard requirement for using
#' `write_testthat_file()` on an agent is the presence of a `read_fn`, which is
#' a function that is invoked to obtain the target table. The `read_fn`
#' statement will be placed at the top of the **testthat** test file so that the
#' target table is available for each of the [testthat::test_that()] statements
#' that follow. If an *agent* does not have a `read_fn` it can be added via the
#' [set_read_fn()].
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' @param name An optional name for for the **testhat** test file. This should
#'   be a name without extension and without the leading `"test-"` text. If
#'   nothing is supplied, the name will be derived from the `tbl_name` in the
#'   agent. If that's not present, a generic name will be used.
#' @param path A path can be specified here if there shouldn't be an attempt to
#'   place the file in `testthat/tests`.
#' @param overwrite Should a **testthat** file of the same name be overwritten?
#'   By default, this is `FALSE`.
#' @param make_read_only Should the file to be written be read only? If `TRUE`
#'   (the default), it will be read-only and text at the top of the file will
#'   indicate this.
#' @param skips This is an optional vector of test-skipping keywords modeled
#'   after the **testthat** `skip_on_*()` functions. The following keywords can
#'   be used to include `skip_on_*()` statements: `"cran"`
#'   ([testthat::skip_on_cran()]), `"travis"` ([testthat::skip_on_travis()]),
#'   `"appveyor"` ([testthat::skip_on_appveyor()]), `"ci"`
#'   ([testthat::skip_on_ci()]), `"covr"` ([testthat::skip_on_covr()]), `"bioc"`
#'   ([testthat::skip_on_bioc()]). There are keywords for skipping tests on
#'   certain operating systems and all of them will insert a specific
#'   [testthat::skip_on_os()] call. These are `"windows"`
#'   (`skip_on_os("windows")`), `"mac"` (`skip_on_os("mac")`), `"linux"`
#'   (`skip_on_os("linux")`), and `"solaris"` (`skip_on_os("solaris")`). These
#'   calls will be placed at the top of the generated **testthat** test file.
#'   
#' @return Invisibly returns `TRUE` if the **testthat** file has been written. 
#' 
#' @export
write_testthat_file <- function(agent,
                                name = NULL,
                                path = NULL,
                                overwrite = FALSE,
                                make_read_only = TRUE,
                                skips = NULL) {

  # Enforce that the agent must have a `read_fn`
  if (is.null(agent$read_fn)) {
    stop(
      "The agent must have a `read_fn` value when transforming into tests.",
      call. = FALSE
    )
  }

  # Select only the necessary columns from the agent's `validation_set` 
  agent_validation_set <- 
    agent$validation_set %>% 
    dplyr::select(
      i, assertion_type, brief, eval_active
    )
  
  # Create a string that will be used to read the table (at the top
  # of the testthat test file)
  read_tbl_str <-
    paste0(
      "tbl <- ",
      as.character(rlang::f_rhs(agent$read_fn))
    )
  
  # Obtain expressions from the agent that correspond to the
  # validation function calls
  
  # Using the `expanded = TRUE` option in `agent_get_exprs()` so that
  # that the expanded form of the validation steps is available
  # (same number of steps as in the validation report)
  agent_exprs_raw <- 
    agent_get_exprs(agent, expanded = TRUE) %>%
    strsplit("%>%") %>%
    unlist() %>%
    gsub("^\n", "", .)
  
  if (grepl("^create_agent", agent_exprs_raw[1])) {
    agent_exprs_raw <- agent_exprs_raw[-1]
  }
  
  agent_exprs_raw <-
    vapply(
      agent_exprs_raw,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        if (grepl("^create_agent", x)) {
          return(x)
        }
        x <- gsub("\\(\n\\s+", "(\n  tbl,\n  ", x)
        x <- gsub("^", "expect_", x)
      }
    )
  
  test_that_desc <- 
    agent_validation_set$brief %>%
    gsub("(Expect that |Expect )", "", .) %>%
    gsub(" $", "", .) %>%
    gsub("\\.$", "", .)
  
  # Initialize vector of `test_that()` tests 
  test_that_tests <- c()
  
  for (i in seq_along(agent_exprs_raw)) {
    
    # TODO: insert threshold value
    test_that_tests <-
      c(test_that_tests,
        paste0(
          "test_that(\"",
          test_that_desc[i],
          "\", {\n\n",
          agent_exprs_raw[i] %>%
            gsub("^", "  ", .) %>%
            gsub("\n  ", "\n    ", .) %>%
            gsub("\n\\)", "\n  )", .),
          "\n})\n\n"
        )
      )
  }

  # Paste the `testthat` test strings together
  test_that_tests <- 
    paste0(
      paste0(read_tbl_str, "\n", "\n"),
      paste(test_that_tests,
            collapse = ""
      ),
      collapse = ""
    )
  
  # Process the `skips` vector
  skips_text <- process_skips_text(skips)
  
  if (!is.null(skips_text)) {
    test_that_tests <-
      paste0(
        skips_text,
        test_that_tests,
        collapse = ""
      )
  }
  
  if (make_read_only) {
    test_that_tests <-
      paste0(
        "# Generated by pointblank: do not edit by hand\n\n",
        test_that_tests,
        collapse = ""
      )
  }

  # Remove trailing newlines  
  test_that_tests <- gsub("\n\n$", "", test_that_tests)
  
  # Create the filename for the testthat test file
  file_name <- resolve_test_filename(agent = agent, name = name)
  
  # Determine if there is a path `tests/testthat`
  if (is.null(path) && fs::dir_exists("tests/testthat")) {
    file_path <- "tests/testthat"
  } else if (is.null(path) && !fs::dir_exists("tests/testthat")) {
    file_path <- "."
  } else if (!is.null(path) && !fs::dir_exists(path)) {
    # Stop function and inform user that this function
    # won't create a path
    stop(
      "The provided `path` does not exist:\n",
      "* Please create the path"
    )
  } else if (!is.null(path) && fs::dir_exists(path)) {
    file_path <- path
  }

  # Create path that contains the testthat test file name
  path <- as.character(fs::path_wd(file_path, file_name))
  
  # TODO: check if file exists; if it does, don't write file 
  # if `overwrite` is FALSE
  
  pb_write_file(
    path = path,
    lines = test_that_tests,
    append = FALSE
  )
  
  if (make_read_only) {
    fs::file_chmod(path = path, mode = "0444")
  }
  
  invisible(TRUE)
}

get_thresholds <- function(agent, type) {
  
  vapply(
    agent$validation_set$actions,
    FUN.VALUE = numeric(1), 
    USE.NAMES = FALSE,
    FUN = function(x) {
      
      type_fraction <- x[[paste0(type, "_fraction")]]
      type_count <- x[[paste0(type, "_count")]]
      
      if (is.null(type_fraction) && is.null(type_count)) {
        x <- NA_real_
      } else if (!is.null(type_fraction)) {
        x <- type_fraction
      } else {
        x <- type_count
      }
      
      x
    }
  )
}

insert_threshold_values <- function(agent_exprs_raw,
                                    threshold_vals) {
  
  vapply(
    seq_along(agent_exprs_raw),
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(x) {
      
      if (
        grepl(
          "(^expect_col_is_|^expect_col_exists|^expect_col_schema_match)",
          agent_exprs_raw[x]
        )
      ) {
        threshold_val <- 1
      } else {
        threshold_val <- threshold_vals[x]
      }
      
      if (is.na(threshold_vals[x])) {
        threshold_val <- 1
      }
      
      agent_exprs_raw[x] %>% 
        gsub("\n\\)", paste0(",\n  threshold = ", threshold_val, "\n\\)"), .)
    }
  )
}

resolve_test_filename <- function(agent,
                                  name) {
  
  if (is.null(name)) {
    if (is.null(agent$tbl_name) ||
        is.na(agent$tbl_name) ||
        agent$tbl_name == "") {
      
      file_name <- "test-pointblank_validation.R"
      
    } else {
      
      file_name <- 
        agent$tbl_name %>%
        fs::path_sanitize() %>%
        gsub("(\\.| |'|:)", "_", .) %>%
        paste0("test-", .) %>%
        paste0(., ".R")
    }
    
  } else {
    
    if (!is.character(name)) {
      stop(
        "The value supplied to `name` must be of class 'character'.",
        call. = FALSE
      )
    }
    
    file_name <- 
      name[1] %>%
      fs::path_sanitize() %>%
      gsub("(\\.| |'|:)", "_", .) %>%
      paste0("test-", .) %>%
      paste0(., ".R")
  }
  
  file_name
}

process_skips_text <- function(skips) {
  
  if (is.null(skips) || !is.character(skips) || skips == "") {
    return(NULL)
  }
  
  skips_keywords <-
    c(
      "cran", "travis", "appveyor", "ci", "covr", "bioc",
      "windows", "mac", "linux", "solaris"
    )
  
  skips_keywords_os <- c("windows", "mac", "linux", "solaris")
  skips_keywords_non_os <- base::setdiff(skips_keywords, skips_keywords_os)
  
  if (!all(skips %in% skips_keywords)) {
    stop("All values provided in `skips` must be valid skipping keywords.",
         call. = FALSE)
  }
  
  skips_text <- 
    vapply(
      unique(skips),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        if (x %in% skips_keywords_non_os) {
          x <- paste0("skip_on_", x, "()\n")
        }
        
        if (x %in% skips_keywords_os) {
          x <- paste0("skip_on_os(\"", x, "\")\n")
        }
        
        x
      }
    )
  
  paste0(paste0(skips_text, collapse = ""), "\n")
}

pb_write_file <- function(path,
                          lines,
                          append = FALSE,
                          line_ending = NULL) {
  
  stopifnot(is.character(path))
  stopifnot(is.character(lines))
  
  if (append) {
    file_mode <- "ab"
  } else {
    file_mode <- "wb"
  }

  # Create a file connection  
  file_connection <- file(path, open = file_mode, encoding = "utf-8")
  
  on.exit(close(file_connection))
  
  # Obtain the appropriate line ending based on the platform
  if (.Platform$OS.type == "windows") {
    line_ending <- "\r\n"
  } else {
    line_ending <- "\n"
  }
  
  lines <- gsub("\r?\n", line_ending, lines)
  
  writeLines(
    text = enc2utf8(lines),
    con = file_connection,
    sep = line_ending, 
    useBytes = TRUE
  )
  
  invisible(TRUE)
}
