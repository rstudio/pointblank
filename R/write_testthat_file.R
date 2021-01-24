#' Transform a **pointblank** agent to a testthat test file
#' 
#' With a **pointblank** *agent*, we can write a **testthat** test file and opt
#' to place it in the `testthat/tests` if it is available in the project path
#' (we can specify an alternate path as well). This works by transforming the
#' validation steps to a series of `expect_*()` calls inside individual
#' [testthat::test_that()] statements.
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
#' @param skip_on_cran This option will include a [testthat::skip_on_cran()]
#'   call at the top of the file to avoid running the data validation test on
#'   CRAN.
#' 
#' @export
write_testthat_file <- function(agent,
                                name = NULL,
                                path = NULL,
                                overwrite = FALSE,
                                make_read_only = TRUE,
                                skip_on_cran = TRUE) {

  # TODO: Add `skip_on_cran()`
  
  # TODO: Add readonly comment 
  
  # TODO: Get warning and 
  
  # TODO: Enforce that the agent must have a `read_fn`
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
      i_o, assertion_type, brief, eval_active
    ) %>%
    dplyr::group_by(i_o) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(i = i_o) %>%
    dplyr::mutate(i = seq_len(nrow(.)))
  
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
    agent_get_exprs(agent) %>%
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
 
  browser()
  
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
