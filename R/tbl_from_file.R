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


# nocov start

#' Get a table from a local or remote file
#' 
#' @description 
#' If your target table is in a file, stored either locally or remotely, the
#' `file_tbl()` function can make it possible to access it in a single function
#' call. Compatible file types for this function are: CSV (`.csv`), TSV
#' (`.tsv`), RDA (`.rda`), and RDS (`.rds`) files. This function generates an
#' in-memory `tbl_df` object, which can be used as a target table for
#' [create_agent()] and [create_informant()]. Another great option is supplying
#' a table-prep formula involving `file_tbl()` to [tbl_store()] so that you have
#' access to tables based on flat files though single names via a table store.
#'
#' In the remote data use case, we can specify a URL starting with `http://`,
#' `https://`, etc., and ending with the file containing the data table. If data
#' files are available in a GitHub repository then we can use the
#' [from_github()] function to specify the name and location of the table data
#' in a repository.
#' 
#' @param file The complete file path leading to a compatible data table either
#'   in the user system or at a `http://`, `https://`, `ftp://`, or `ftps://`
#'   URL. For a file hosted in a GitHub repository, a call to the
#'   [from_github()] function can be used here.
#' @param type The file type. This is normally inferred by file extension and is
#'   by default `NULL` to indicate that the extension will dictate the type of
#'   file reading that is performed internally. However, if there is no
#'   extension (and valid extensions are `.csv`, `.tsv`, `.rda`, and `.rds`), we
#'   can provide the type as either of `csv`, `tsv`, `rda`, or `rds`.
#' @param ... Options passed to **readr**'s `read_csv()` or `read_tsv()`
#'   function. Both functions have the same arguments and one or the other will
#'   be used internally based on the file extension or an explicit value given
#'   to `type`.
#' @param keep In the case of a downloaded file, should it be stored in the
#'   working directory (`keep = TRUE`) or should it be downloaded to a temporary
#'   directory? By default, this is `FALSE`.
#' @param verify If `TRUE` (the default) then a verification of the data object
#'   having the `data.frame` class will be carried out.
#'   
#' @return A `tbl_df` object.
#' 
#' @section Examples:
#' 
#' ## Producing tables from CSV files
#' 
#' A local CSV file can be obtained as a tbl object by supplying a path to the
#' file and some CSV reading options (the ones used by `readr::read_csv()`) to
#' the `file_tbl()` function. For this example we could obtain a path to a CSV
#' file in the **pointblank** package with `system.file()`.
#' 
#' ```r
#' csv_path <- 
#'   system.file(
#'     "data_files", "small_table.csv",
#'     package = "pointblank"
#'   )
#' ```
#' 
#' Then use that path in `file_tbl()` with the option to specify the column
#' types in that CSV.
#' 
#' ```r
#' tbl <- 
#'   file_tbl(
#'     file = csv_path,
#'     col_types = "TDdcddlc"
#'   )
#' 
#' tbl
#' ```
#' 
#' \preformatted{## # A tibble: 13 × 8
#' ##    date_time           date           a b           c      d e     f    
#' ##    <dttm>              <date>     <dbl> <chr>   <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high 
#' ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-…     7   834. TRUE  low  
#' ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-…     8   108. FALSE low  
#' ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-…    NA  2230. TRUE  high}
#' 
#' 
#' 
#' Now that we have a `tbl` object that is a tibble it could be introduced to
#' [create_agent()] for validation.
#' 
#' ```r
#' agent <- create_agent(tbl = tbl)
#' ```
#'
#' A different strategy is to provide the data-reading function call directly to
#' [create_agent()]:
#' 
#' ```r
#' agent <- 
#'   create_agent(
#'     tbl = ~ file_tbl(
#'       file = system.file(
#'         "data_files", "small_table.csv",
#'         package = "pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     )
#'   ) %>%
#'   col_vals_gt(columns = vars(a), value = 0)
#' ```
#'
#' All of the file-reading instructions are encapsulated in the `tbl` expression
#' (with the leading `~`) so the agent will always obtain the most recent
#' version of the table (and the logic can be translated to YAML, for later
#' use).
#' 
#' ## Producing tables from files on GitHub
#' 
#' A CSV can be obtained from a public GitHub repo by using the [from_github()]
#' helper function. Let's create an agent a supply a table-prep formula that
#' gets the same CSV file from the GitHub repository for the pointblank package.
#' 
#' ```r
#' agent <- 
#'   create_agent(
#'     tbl = ~ file_tbl(
#'       file = from_github(
#'         file = "inst/data_files/small_table.csv",
#'         repo = "rich-iannone/pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     ),
#'     tbl_name = "small_table",
#'     label = "`file_tbl()` example.",
#'   ) %>%
#'   col_vals_gt(columns = vars(a), value = 0) %>%
#'   interrogate()
#' ```
#' 
#' ```r
#' agent
#' ```
#' 
#' \if{html}{
#' 
#' \out{
#' `r pb_get_image_tag(file = "man_file_tbl_1.png")`
#' }
#' }
#' 
#' This interrogated the data that was obtained from the remote source file,
#' and, there's nothing to clean up (by default, the downloaded file goes into a
#' system temp directory).
#' 
#' ## File access, table creation, and prep via the table store
#' 
#' Using table-prep formulas in a centralized table store can make it easier to
#' work with tables from disparate sources. Here's how to generate a table store
#' with two named entries for table preparations involving the [tbl_store()] and
#' `file_tbl()` functions.
#' 
#' ```r
#' store <-
#'   tbl_store(
#'     small_table_file ~ file_tbl(
#'       file = system.file(
#'         "data_files", "small_table.csv",
#'         package = "pointblank"
#'       ),
#'       col_types = "TDdcddlc"
#'     ),
#'     small_high_file ~ {{ small_table_file }} %>%
#'       dplyr::filter(f == "high")
#'   )
#' ```
#' 
#' Now it's easy to access either of these tables via [tbl_get()]. We can
#' reference the table in the store by its name (given to the left of the `~`).
#' 
#' ```r
#' tbl_get(tbl = "small_table_file", store = store)
#' ```
#' 
#' \preformatted{## # A tibble: 13 × 8
#' ##    date_time           date           a b           c      d e     f    
#' ##    <dttm>              <date>     <dbl> <chr>   <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-…     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-…     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-…     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-…    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-…     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-…     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-…     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-…     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-…     9   838. FALSE high 
#' ## 11 2016-01-26 20:07:00 2016-01-26     4 2-dmx-…     7   834. TRUE  low  
#' ## 12 2016-01-28 02:51:00 2016-01-28     2 7-dmx-…     8   108. FALSE low  
#' ## 13 2016-01-30 11:23:00 2016-01-30     1 3-dka-…    NA  2230. TRUE  high}
#' 
#' 
#' 
#' The second table in the table store is a mutated version of the first. It's
#' just as easily obtainable via [tbl_get()]:
#' 
#' ```r
#' tbl_get(tbl = "small_high_file", store = store)
#' ```
#' 
#' \preformatted{## # A tibble: 6 × 8
#' ##   date_time           date           a b             c     d e     f    
#' ##   <dttm>              <date>     <dbl> <chr>     <dbl> <dbl> <lgl> <chr>
#' ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE  high 
#' ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE  high 
#' ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high 
#' ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high 
#' ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high 
#' ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high}
#' 
#' 
#' 
#' The table-prep formulas in the `store` object could also be used in functions
#' with a `tbl` argument (like [create_agent()] and [create_informant()]). This
#' is accomplished most easily with the [tbl_source()] function.
#' 
#' ```r
#' agent <- 
#'   create_agent(
#'     tbl = ~ tbl_source(
#'       tbl = "small_table_file",
#'       store = store
#'     )
#'   )
#' ```
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ tbl_source(
#'       tbl = "small_high_file",
#'       store = store
#'     )
#'   )
#' ```
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-7
#'
#' @export
file_tbl <- function(
    file,
    type = NULL,
    ...,
    keep = FALSE,
    verify = TRUE
) {
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop(
      "Reading a table from a file requires the readr package:\n",
      " * It can be installed with `install.packages(\"readr\")`.",
      call. = FALSE
    )
  }
  
  file_extension <- tolower(tools::file_ext(file))
  file_name <- basename(file)

  if (grepl("^(ht|f)tps?://", file)) {
    remote_file <- TRUE
  } else {
    remote_file <- FALSE
  }
  
  # If the file is a remote file then download that file
  if (remote_file) {
    
    # If not keeping a downloaded file (option set through the `keep`
    # argument), then make the destination path one that's in a temp dir
    if (!keep) {
      
      temp_dir <- tempdir()
      file_path <- file.path(temp_dir, file_name)
      
    } else {
      file_path <- file_name
    }
    
    download_remote_file(url = file, destfile = file_path)
    
  } else {
    file_path <- file
  }
  
  # Unless specified by `type`, the `file_type` will be determined
  # by the file extension (or inner extension if the file is compressed)
  if (!is.null(type)) {
    
    # Check that the `type` provided is valid for this function
    if (!(tolower(type) %in% c("rda", "rdata", "rds", "csv", "tsv"))) {
      stop(
        "If specifying the file `type`, it must be one of the following:\n",
        " * `rda`, `rds`, `csv`, or `tsv`.",
        call. = FALSE
      )
    }
    
    file_type <- tolower(type)
    
  } else {
    
    if (file_extension %in% c("gz", "bz2", "xz", "zip")) {
      
      file_basename_no_ext <- 
        gsub(
          paste0("\\.(", paste("gz", "bz2", "xz", "zip", sep = "|"), ")"),
          "", basename(file)
        )
      
      secondary_file_ext <- tolower(tools::file_ext(file_basename_no_ext))
      
      if (secondary_file_ext != "") {
        file_type <- secondary_file_ext
      } else {
        stop(
          "File has no secondary extension to indicate the file type:\n",
          " * Use the `type` argument to explicitly state the file type", 
          call. = FALSE
        )
      }
    } else {
      file_type <- file_extension
    }
  }
  
  access_time <- Sys.time()
  
  if (file_type %in% c("rda", "rdata")) {
    
    x <- load_rda_object(file = file_path)
    
  } else if (file_type == "rds") {
    
    x <- readr::read_rds(file = file_path)
    
  } else if (file_type %in% c("csv", "tsv")) {
    
    x <- readr::read_csv(file = file_path, ...)
    
  } else {
    
    stop(
      "The file type is incompatible with `file_tbl()`, the following work:\n",
      " * Comma or tab separated values (`.csv` or `.tsv`)\n",
      " * RDA or RDS files (`.rda`/`.rdata` or `.rds`)",
      call. = FALSE
    )
  }
  
  # If `verify = TRUE` then ensure that the data object inherits
  # from `data.frame`; this can either be a data frame proper or
  # a tibble (`tbl_df`)
  if (verify && !inherits(x, "data.frame")) {
    
    stop(
      "The data object is not a data table:\n",
      " * It is an object of class `", class(x)[1], "`.",
      call. = FALSE
    )
  }
  
  attr(x, "pb_tbl_name") <- file_name
  attr(x, "pb_full_path") <- file
  attr(x, "pb_dir_name") <- dirname(file)
  attr(x, "pb_file_type") <- file_type
  attr(x, "pb_access_time") <- access_time
  
  x
}

#' Specify a file for download from GitHub
#' 
#' The `from_github()` function is helpful for generating a valid URL that
#' points to a data file in a public GitHub repository. This function can be
#' used in the `file` argument of the [file_tbl()] function or anywhere else
#' where GitHub URLs for raw user content are needed.
#' 
#' @param file The name of the file to target in a GitHub repository. This can
#'   be a path leading to and including the file. This is combined with any path
#'   given in `subdir`.
#' @param repo The GitHub repository address in the format
#'   `username/repo[/subdir][@ref|#pull|@*release]`.
#' @param subdir A path string representing a subdirectory in the GitHub
#'   repository. This is combined with any path components included in `file`.
#' @param default_branch The name of the default branch for the repo. This is
#'   usually `"main"` (the default used here).
#'   
#' @return A character vector of length 1 that contains a URL.
#' 
#' @examples
#' # A valid URL to a data file in GitHub can be
#' # obtained from the HEAD of the default branch
#' # from_github(
#' #   file = "inst/data_files/small_table.csv",
#' #   repo = "rich-iannone/pointblank"
#' # )
#' 
#' # The path to the file location can be supplied
#' # fully or partially to `subdir`
#' # from_github(
#' #   file = "small_table.csv",
#' #   repo = "rich-iannone/pointblank",
#' #   subdir = "inst/data_files"
#' # )
#' 
#' # We can use the first call in combination with
#' # `file_tbl()` and `create_agent()`; this
#' # supplies a table-prep formula that gets
#' # a CSV file from the GitHub repository for the
#' # pointblank package 
#' # agent <- 
#' #   create_agent(
#' #     tbl = ~ file_tbl(
#' #       file = from_github(
#' #         file = "inst/data_files/small_table.csv",
#' #         repo = "rich-iannone/pointblank"
#' #       ),
#' #       col_types = "TDdcddlc"
#' #     )
#' #   ) %>%
#' #   col_vals_gt(vars(a), 0) %>%
#' #   interrogate()
#' 
#' # The `from_github()` helper function is
#' # pretty powerful and can get at lots of
#' # different files in a repository
#' 
#' # A data file from GitHub can be obtained from
#' # a commit at release time
#' # from_github(
#' #   file = "inst/extdata/small_table.csv",
#' #   repo = "rich-iannone/pointblank@v0.2.1"
#' # )
#' 
#' # A file may also be obtained from a repo at the
#' # point in time of a specific commit (partial or
#' # full SHA-1 hash for the commit can be used)
#' # from_github(
#' #   file = "data-raw/small_table.csv",
#' #   repo = "rich-iannone/pointblank@e04a71"
#' # )
#' 
#' # A file may also be obtained from an
#' # *open* pull request
#' # from_github(
#' #   file = "data-raw/small_table.csv",
#' #   repo = "rich-iannone/pointblank#248"
#' # )
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-6
#' 
#' @export
from_github <- function(
    file,
    repo,
    subdir = NULL,
    default_branch = "main"
) {
  
  # get the username, repo, subdir component
  u_r_s <- gsub("(@|#).*", "", repo)
  u_r_s <- unlist(strsplit(u_r_s, "/"))
  
  # Get the username and repo
  username <- u_r_s[1]
  repository <- u_r_s[2]
  
  # Get the package subdir if length of `u_r_s` is 3
  if (length(u_r_s) == 3) {
    subdir_file <- u_r_s[3]
  } else {
    subdir_file <- NULL
  }

  if (grepl("@*", repo, fixed = TRUE)) {

    ref_res <- unlist(strsplit(repo, "@\\*"))[2]
    
  } else if (grepl("@", repo, fixed = TRUE)) {
    
    ref_res <- unlist(strsplit(repo, "@"))[2]
    
  } else if (grepl("#", repo, fixed = TRUE)) {
    
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop(
        "Getting a table from a file in a PR requires the jsonlite package:\n",
        " * It can be installed with `install.packages(\"jsonlite\")`.",
        call. = FALSE
      )
    }

    pr_number <- unlist(strsplit(repo, "#"))[2]
    pulls_doc_tempfile <- tempfile(pattern = "pulls", fileext = ".json")
    
    pulls_doc_url <-
      as.character(
        glue::glue(
          "https://api.github.com/repos/{username}/{repository}/pulls"
        )
      )
    
    # Download the `pulls` JSON document for the repo from GitHub 
    download_remote_file(
      url = pulls_doc_url,
      destfile = pulls_doc_tempfile,
      quiet = TRUE
    )
    
    # Resolve the PR number to a merge commit SHA
    ref_res <-
      (jsonlite::fromJSON(pulls_doc_tempfile, flatten = TRUE) %>%
         dplyr::select(number, merge_commit_sha, head.ref) %>%
         dplyr::filter(number == as.integer(pr_number)) %>%
         dplyr::pull(merge_commit_sha))[1]
    
  } else {
    ref_res <- default_branch
  }
  
  if (!is.null(subdir_file)) {
    file_path <- as.character(glue::glue("{subdir_file}/{file}"))
  } else if (!is.null(subdir)) {
    file_path <- as.character(glue::glue("{subdir}/{file}"))
  } else {
    file_path <- file
  }
  
  url <-
    as.character(
      glue::glue(
        "https://github.com/{username}/{repository}/raw/{ref_res}/{file_path}"
      )
    )
  
  url
}

download_remote_file <- function(url, ...) {
  
  if (grepl("^https?://", url)) {
    
    is_r32 <- getRversion() >= "3.2"
    
    if (.Platform$OS.type == "windows") {
      
      if (is_r32) {
        
        method <- "wininet"
        
      } else {
        
        seti2 <- utils::"setInternet2"
        internet2_start <- seti2(NA)
        
        if (!internet2_start) {
          
          on.exit(suppressWarnings(seti2(internet2_start)))
          suppressWarnings(seti2(TRUE))
        }
        
        method <- "internal"
      }
      
      suppressWarnings(utils::download.file(url, method = method, ...))
      
    } else {
      
      if (is_r32 && capabilities("libcurl")) {
        
        method <- "libcurl"
        
      } else if (nzchar(Sys.which("wget")[1])) {
        
        method <- "wget"
        
      } else if (nzchar(Sys.which("curl")[1])) {
        
        method <- "curl"
        orig_extra_options <- getOption("download.file.extra")
        
        on.exit(options(download.file.extra = orig_extra_options))
        options(download.file.extra = paste("-L", orig_extra_options))
        
      } else if (nzchar(Sys.which("lynx")[1])) {
        
        method <- "lynx"
        
      } else {
        stop("No download method can be found.")
      }
      
      utils::download.file(url, method = method, ...)
    }
    
  } else {
    utils::download.file(url, ...)
  }
}

load_rda_object <- function(file) {
  
  env <- new.env()
  nm <- load(file, env)[1]
  env[[nm]]
}

get_attr_file_tbl <- function(x, attr) {

  # Possible `attr` values are:
  # * "tbl_name"
  # * "full_path"
  # * "dir_name"
  # * "file_type"
  
  attr(x, which = paste0("pb_", attr), exact = TRUE)
}

# nocov end
