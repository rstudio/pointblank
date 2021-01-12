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
#' @description If your target table is in a file, stored either locally or
#'   remotely, the `file_tbl()` function can make it possible to access it in a
#'   single function call. Compatible file types for this function are: CSV
#'   (`.csv`), TSV (`.tsv`), RDA (`.rda`), and RDS (`.rds`) files. This function
#'   generates an in-memory `tbl_dbl` object, which can be used as a target
#'   table for [create_agent()] and [create_informant()]. The ideal option for
#'   data access with `file_tbl()` is using this function as the `read_fn`
#'   parameter in either of the aforementioned `create_*()` functions. This can
#'   be done by using a leading `~` (e.g,. `read_fn = ~file_tbl(...)`).
#'
#'   In the remote data use case, we can specify a URL starting with `http://`
#'   or `https://` and ending with the file containing the data table. If data
#'   files are available in a GitHub repository then we can use the
#'   [from_github()] function to specify the name and location of the table data
#'   for a repository.
#' 
#' @param file The complete file path leading to a compatible data table either
#'   in the user system or at a `http://` or `https://` URL. For a file hosted
#'   in a GitHub repository, a call to the [from_github()] function can be used
#'   here.
#' @param type The file type. This is normally inferred by file extension and is
#'   by default `NULL` to indicate that the extension will dictate the type of
#'   file reading that is performed internally. However, if there is no
#'   extension (and valid extensions are `.csv`, `.tsv`, `.rda`, and `.rds`), we
#'   can provide the type as either of `csv`, `tsv`, `rda`, or `rds`.
#' @param ... Options passed to **readr**'s `read_csv()` or `read_tsv()`
#'   function.
#' @param keep In the case of a downloaded file, should it be stored in the
#'   working directory (`keep = TRUE`) or should it be downloaded to a temporary
#'   directory? By default, this is `FALSE`.
#'   
#' @return A `tbl_df` object.
#'
#' @family Planning and Prep
#' @section Function ID:
#' 1-7
#'
#' @export
file_tbl <- function(file,
                     type = NULL,
                     ...,
                     keep = FALSE) {
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop(
      "Reading a table from a file requires the readr package:\n",
      " * It can be installed with `install.packages(\"readr\")`.",
      call. = FALSE
    )
  }
  
  file_extension <- tolower(tools::file_ext(file))
  file_name <- basename(file)

  if (grepl("^https?://", file)) {
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
          " * Use the `type` argument to explicitly state type of file this is", 
          call. = FALSE
        )
      }
    } else {
      file_type <- file_extension
    }
  }
  
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
#'   
#' @return A character vector of length 1 that contains a URL.
#' 
#' @family Utility and Helper Functions
#' @section Function ID:
#' 12-6
#' 
#' @export
from_github <- function(file,
                        repo,
                        subdir = NULL) {
  
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
    ref_res <- "master"
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

download_remote_file <- function(url,
                                 ...) {
  
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

# nocov end
