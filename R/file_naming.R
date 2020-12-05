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

#' Append a date string to a file name
#' @export
affix_date <- function(filename,
                       position = c("end", "start"),
                       format = "%Y-%m-%d",
                       delimiter = "_",
                       utc = TRUE) {
  
  position <- match.arg(position)
  
  affix_time_to_filename(
    filename = filename,
    position = position,
    format = format,
    delimiter = delimiter,
    utc = utc
  )
}

#' Append a datetime string to a file name
#' @export
affix_datetime <- function(filename,
                           position = c("end", "start"),
                           format = "%Y-%m-%dT%H:%M:%S",
                           delimiter = "_",
                           utc = TRUE) {
  
  position <- match.arg(position)
  
  affix_time_to_filename(
    filename = filename,
    position = position,
    format = format,
    delimiter = delimiter,
    utc = utc
  )
}

affix_time_to_filename <- function(filename,
                                   position,
                                   format,
                                   delimiter,
                                   utc) {
  
  curr_time <- Sys.time()
  
  if (utc) {
    attr(curr_time, "tzone") <- "UTC"
  }
  
  curr_time <- format(curr_time, format)
  
  if (position == "end") {
    
    filename_rev <-
      paste0(
        tools::file_path_sans_ext(filename),
        delimiter,
        curr_time,
        ".",
        tools::file_ext(filename)
      )
    
  } else {
    
    filename_rev <-
      paste0(
        curr_time,
        delimiter,
        filename
      )
  }
  
  filename_rev
}
