#------------------------------------------------------------------------------#
# 
#                 _         _    _      _                _    
#                (_)       | |  | |    | |              | |   
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |                                                        
#  |_|                                                        
#  
#  This file is part of the 'rstudio/pointblank' project.
#  
#  Copyright (c) 2017-2024 pointblank authors
#  
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
# 
#------------------------------------------------------------------------------#


#' Read **pointblank** *agents* stored on disk as a *multiagent* 
#'
#' @description
#' 
#' An *agent* or *informant* can be written to disk with the [x_write_disk()]
#' function. While useful for later retrieving the stored agent with
#' [x_read_disk()] it's also possible to read a series of on-disk agents with
#' the `read_disk_multiagent()` function, which creates a `ptblank_multiagent`
#' object. A *multiagent* object can also be generated via the
#' [create_multiagent()] function but is less convenient to use if one is just
#' using agents that have been previously written to disk.
#'
#' @param filenames *File names*
#' 
#'   `vector<character>` // *default:* `NULL` (`optional`)
#' 
#'   The names of files (holding *agent* objects) that were previously written
#'   by [x_write_disk()].
#'   
#' @param pattern *Regex pattern*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   A regex pattern for accessing saved-to-disk *agent* files located in a
#'   directory (specified in the `path` argument).
#'   
#' @param path *File path*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   A path to a collection of files. This is either optional in the case that
#'   files are specified in `filenames` (the `path` combined with all
#'   `filenames`), or, required when providing a `pattern` for file names.
#'   
#' @return A `ptblank_multiagent` object.
#' 
#' @family The multiagent
#' @section Function ID:
#' 10-2
#'
#' @export
read_disk_multiagent <- function(
    filenames = NULL,
    pattern = NULL,
    path = NULL
) {
  
  if (!is.null(pattern)) {
    
    if (is.null(path)) {
      path <- "."
    }
    
    file_list <- list.files(path = path, pattern = pattern)
    
    if (length(file_list) < 1) {
      
      stop("No files.", call. = FALSE)
    }
    
  } else if (!is.null(filenames)) {
    
    # TODO: check that `filenames` is a character vector
    # TODO: check that `filenames` isn't zero length
    
    file_list <- filenames
    
    # TODO: verify that the `filenames` point to actual files on disk
  }
  
  agent_list <-
    lapply(
      file_list,
      FUN = function(x) {

        agent <- x_read_disk(filename = x, path = path)
        
        # TODO: Ensure that the `agent` is actually an agent
        
        class(agent) <-
          c(setdiff(class(agent), "ptblank_agent"), "ptblank_agent_i")
        
        agent
      }
    )
  
  # TODO: Verify that there are agents in here, stop if there isn't at
  # least one
  
  # TODO: Sort the agents in `agent_list` by their `time_end` timestamps
  
  agent_series <-
    list(
      overview_tbl = list(),
      agents = agent_list
    )
  
  class(agent_series) <- "ptblank_multiagent"
  agent_series
}
