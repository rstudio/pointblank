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


#' Create a **pointblank** *multiagent* object
#'
#' @param ... One or more **pointblank** agent objects.
#' @param lang The language to use for any reporting that will be generated from
#'   the *multiagent*. (e.g., individual *agent reports*, *multiagent reports*,
#'   etc.). By default, `NULL` will create English (`"en"`) text. Other options
#'   include French (`"fr"`), German (`"de"`), Italian (`"it"`), Spanish
#'   (`"es"`), Portuguese, (`"pt"`), Chinese (`"zh"`), and Russian (`"ru"`).
#' @param locale An optional locale ID to use for formatting values in the
#'   reporting outputs according the locale's rules. Examples include `"en_US"`
#'   for English (United States) and `"fr_FR"` for French (France); more simply,
#'   this can be a language identifier without a country designation, like "es"
#'   for Spanish (Spain, same as `"es_ES"`).
#'   
#' @return A `ptblank_multiagent` object.
#'
#' @export
create_multiagent <- function(...,
                              lang = NULL,
                              locale = NULL) {
  
  agent_list <- list(...)
    
  agent_list <- 
    lapply(
      agent_list,
      FUN = function(agent) {
        
        # TODO: Ensure that the `agent` is actually an agent
        
        class(agent) <-
          c(setdiff(class(agent), "ptblank_agent"), "ptblank_agent_i")
        
        agent
      }
    )
  
  agent_series <-
    list(
      overview_tbl = list(),
      agents = agent_list
    )
  
  class(agent_series) <- "ptblank_multiagent"
  agent_series
}

