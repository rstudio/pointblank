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


#' Did all of the validations fully *pass*?
#'
#' @description
#' Given an agent's validation plan that had undergone interrogation via
#' `interrogate()`, did every single validation step result in zero *fail*
#' levels? Using the `all_passed()` function will let us know whether that's
#' `TRUE` or not.
#'
#' @param agent An agent object of class `ptblank_agent`.
#'
#' @return A logical value.
#' 
#' @examples
#' # Create a simple table with
#' # a column of numerical values
#' tbl <- 
#'   dplyr::tibble(a = c(5, 7, 8, 5))
#' 
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), value = 4) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent = agent)
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-4
#' 
#' @export
all_passed <- function(agent) {
  
  if (!has_agent_intel(agent)) {
    stop("The agent hasn't performed an interrogation.", call. = FALSE)
  }
  
  all(agent$validation_set$all_passed)
}
