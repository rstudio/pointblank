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
#' `interrogate()`, did every single validation step result in zero *failing*
#' test units? Using the `all_passed()` function will let us know whether that's
#' `TRUE` or not.
#' 
#' @details
#' The `all_passed()` function provides a single logical value based on an
#' interrogation performed in the *agent*-based workflow. For very large-scale
#' validation (where data quality is a known issue, and is perhaps something to
#' be tamed over time) this function is likely to be less useful since it is
#' quite stringent (all test units must pass across all validation steps).
#' 
#' Should there be a requirement for logical values produced from validation, a
#' more flexible alternative is in using the test (`test_*()`) variants of the
#' validation functions. Each of those produce a single logical value and each
#' and have a `threshold` option for failure levels. Another option is to
#' utilize post-interrogation objects within the *agent*'s x-list (obtained by
#' using the [get_agent_x_list()] function). This allows for many possibilities
#' in producing a single logical value from an interrogation.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param i A vector of validation step numbers. These values are assigned to
#'   each validation step by **pointblank** in the order of definition. If
#'   `NULL` (the default), all validation steps will be used for the evaluation
#'   of complete *passing*.
#'
#' @return A logical value.
#' 
#' @section Demos:
#' 
#' Create a simple table with a column of numerical values.
#' 
#' ```{r}
#' tbl <- dplyr::tibble(a = c(4, 5, 7, 8))
#' 
#' tbl
#' ```
#' 
#' Validate that values in column `a` are always greater than 4.
#' 
#' ```{r}
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), value = 3) %>%
#'   col_vals_lte(vars(a), value = 10) %>%
#'   col_vals_increasing(vars(a)) %>%
#'   interrogate()
#' ```
#' 
#' Determine if these column validations have all passed by using `all_passed()`
#' (they do).
#' 
#' ```{r}
#' all_passed(agent = agent)
#' ```
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 8-4
#' 
#' @export
all_passed <- function(
    agent,
    i = NULL
) {
  
  # Stop function if the agent did not interrogate the target data
  if (!has_agent_intel(agent)) {
    
    stop(
      "The agent hasn't performed an interrogation.",
      call. = FALSE
    )
  }
  
  # Obtain the vector of logical values in the `all_passed`
  # column of the agent's `validation_set` tibble
  all_passed_vec <- agent$validation_set$all_passed
  
  # In the rare instance that an interrogation was performed
  # with an agent having no validation steps, the return value
  # should be NA
  if (length(all_passed_vec) < 1) {
    
    if (is.null(i)) {
      
      return(NA)
      
    } else {
      
      # Stop function if any value is provided for `i`
      stop(
        "You cannot provide a value for `i` when the agent ",
        "contains no validation steps.",
        call. = FALSE
      )
    }
  }
  
  # If a vector is provided to `i` then subset the `all_passed_vec`
  # by that vector
  if (!is.null(i)) {
    
    all_passed_vec_range <- seq_along(all_passed_vec)
    
    if (!all(i %in% all_passed_vec_range)) {
      
      stop(
        "All values provided for `i` must be in the range of ",
        "the validation step numbers present in the agent.",
        call. = FALSE
      )
    }
    
    all_passed_vec <- all_passed_vec[i]
  }
  
  # Any NA values in `all_passed` would indicate an evaluation
  # failure during interrogation; so, any NAs should result
  # in the return value being FALSE
  if (any(is.na(all_passed_vec))) {
    return(FALSE)
  }
  
  all(all_passed_vec)
}
