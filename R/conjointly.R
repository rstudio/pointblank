#' Perform multiple rowwise validations for joint validity
#'
#' @inheritParams col_vals_gt
#' @param ... a collection one-sided formulas that consist of validation step
#' functions that validate row units. Specifically, these functions should be
#' those with the naming pattern `col_vals_*()` (e.g,
#' `~ col_vals_gte(., columns = vars(a), value = 5.5)`).
#' @param .list Allows for the use of a list as an input alternative to `...`.
#'
#' @examples
#' # Create a simple data frame with
#' # three columns of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(3, 4, 6, 8, 9, 11),
#'     c = c(2, 6, 8, NA, 3, 8)
#'   )
#'
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = df) %>%
#'   conjointly(
#'     ~ col_vals_gt(., columns = vars(a), value = 6),
#'     ~ col_vals_lt(., columns = vars(b), value = 10),
#'     ~ col_vals_not_null(., columns = vars(c))
#'     ) %>%
#'   interrogate()
#'
#' @import rlang
#' @export
conjointly <- function(x,
                       ...,
                       .list = list2(...),
                       preconditions = NULL,
                       brief = NULL,
                       actions = NULL) {

  # Obtain all of the group's elements
  list_elements <- .list
  
  dots_attrs <- list_elements[rlang::names2(list_elements) != ""]
  
  validation_formulas <-
    list_elements[
      vapply(
        list_elements,
        function(x) rlang::is_formula(x),
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]
  
  agent <- x
  
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "conjointly",
      column = NULL,
      value = NULL,
      set = NULL,
      regex = NULL,
      incl_na = NULL,
      preconditions = preconditions,
      actions = actions,
      brief = brief
    )
  
  # Get the current step
  current_step <- (agent$validation_set %>% nrow())
  
  for (formula in validation_formulas) {
    
    agent <-
      eval(
        expr = parse(
          text =
            formula %>%
            rlang::f_rhs() %>%
            rlang::expr_deparse() %>%
            tidy_gsub("(.", "(agent", fixed = TRUE)
        ),
        envir = NULL
      )
  }
  
  joint_rows <- (current_step + 1):(agent$validation_set %>% nrow())
  
  agent$validation_set[joint_rows, "i"] <- current_step
  agent$validation_set[current_step, "j"] <- NA_integer_
  # agent$validation_set[joint_rows, "preconditions"] <- preconditions
  # agent$validation_set[joint_rows, "actions"] <- list(actions)
  agent$validation_set[joint_rows, "j"] <- seq(joint_rows)
  
  agent
}  
  