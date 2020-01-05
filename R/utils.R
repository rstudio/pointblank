is_ptblank_agent <- function(x) {
  inherits(x, "ptblank_agent")
}

is_a_table_object <- function(x) {
  inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))
}

get_tbl_object <- function(agent) {
  agent$tbl
}

has_agent_intel <- function(agent) {
  inherits(agent, "has_intel")
}

is_agent_empty <- function(agent) {
  if (!is_ptblank_agent(agent)) return(FALSE)
  if (nrow(agent$validation_set) == 0) TRUE else FALSE
}

interrogation_time <- function(agent) {
  if (has_agent_intel(agent)) agent$time else NA
}

number_of_validation_steps <- function(agent) {
  if (is_ptblank_agent(agent)) agent$validation_set %>% nrow() else NA
}

get_assertion_type_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "assertion_type"]]
}

get_column_as_sym_at_idx <- function(agent, idx) {
  rlang::sym(agent$validation_set[[idx, "column"]] %>% gsub("'", "", .))
}

get_column_value_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "value"]]
}

get_column_set_values_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "set"]]
}

get_column_na_pass_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "na_pass"]]
}

get_column_regex_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "regex"]]
}

get_all_cols <- function(agent) {
  agent$col_names
}

resolve_expr_to_cols <- function(tbl, var_expr) {
  
  var_expr <- enquo(var_expr)
  
  if ((var_expr %>% rlang::get_expr() %>% as.character())[1] == "vars") {
    
    cols <- (var_expr %>% rlang::get_expr() %>% as.character())[-1]
    return(cols)
  }
  
  tidyselect::vars_select(.vars = colnames(tbl), {{ var_expr }}) %>% unname()
}

resolve_columns <- function(x, var_expr, preconditions) {
  
  # Return an empty character vector if the expr is NULL
  if (inherits(var_expr, "quosure") && var_expr %>% rlang::as_label() == "NULL") {
    return(character(0))
  } 
  
  # Get the column names
  if (is.null(preconditions)) {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      column <- resolve_expr_to_cols(tbl = x, var_expr = !!var_expr)
      column <- column[1]
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
    
  } else {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      tbl <- x
      
      tbl <- 
        preconditions %>%
        rlang::f_rhs() %>%
        rlang::eval_tidy()
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
      column <- column[1]
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      
      tbl <- 
        preconditions %>%
        rlang::f_rhs() %>%
        rlang::eval_tidy()
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
  }
  
  column
}

tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {
  
  gsub(pattern, replacement, x, fixed = fixed)
}
