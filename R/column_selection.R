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
      
      tbl <- get_focal_tbl_object(agent = x)
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

      tbl <- get_focal_tbl_object(agent = x)
      
      tbl <- 
        preconditions %>%
        rlang::f_rhs() %>%
        rlang::eval_tidy()
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
  }

 column
}
