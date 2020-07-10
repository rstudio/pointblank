#' @export
agent_yaml_read <- function(file) {
  expr_from_agent_yaml(file = file, interrogate = FALSE) %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
}

#' @export
agent_yaml_interrogate <- function(file) {
  expr_from_agent_yaml(file = file, interrogate = TRUE) %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
}

expr_from_agent_yaml <- function(file,
                                 interrogate = FALSE) {
  
  y <- yaml::read_yaml(file = file)
  
  expr_str <- 
    glue::glue(
      "
      create_agent(
      read_fn = {y$read_fn},
      name = \"{y$name}\",
      actions = {make_action_levels_str(y$action_levels)},
      embed_report = {y$embed_report},
      reporting_lang = \"{y$reporting_lang}\"
      ) {make_validation_steps(y$steps)}"
    ) %>% as.character()
  
  if (interrogate) {
    expr_str <- paste0(expr_str, "%>%\ninterrogate()")
  }
  expr_str
}

make_action_levels_str <- function(al) {
  
  if (is.null(al)) {
    return("NULL")
  }
  
  warn_at <- c(al$warn_fraction, al$warn_count) %||% "NULL"
  stop_at <- c(al$stop_fraction, al$stop_count) %||% "NULL"
  notify_at <- c(al$notify_fraction, al$notify_count) %||% "NULL"
  
  # TODO: incorporate fns component
  
  glue::glue(
    "action_levels(warn_at = {warn_at}, stop_at = {stop_at}, notify_at = {notify_at})"
  )
}

make_validation_steps <- function(steps) {
  
  if (length(steps) == 0) return("")
  
  lapply(
    seq_along(steps),
    FUN = function(x) { 
      
      step_i <- steps[[x]]
      step_fn <- names(step_i)
      
      vapply(
        seq_along(step_i[[1]]),
        FUN.VALUE = character(1), 
        FUN = function(x) {
          
          arg_name <- names(step_i[[1]][x])
          val <- step_i[[1]][[x]]
          
          if (arg_name == "fns") {
            return(paste("  ", val, collapse = ",\n"))
          }
          
          if (is.character(val) &&
              !grepl("^vars\\(.*?\\)$", val) &&
              !(arg_name %in% c("preconditions", "expr", "schema"))) {
            val <- paste0("\"", val, "\"")
          }
          
          if (length(val) > 1) {
            val <- paste0("c(", paste(as.character(val), collapse = ", "), ")")
          } else {
            val <- as.character(val)
          }
          
          paste(" ", arg_name, "=", val)
        }
      ) %>% 
        paste(collapse = ",\n") %>%
        paste0("%>%\n", step_fn, "(\n", ., "\n)")
    }
  ) %>% 
    unlist() %>%
    paste(collapse = " ") %>%
    paste0(., " ")
}
