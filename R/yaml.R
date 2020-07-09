#
# YAML creation and writing
#

to_list_columns <- function(columns) {
  list(columns = paste0("vars(", columns,")"))
}

to_list_value <- function(value) {
  list(value = get_arg_value(value))
}

to_list_na_pass <- function(na_pass) {
  list(na_pass = na_pass)
}

to_list_preconditions <- function(preconditions) {
  if (is.null(preconditions[[1]])) {
    return(NULL)
  } else {
    return(list(preconditions = as.character(preconditions)))
  }
}

to_list_active <- function(active) {
  list(active = active)
}

to_list_step <-  function(validation_fn, ...) {
  list_step <- list(validation_fn = list(...))
  names(list_step) <- validation_fn
  for (i in rev(seq_len(length(list_step[[1]])))) {
    if (is.null(list_step[[1]][[i]][[1]])) {
      list_step[[1]][[i]][[1]] <- NULL
    }
  }
  list_step
}

to_list_action_levels <- function(actions) {
  
  agent_actions <- agent$actions
  agent_actions[sapply(agent_actions, is.null)] <- NULL
  agent_actions$fns[sapply(agent_actions$fns, is.null)] <- NULL
  
  if (length(agent_actions$fns) == 0) agent_actions$fns <- NULL
  
  list(action_levels = agent_actions)
}

to_list_read_fn <- function(read_fn) {
  list(read_fn = read_fn %>% rlang::as_label())
}

to_list_name <- function(name) {
  list(name = name)
}

get_arg_value <- function(value) {
  
  if (inherits(value, "list") && inherits(value[[1]], "quosures")) {
    out <- paste0("vars(", rlang::as_label(value[[1]][[1]]),")")
  } else if (inherits(value, "list") && inherits(value[[1]], "numeric")) {
    out <- value[[1]] 
  } else {
    out <- as.character(value[[1]])
  }
  
  out
}

as_agent_yaml_list <- function(agent) {
  
  lst_name <- to_list_name(agent$name)
  lst_read_fn <- to_list_read_fn(agent$read_fn)
  lst_action_levels <- to_list_action_levels(agent$actions)
  
  # TODO: write out `end_fns`
  
  lst_embed_report <- list(embed_report = agent$embed_report)
  lst_reporting_lang <- list(reporting_lang = agent$reporting_lang)
  
  agent_validation_set <- 
    agent$validation_set %>% 
    dplyr::select(
      assertion_type, column, values, na_pass,
      preconditions, actions, brief, active
    )
  
  all_steps <- list()
  
  for (i in seq_len(nrow(agent_validation_set))) {
    
    step_list <- agent_validation_set[i, ] %>% as.list()
    
    validation_fn <- step_list$assertion_type
    
    if (validation_fn %in% c(
      "col_vals_lt",
      "col_vals_lte",
      "col_vals_equal",
      "col_vals_not_equal",
      "col_vals_gte",
      "col_vals_gt"
    )) {
      lst_columns <- to_list_columns(step_list$column[[1]])
      lst_value <- to_list_value(step_list$values)
      lst_na_pass <- to_list_na_pass(step_list$na_pass)
      lst_preconditions <- to_list_preconditions(step_list$preconditions)
      lst_active <- to_list_active(step_list$active)
      lst_step <- to_list_step(validation_fn, lst_columns, lst_value, lst_na_pass,
                               lst_preconditions, lst_active)
      
      all_steps <- c(all_steps, lst_step)
    }
  }
  
  c(
    lst_name,
    lst_read_fn,
    lst_action_levels,
    lst_embed_report,
    lst_reporting_lang,
    list(steps = all_steps)
  )
}

as_agent_yaml_string <- function(agent) {
  
  as_agent_yaml_list(agent) %>%
  yaml::as.yaml(
    handlers = list(
      logical = function(x) {
        result <- ifelse(x, "true", "false")
        class(result) <- "verbatim"
        return(result)
      })
  )
}

write_agent_yaml <- function(agent,
                             file) {
  
  as_agent_yaml_list(agent) %>%
    write_yaml(
      file = file,
      handlers = list(
        logical = function(x) {
          result <- ifelse(x, "true", "false")
          class(result) <- "verbatim"
          return(result)
        }
      )
    )
}

#
# YAML reading and execution
#

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
  
  lapply(seq_along(steps), FUN = function(x) { 
    step_i <- steps[x]
    step_fn <- names(step_i)
    
    vapply(
      step_i[[1]],
      FUN.VALUE = character(1), 
      FUN = function(x) {
        paste(" ", names(x), "=", as.character(x[[1]]))
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

read_agent_yaml <- function(file) {
  yaml::read_yaml(file = file)
}

execute_agent_yaml <- function(file) {
  
  y <- read_agent_yaml(file = file)
  
  rlang::eval_tidy(
    rlang::parse_expr(
      glue::glue(
        "
        create_agent(
        read_fn = {y$read_fn},
        name = \"{y$name}\",
        actions = {make_action_levels_str(y$action_levels)},
        embed_report = {y$embed_report},
        reporting_lang = \"{y$reporting_lang}\"
        ) {make_validation_steps(y$steps)}","%>%\ninterrogate()"
      )
    )
  )
}
