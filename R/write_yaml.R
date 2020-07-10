#' @export
agent_yaml_write <- function(agent,
                             file) {
  
  as_agent_yaml_list(agent) %>%
    yaml::write_yaml(
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

#' @export
agent_yaml_string <- function(agent = NULL,
                              path = NULL) {
  
  if (is.null(agent) && is.null(path)) {
    stop(
      "An `agent` object or a `path` to a YAML file must be specified.",
      call. = FALSE
    )
  }
  
  if (!is.null(agent) && !is.null(path)) {
    stop("Only one of `agent` or `path` should be specified.", call. = FALSE)
  }
  
  if (!is.null(agent)) {
    
    as_agent_yaml_list(agent) %>%
      yaml::as.yaml(
        handlers = list(
          logical = function(x) {
            result <- ifelse(x, "true", "false")
            class(result) <- "verbatim"
            result
          }
        )
      ) %>% cat()
    
  } else {
    readLines(path) %>% paste(collapse = "\n") %>% cat()
  }
}

as_vars_fn <- function(columns) {
  paste0("vars(", columns,")")
}

as_list_preconditions <- function(preconditions) {
  if (is.null(preconditions[[1]])) {
    return(NULL)
  } else {
    return(as.character(preconditions))
  }
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

get_arg_value_lr <- function(value) {

  if (inherits(value, "quosure")) {
    out <- paste0("vars(", rlang::as_label(value),")")
  } else if (inherits(value, "numeric")) {
    out <- value
  } else {
    out <- as.character(value)
  }
  
  out
}

as_agent_yaml_list <- function(agent) {

  if (is.null(agent$read_fn)) {
    stop(
      "The agent must have a `read_fn` value to transform it into YAML.",
       call. = FALSE
    )
  }
  
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
      "col_vals_lt", "col_vals_lte",
      "col_vals_equal", "col_vals_not_equal",
      "col_vals_gte", "col_vals_gt"
    )) {

      lst_step <- 
        list(
          validation_fn = list(
            columns = as_vars_fn(step_list$column[[1]]),
            value = get_arg_value(step_list$values),
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (validation_fn %in% c("col_vals_between", "col_vals_not_between")) {

      lst_step <- 
        list(
          validation_fn = list(
            columns = as_vars_fn(step_list$column[[1]]),
            left = get_arg_value_lr(step_list$values[[1]][[1]]),
            right = get_arg_value_lr(step_list$values[[1]][[2]]),
            inclusive = as.logical(c(names(step_list$values[[1]][1]), names(step_list$values[[1]][1]))),
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (validation_fn %in% c("col_vals_in_set", "col_vals_not_in_set")) {
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = as_vars_fn(step_list$column[[1]]),
            set = step_list$values[[1]],
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn

    } else if (validation_fn %in% c("col_vals_null", "col_vals_not_null")) {
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = as_vars_fn(step_list$column[[1]]),
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (validation_fn == "col_vals_regex") {

      lst_step <- 
        list(
          validation_fn = list(
            columns = as_vars_fn(step_list$column[[1]]),
            regex = get_arg_value(step_list$values),
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (grepl("col_is_", validation_fn) || validation_fn == "col_exists") {

      lst_step <- 
        list(
          validation_fn = list(
            columns = as_vars_fn(step_list$column[[1]]),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (validation_fn == "col_vals_expr") {

      lst_step <- 
        list(
          validation_fn = list(
            expr = paste0("~", rlang::as_label(step_list$values[[1]])),
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (validation_fn == "rows_distinct") {

      if (is.na(step_list$column[[1]][[1]])) {
        vars_cols <- NULL
      } else {
        vars_cols <- as_vars_fn(step_list$column[[1]])
      }
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = vars_cols,
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
    
    } else if (validation_fn == "col_schema_match") {
      
      vals <- step_list$values[[1]]
      length_vals <- length(vals) - 2
      
      vals_complete <- vals$`__complete__`
      vals_complete <- vals$`__complete__`
      
      vals_columns <- vals[seq_len(length_vals)]
      
      schema_fn <- 
        vapply(
          seq_along(vals_columns),
          FUN.VALUE = character(1), 
          FUN = function(x) {
            
            arg_name <- names(vals_columns[x])
            values <- paste0("\"", vals_columns[[x]], "\"")
            
            if (length(values) > 1) {
              values <- paste0("c(", paste(values, collapse = ", "), ")")
            }

            paste("   ", arg_name, "=", values)
          }
        ) %>% 
        paste(collapse = ",\n") %>%
        paste0("col_schema(\n", ., "\n)")
      
      lst_step <- 
        list(
          validation_fn = list(
            schema = schema_fn,
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
      
    } else if (validation_fn == "conjointly") {
      
      lst_step <- 
        list(
          validation_fn = list(
            fns = as.character(step_list$values[[1]]),
            preconditions = as_list_preconditions(step_list$preconditions),
            active = step_list$active
          )
        )
      names(lst_step) <- validation_fn
    }
    
    all_steps <- c(all_steps, list(lst_step))
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
