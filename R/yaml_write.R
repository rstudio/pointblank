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


#' Write **pointblank** objects to YAML files
#' 
#' @description
#' 
#' With `yaml_write()` we can take different **pointblank** objects (these are
#' the `ptblank_agent`, `ptblank_informant`, and `tbl_store`) and write them to
#' YAML. With an *agent*, for example, `yaml_write()` will write that everything
#' that is needed to specify an *agent* and it's validation plan to a YAML file.
#' With YAML, we can modify the YAML markup if so desired, or, use as is to
#' create a new agent with the [yaml_read_agent()] function. That *agent* will
#' have a validation plan and is ready to [interrogate()] the data. We can go a
#' step further and perform an interrogation directly from the YAML file with
#' the [yaml_agent_interrogate()] function. That returns an agent with intel
#' (having already interrogated the target data table). An *informant* object
#' can also be written to YAML with `yaml_write()`.
#'
#' One requirement for writing an *agent* or an *informant* to YAML is that we
#' need to have a table-prep formula specified (it's an R formula that is used
#' to read the target table when [interrogate()] or [incorporate()] is called).
#' This option can be set when using [create_agent()]/[create_informant()] or
#' with [set_tbl()] (useful with an existing agent or informant object).
#' 
#' @param ... *Pointblank agents, informants, table stores*
#' 
#'   `<series of obj:<ptblank_agent|ptblank_informant|tbl_store>>`
#'   // **required**
#' 
#'   Any mix of **pointblank** objects such as the *agent*
#'   (`ptblank_agent`), the *informant* (`ptblank_informant`), or the table
#'   store (`tbl_store`). The agent and informant can be combined into a single
#'   YAML file (so long as both objects refer to the same table). A table store
#'   cannot be combined with either an agent or an informant so it must undergo
#'   conversion alone.
#'   
#' @param .list *Alternative to `...`*
#' 
#'   `<list of multiple expressions>` // **required** (or, use `...`)
#' 
#'   Allows for the use of a list as an input alternative to `...`.
#' 
#' @param filename *File name*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   The name of the YAML file to create on disk. It is recommended that either
#'   the `.yaml` or `.yml` extension be used for this file. If not provided then
#'   default names will be used (`"tbl_store.yml"`) for a table store and the
#'   other objects will get default naming to the effect of
#'   `"<object>-<tbl_name>.yml"`.
#' 
#' @param path *File path*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   An optional path to which the YAML file should be saved (combined with
#'   `filename`).
#' 
#' @param expanded *Expand validation when repeating across multiple columns*
#' 
#'   `scalar<logical>` // *default:* `FALSE`
#' 
#'   Should the written validation expressions for an *agent* be expanded such
#'   that **tidyselect** expressions for columns are evaluated, yielding a
#'   validation function per column? By default, this is `FALSE` so expressions
#'   as written will be retained in the YAML representation.
#' 
#' @param quiet *Inform (or not) upon file writing*
#' 
#'   `scalar<logical>` // *default:* `FALSE`
#' 
#'.  Should the function *not* inform when the file is written?
#'   
#' @return Invisibly returns `TRUE` if the YAML file has been written.
#'   
#' @section Examples:
#' 
#' ## Writing an `agent` object to a YAML file
#' 
#' Let's go through the process of developing an agent with a validation plan.
#' We'll use the `small_table` dataset in the following examples, which will
#' eventually offload the developed validation plan to a YAML file.
#' 
#' ```{r}
#' small_table
#' ```
#' 
#' Creating an `action_levels` object is a common workflow step when creating a
#' **pointblank** agent. We designate failure thresholds to the `warn`, `stop`,
#' and `notify` states using [action_levels()].
#' 
#' ```r
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' ```
#' 
#' Now let's create the `agent` and pass it the `al` object (which serves as a
#' default for all validation steps which can be overridden). The data will be
#' referenced in `tbl` with a leading `~` and this is a requirement for writing
#' to YAML since the preparation of the target table must be self contained.
#' 
#' ```r
#' agent <- 
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = al
#'   )
#' ```
#' 
#' Then, as with any `agent` object, we can add steps to the validation plan by
#' using as many validation functions as we want.
#' 
#' ```r
#' agent <-
#'   agent %>% 
#'   col_exists(columns = c(date, date_time)) %>%
#'   col_vals_regex(
#'     columns = b,
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(columns = d, value = 100) %>%
#'   col_vals_lte(columns = c, value = 5)
#' ```
#'
#' The agent can be written to a **pointblank**-readable YAML file with the
#' `yaml_write()` function. Here, we'll use the filename
#' `"agent-small_table.yml"` and, after writing, the YAML file will be in the
#' working directory:
#' 
#' ```r
#' yaml_write(agent, filename = "agent-small_table.yml")
#' ```
#' 
#' We can view the YAML file in the console with the [yaml_agent_string()]
#' function.
#' 
#' ```r
#' yaml_agent_string(filename = "agent-small_table.yml")
#' ```
#' 
#' ```yaml
#' type: agent
#' tbl: ~small_table
#' tbl_name: small_table
#' label: A simple example with the `small_table`.
#' lang: en
#' locale: en
#' actions:
#'   warn_fraction: 0.1
#'   stop_fraction: 0.25
#'   notify_fraction: 0.35
#' steps:
#' - col_exists:
#'     columns: c(date, date_time)
#' - col_vals_regex:
#'     columns: c(b)
#'     regex: '[0-9]-[a-z]{3}-[0-9]{3}'
#' - rows_distinct:
#'     columns: ~
#' - col_vals_gt:
#'     columns: c(d)
#'     value: 100.0
#' - col_vals_lte:
#'     columns: c(c)
#'     value: 5.0
#' ```
#' 
#' Incidentally, we can also use [yaml_agent_string()] to print YAML in the
#' console when supplying an agent as the input. This can be useful for
#' previewing YAML output just before writing it to disk with `yaml_write()`.
#' 
#' ## Reading an `agent` object from a YAML file
#'
#' There's a YAML file available in the **pointblank** package that's also
#' called `"agent-small_table.yml"`. The path for it can be accessed through
#' `system.file()`:
#' 
#' ```r
#' yml_file_path <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ```
#' 
#' The YAML file can be read as an agent with a pre-existing validation plan by
#' using the [yaml_read_agent()] function.
#' 
#' ```r
#' agent <- yaml_read_agent(filename = yml_file_path)
#' 
#' agent
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_1.png")`
#' }
#' }
#' 
#' This particular agent is using `~ tbl_source("small_table", "tbl_store.yml")`
#' to source the table-prep from a YAML file that holds a table store (can be
#' seen using `yaml_agent_string(agent = agent)`). Let's put that file in the
#' working directory (the **pointblank** package has the corresponding YAML
#' file):
#' 
#' ```r
#' yml_tbl_store_path <-
#'   system.file(
#'     "yaml", "tbl_store.yml",
#'     package = "pointblank"
#'   )
#' 
#' file.copy(from = yml_tbl_store_path, to = ".")
#' ```
#' 
#' As can be seen from the validation report, no interrogation was yet
#' performed. Saving an agent to YAML will remove any traces of interrogation
#' data and serve as a plan for a new interrogation on the same target table. We
#' can either follow this up with with [interrogate()] and get an agent with
#' intel, or, we can interrogate directly from the YAML file with
#' [yaml_agent_interrogate()]:
#' 
#' ```r
#' agent <- yaml_agent_interrogate(filename = yml_file_path)
#' 
#' agent
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_2.png")`
#' }
#' }
#' 
#' ## Writing an `informant` object to a YAML file
#' 
#' Let's walk through how we can generate some useful information for a really
#' small table. We can create an `informant` object with [create_informant()]
#' and we'll again use the `small_table` dataset.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`."
#'   )
#' ```
#' 
#' Then, as with any `informant` object, we can add info text to the
#' using as many `info_*()` functions as we want.
#' 
#' ```r
#' informant <- 
#'   informant %>%
#'   info_columns(
#'     columns = a,
#'     info = "In the range of 1 to 10. (SIMPLE)"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = date,
#'     info = "The date part of `date_time`. (CALC)"
#'   )
#' ```
#' 
#' The informant can be written to a **pointblank**-readable YAML file with the
#' `yaml_write()` function. Here, we'll use the filename
#' `"informant-small_table.yml"` and, after writing, the YAML file will be in
#' the working directory:
#' 
#' ```r
#' yaml_write(informant, filename = "informant-small_table.yml")
#' ```
#' 
#' We can inspect the YAML file in the working directory and expect to see the
#' following:
#' 
#' ```yaml
#' type: informant
#' tbl: ~small_table
#' tbl_name: small_table
#' info_label: A simple example with the `small_table`.
#' lang: en
#' locale: en
#' table:
#'   name: small_table
#'   _columns: 8
#'   _rows: 13.0
#'   _type: tbl_df
#' columns:
#'   date_time:
#'   _type: POSIXct, POSIXt
#' info: Time-based values (e.g., `Sys.time()`).
#' date:
#'   _type: Date
#'   info: Time-based values (e.g., `Sys.time()`). The date part of `date_time`.
#' a:
#'   _type: integer
#'   info: In the range of 1 to 10. (SIMPLE)
#' b:
#'   _type: character
#' c:
#'   _type: numeric
#' d:
#'   _type: numeric
#' e:
#'   _type: logical
#' f:
#'   _type: character
#' ```
#' 
#' ## Reading an `informant` object from a YAML file
#'
#' There's a YAML file available in the **pointblank** package that's also
#' called `"informant-small_table.yml"`. The path for it can be accessed through
#' `system.file()`:
#' 
#' ```r
#' yml_file_path <- 
#'   system.file(
#'     "yaml", "informant-small_table.yml",
#'     package = "pointblank"
#'   )
#' ```
#' 
#' The YAML file can be read as an informant by using the
#' [yaml_read_informant()] function.
#' 
#' ```r
#' informant <- yaml_read_informant(filename = yml_file_path)
#' 
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_3.png")`
#' }
#' }
#' 
#' As can be seen from the information report, the available table metadata was
#' restored and reported. If you expect metadata to change with time, it might
#' be beneficial to use [incorporate()] to query the target table. Or, we can
#' perform this querying directly from the YAML file with
#' [yaml_informant_incorporate()]:
#' 
#' ```r
#' informant <- yaml_informant_incorporate(filename = yml_file_path)
#' ```
#' 
#' There will be no apparent difference in this particular case since
#' `small_data` is a static table with no alterations over time. However,
#' using [yaml_informant_incorporate()] is good practice since this refreshing
#' of data will be important with real-world datasets.
#' 
#' @family pointblank YAML
#' @section Function ID:
#' 11-1
#' 
#' @export
yaml_write <- function(
    ...,
    .list = list2(...),
    filename = NULL,
    path = NULL,
    expanded = FALSE,
    quiet = FALSE
) {

  # Collect a list of pointblank objects
  obj_list <- .list
  
  # Determine which types of pointblank objects
  # are available in `obj_list`
  object_types <- 
    vapply(
      obj_list,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        if (inherits(x, "ptblank_agent")) {
          x <- "agent"
        } else if (inherits(x, "ptblank_informant")) {
          x <- "informant"
        } else if (inherits(x, "tbl_store")) {
          x <- "tbl_store"
        } else {
          x <- NA_character_
        }
      }
    )
  
  if ("tbl_store" %in% object_types) {
    
    tbl_store <- obj_list[[object_types == "tbl_store"]]
    
    x <- as_tbl_store_yaml_list(tbl_store = tbl_store)
    
    if (is.null(filename)) {
      filename <- "tbl_store.yml"
    }
    
    if (!is.null(path)) {
      filename <- file.path(path, filename)
    }
    
    yaml::write_yaml(
      x = x,
      file = filename,
      handlers = list(
        logical = function(x) {
          result <- ifelse(x, "true", "false")
          class(result) <- "verbatim"
          result
        }
      )
    )
    
    # Generate cli message w.r.t. written YAML file
    if (!quiet) {
      cli_bullet_msg(
        msg = "The table store YAML file has been written to `{filename}`",
        bullet = cli::symbol$tick,
        color = "green"
      )
    }
    
    return(invisible(TRUE))
  }
  
  if ("agent" %in% object_types) {
    agent <- obj_list[[object_types == "agent"]]
  } else {
    agent <- NULL
  }
  if ("informant" %in% object_types) {
    informant <- obj_list[[object_types == "informant"]]
  } else {
    informant <- NULL
  }
  
  if (is.null(agent) && is.null(informant)) {
    
    stop(
      "An agent or informant object must be supplied to `yaml_write()`.",
      call. = FALSE
    )
  }

  if (!is.null(agent) && !is.null(informant)) {
    x <- 
      c(
        as_agent_yaml_list(agent = agent, expanded = expanded),
        as_informant_yaml_list(informant = informant)
      )
    
    # TODO: combine with tbl name (e.g., `pointblank-<tbl_name>.yml`)
    
    if (is.null(filename)) {
      filename <- "pointblank.yml"
    }
    
    yaml_type <- "agent and informant"
    
    # TODO: manage conflicts between both YAML representations
    
  } else if (!is.null(agent)) {
    x <- as_agent_yaml_list(agent = agent, expanded = expanded)
    
    if (is.null(filename)) {
      if (!is.null(agent$tbl_name) && !is.na(agent$tbl_name)) {
        filename <- paste0("agent-", agent$tbl_name, ".yml")
      } else {
        filename <- "agent.yml"
      }
    }
    
    yaml_type <- "agent"
    
  } else {
    x <- as_informant_yaml_list(informant = informant)
    
    if (is.null(filename)) {
      if (!is.null(informant$tbl_name) && !is.na(informant$tbl_name)) {
        filename <- paste0("informant-", informant$tbl_name, ".yml")
      } else {
        filename <- "informant.yml"
      }
    }
    
    yaml_type <- "informant"
  }
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  
  # Write the YAML to disk
  yaml::write_yaml(
    x = x,
    file = filename,
    handlers = list(
      logical = function(x) {
        result <- ifelse(x, "true", "false")
        class(result) <- "verbatim"
        result
      }
    )
  )
  
  # Generate cli message w.r.t. written YAML file
  if (!quiet) {
    cli_bullet_msg(
      msg = "The {yaml_type} YAML file has been written to `{filename}`",
      bullet = cli::symbol$tick,
      color = "green"
    )
  }
  
  invisible(TRUE)
}

#' Display **pointblank** YAML using an agent or a YAML file
#' 
#' @description
#' 
#' With **pointblank** YAML, we can serialize an agent's validation plan (with
#' [yaml_write()]), read it back later with a new agent (with
#' [yaml_read_agent()]), or perform an interrogation on the target data table
#' directly with the YAML file (with [yaml_agent_interrogate()]). The
#' `yaml_agent_string()` function allows us to inspect the YAML generated by
#' [yaml_write()] in the console, giving us a look at the YAML without needing
#' to open the file directly. Alternatively, we can provide an *agent* to the
#' `yaml_agent_string()` and view the YAML representation of the validation plan
#' without needing to write the YAML to disk beforehand.
#'
#' @param agent An *agent* object of class `ptblank_agent`. If an object is
#'   provided here, then `filename` must not be provided.
#' 
#' @param filename The name of the YAML file that contains fields related to an
#'   *agent*. If a file name is provided here, then *agent* object must not be
#'   provided in `agent`.
#' 
#' @param path An optional path to the YAML file (combined with `filename`).
#' 
#' @param expanded Should the written validation expressions for an *agent* be
#'   expanded such that **tidyselect** expressions for columns are evaluated, 
#'   yielding a validation function per column? By default, this is `FALSE`
#'   so expressions as written will be retained in the YAML representation.
#' 
#' @return Nothing is returned. Instead, text is printed to the console.
#'   
#' @section Examples:
#'
#' There's a YAML file available in the **pointblank** package that's called
#' `"agent-small_table.yml"`. The path for it can be accessed through
#' `system.file()`:
#' 
#' ```r
#' yml_file_path <- 
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ```
#' 
#' We can view the contents of the YAML file in the console with the
#' `yaml_agent_string()` function.
#' 
#' ```r
#' yaml_agent_string(filename = yml_file_path)
#' ```
#' 
#' ```yaml
#' type: agent
#' tbl: ~ tbl_source("small_table", "tbl_store.yml")
#' tbl_name: small_table
#' label: A simple example with the `small_table`.
#' lang: en
#' locale: en
#' actions:
#'   warn_fraction: 0.1
#'   stop_fraction: 0.25
#'   notify_fraction: 0.35
#' steps:
#' - col_exists:
#'     columns: vars(date)
#' - col_exists:
#'     columns: vars(date_time)
#' - col_vals_regex:
#'     columns: vars(b)
#'     regex: '[0-9]-[a-z]{3}-[0-9]{3}'
#' - rows_distinct:
#'     columns: ~
#' - col_vals_gt:
#'     columns: vars(d)
#'     value: 100.0
#' - col_vals_lte:
#'     columns: vars(c)
#'     value: 5.0
#' ```
#' 
#' Incidentally, we can also use `yaml_agent_string()` to print YAML in the
#' console when supplying an *agent object* as the input. This can be useful for
#' previewing YAML output just before writing it to disk with [yaml_write()].
#' 
#' @family pointblank YAML
#' @section Function ID:
#' 11-5
#' 
#' @export
yaml_agent_string <- function(
    agent = NULL,
    filename = NULL,
    path = NULL,
    expanded = FALSE
) {
  
  switch(
    rlang::check_exclusive(agent, filename),
    agent = {
      # Display the agent's YAML as a nicely formatted string by
      # generating the YAML (`as_agent_yaml_list() %>% as.yaml()`) and
      # then emitting it to the console via `message()`
      message(
        as_agent_yaml_list(
          agent = agent,
          expanded = expanded
        ) %>%
          yaml::as.yaml(
            handlers = list(
              logical = function(x) {
                result <- ifelse(x, "true", "false")
                class(result) <- "verbatim"
                result
              }
            )
          )
      )
    },
    filename = {
      # Display the agent's YAML as a nicely formatted string by
      # reading the YAML file specified by `file` (and perhaps `path`)
      # and then emitting it to the console via `message()`
      if (!is.null(path)) {
        filename <- file.path(path, filename)
      }
      message(
        readLines(filename) %>%
          paste(collapse = "\n")
      )
    }
  )
  
}

as_c_fn <- function(columns) {
  columns <- strsplit(unlist(columns), ", ")[[1]]
  paste0("c(", paste0('"', columns, '"', collapse = ", "), ")")
}

as_list_preconditions <- function(preconditions) {
  
  if (is.null(preconditions[[1]])) {
    
    return(NULL)
    
  } else if (is.function(preconditions[[1]])) {
    
    return(
      paste(deparse(preconditions[[1]]), collapse = "\n") %>%
        gsub("function (x) \n{", "function(x) {", ., fixed = TRUE)
    )
    
  }
  
  as.character(preconditions)
}

as_list_segments <- function(segments) {
  
  if (is.null(segments[[1]])) {
    return(NULL)
  }

  segments <- unlist(segments)
  
  components <- c()
  
  for (i in seq_along(segments)) {
    
    if (rlang::is_formula(segments[[i]]) &&
        !inherits(segments[[i]], "quosure")) {
      
      components <-
        c(components, paste(capture_formula(segments[[i]]), collapse = " "))
      
    } else if (inherits(segments[[i]], "quosure")) {
      
      components <-
        c(components, paste0("vars(", as_label(segments[[i]]), ")"))
    }
  }
  
  paste0("list(", paste(components, collapse = ", "), ")")
}

as_list_active <- function(active) {

  if (is.logical(active[[1]])) {
    return(active[[1]])
  }
  
  as.character(active)
}

to_list_action_levels <- function(actions) {
  
  agent_actions <- actions
  agent_actions[sapply(agent_actions, is.null)] <- NULL
  agent_actions$fns[sapply(agent_actions$fns, is.null)] <- NULL
  
  if (length(agent_actions$fns) == 0) agent_actions$fns <- NULL
  
  if (length(agent_actions$fns) == 0) {
    agent_actions$fns <- NULL
  } else {
    agent_actions$fns <-
      lapply(
        agent_actions$fns,
        FUN = function(x) {
          if (!is.null(x)) x %>% as.character() %>% paste(collapse = "")
        }
      )
  }
  
  list(actions = agent_actions)
}

as_action_levels <- function(actions, actions_default = NULL) {

  agent_actions <- actions
  agent_actions[sapply(agent_actions, is.null)] <- NULL
  agent_actions$fns[sapply(agent_actions$fns, is.null)] <- NULL
  
  if (length(agent_actions$fns) == 0) {
    agent_actions$fns <- NULL
  } else {
    agent_actions$fns <-
      lapply(
        agent_actions$fns,
        FUN = function(x) {
          if (!is.null(x)) x %>% as.character() %>% paste(collapse = "")
        }
      )
  }

  if (!is.null(actions_default)) {
    if (identical(agent_actions, actions_default)) {
      return(NULL)
    }
  }
  
  agent_actions
}

get_schema_list <- function(schema) {

  vals <- schema

  complete <- schema$`__complete__`
  in_order <- schema$`__in_order__`
  is_exact <- schema$`__is_exact__`
  
  type <- ifelse(inherits(schema, "r_type"), "r", "sql")
  
  vals <- 
    vals[!(names(vals) %in% c("__complete__", "__in_order__", "__is_exact__"))]
  
  if (type == "sql") {
    vals <- c(vals, list(`.db_col_types` = "sql"))
  }

  list(
    schema = vals, 
    complete = complete,
    in_order = in_order,
    is_exact = is_exact
  )
}

to_list_read_fn <- function(read_fn) {

  if (inherits(read_fn, "function")) {
    read_fn_char <- utils::capture.output(read_fn)
  } else {
    read_fn_char <- paste(as.character(read_fn), collapse = "")
  }
  
  list(tbl = read_fn_char)
}

to_list_label <- function(label) {
  list(label = label)
}

to_list_fn <- function(fn) {
  list(fn = fn)
}

to_list_info_label <- function(label) {
  list(info_label = label)
}

to_list_tbl_name <- function(tbl_name) {

  if (is.na(tbl_name)) {
    tbl_name <- NULL
  }
  
  list(tbl_name = tbl_name)
}

get_arg_value <- function(value) {
  
  if (inherits(value, "list") && inherits(value[[1]], "quosures")) {
    out <- paste0("vars(", rlang::as_label(value[[1]][[1]]), ")")
  } else if (inherits(value, "list") && inherits(value[[1]], "numeric")) {
    out <- value[[1]] 
  } else {
    out <- as.character(value[[1]])
  }
  
  out
}

get_arg_value_lr <- function(value) {

  if (inherits(value, "quosure")) {
    out <- paste0("vars(", rlang::as_label(value), ")")
  } else if (inherits(value, "numeric")) {
    out <- value
  } else {
    out <- as.character(value)
  }
  
  out
}

prune_lst_step <- function(lst_step) {

  if (
    "preconditions" %in% names(lst_step[[1]]) &&
    is.null(lst_step[[1]][["preconditions"]])
  ) {
    lst_step[[1]]["preconditions"] <- NULL
  }
  if (
    "segments" %in% names(lst_step[[1]]) &&
    is.null(lst_step[[1]][["segments"]])
  ) {
    lst_step[[1]]["segments"] <- NULL
  }
  if (
    "na_pass" %in% names(lst_step[[1]]) &&
    !lst_step[[1]][["na_pass"]]
  ) {
    lst_step[[1]]["na_pass"] <- NULL
  }
  if (
    "active" %in% names(lst_step[[1]]) &&
    lst_step[[1]][["active"]] == "TRUE"
  ) {
    lst_step[[1]]["active"] <- NULL
  }
  if (
    "complete" %in% names(lst_step[[1]]) &&
    lst_step[[1]][["complete"]]
  ) {
    lst_step[[1]]["complete"] <- NULL
  }
  if (
    "inclusive" %in% names(lst_step[[1]]) &&
    (lst_step[[1]][["inclusive"]][1] && lst_step[[1]][["inclusive"]][2])
  ) {
    lst_step[[1]]["inclusive"] <- NULL
  }
  if (
    "in_order" %in% names(lst_step[[1]]) &&
    lst_step[[1]][["in_order"]]
  ) {
    lst_step[[1]]["in_order"] <- NULL
  }
  if (
    "is_exact" %in% names(lst_step[[1]]) &&
    lst_step[[1]][["is_exact"]]
  ) {
    lst_step[[1]]["is_exact"] <- NULL
  }
  if (
    "allow_stationary" %in% names(lst_step[[1]]) &&
    !lst_step[[1]][["allow_stationary"]]
  ) {
    lst_step[[1]]["allow_stationary"] <- NULL
  }
  if (
    "decreasing_tol" %in% names(lst_step[[1]]) &&
    is.null(lst_step[[1]][["decreasing_tol"]])
  ) {
    lst_step[[1]]["decreasing_tol"] <- NULL
  }
  if (
    "increasing_tol" %in% names(lst_step[[1]]) &&
    is.null(lst_step[[1]][["increasing_tol"]])
  ) {
    lst_step[[1]]["increasing_tol"] <- NULL
  }
  if ("actions" %in% names(lst_step[[1]])) {
    if (
      length(lst_step[[1]][["actions"]][["action_levels"]]) == 1 &&
      length(lst_step[[1]][["actions"]][["action_levels"]][["fns"]]) == 0
    ) {
      lst_step[[1]][["actions"]] <- NULL
    } else if (is.null(lst_step[[1]][["actions"]])) {
      lst_step[[1]][["actions"]] <- NULL
    }
  }
  if ("label" %in% names(lst_step[[1]]) && is.na(lst_step[[1]][["label"]])) {
    lst_step[[1]]["label"] <- NULL
  }
  
  lst_step
}

as_agent_yaml_list <- function(agent, expanded) {

  if (is.null(agent$read_fn)) {
    stop(
      "The agent must have a `tbl` value that can be put into YAML.",
       call. = FALSE
    )
  }

  action_levels_default <- as_action_levels(agent$actions)
  end_fns <- agent$end_fns %>% unlist()
  
  lst_label <- to_list_label(agent$label)
  lst_tbl_name <- to_list_tbl_name(agent$tbl_name)
  lst_read_fn <- to_list_read_fn(agent$read_fn)
  
  if (is.null(action_levels_default)) {
    lst_action_levels <- NULL
  } else {
    lst_action_levels <- list(actions = action_levels_default)
  }
  
  if (is.null(end_fns)) {
    lst_end_fns <- NULL
  } else {
    lst_end_fns <- list(end_fns = as.character(end_fns))
  }
  
  if (is.null(agent$embed_report) || 
      (!is.null(agent$embed_report) && !agent$embed_report)) {
    lst_embed_report <- NULL
  } else {
    lst_embed_report <- list(embed_report = agent$embed_report)
  }

  if (is.null(agent$lang)) {
    lst_lang <- "en"
  } else {
    lst_lang <- list(lang = agent$lang)
  }
  
  if (is.null(agent$locale)) {
    lst_locale <- "en"
  } else {
    lst_locale <- list(locale = agent$locale)
  }

  # Select only the necessary columns from the agent's `validation_set`
  if (!expanded) {
    
    # This subset of `agent$validation_set` will depend on the value of
    # `expanded` (default is FALSE, which preserves tidyselect expressions
    # and doesn't split `vars()`)
  
    agent_validation_set <- 
      agent$validation_set %>% 
      dplyr::select(
        i_o, assertion_type, columns_expr, column, values, na_pass,
        preconditions, seg_expr, actions, label, brief, active
      ) %>%
      dplyr::group_by(i_o) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(i = i_o)
    
    # Temporary conversion of `$label` to list-column
    step_labels <- split(agent$validation_set$label, agent$validation_set$i_o)
    step_labels_collapsed <- lapply(step_labels, function(label) {
      # Collapse `label` when possible
      if (all(is.na(label)) || all(label == label[1])) label[1] else label
    })
    agent_validation_set$label <- unname(step_labels_collapsed)
  
  } else {
    
    # This subset of `agent$validation_set` has the same number of
    # validation steps (i) as the agent report; in this, tidyselect
    # expressions and `vars()` expressions with multiple columns are
    # evaluated and split into a validation step per target column
    
    agent_validation_set <- 
      agent$validation_set %>% 
      dplyr::select(
        i, assertion_type, columns_expr, column, values, na_pass,
        preconditions, seg_expr, actions, label, brief, active
      )
  }
  
  all_steps <- list()
  
  for (i in seq_len(nrow(agent_validation_set))) {
    
    step_list <- agent_validation_set[i, ] %>% as.list()

    validation_fn <- step_list$assertion_type
    
    if (validation_fn %in% c(
      "col_vals_lt", "col_vals_lte",
      "col_vals_equal", "col_vals_not_equal",
      "col_vals_gte", "col_vals_gt"
    )) {
      
      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            value = get_arg_value(step_list$values),
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (grepl("between", validation_fn)) {

      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )

      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            left = get_arg_value_lr(step_list$values[[1]][[1]]),
            right = get_arg_value_lr(step_list$values[[1]][[2]]),
            inclusive = as.logical(
              c(
                names(step_list$values[[1]][1]),
                names(step_list$values[[1]][2])
              )
            ),
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (grepl("(in_set|make_set|make_subset)", validation_fn)) {

      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            set = step_list$values[[1]],
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )

    } else if (grepl("null", validation_fn)) {
      
      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_vals_increasing") {
      
      if (step_list$values[[1]][2] == 0) {
        decreasing_tol <- NULL
      } else {
        decreasing_tol <- step_list$values[[1]][2]
      }
      
      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            allow_stationary = ifelse(
              step_list$values[[1]][1] == 1, TRUE, FALSE
            ),
            decreasing_tol = decreasing_tol,
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_vals_decreasing") {
      
      if (step_list$values[[1]][2] == 0) {
        increasing_tol <- NULL
      } else {
        increasing_tol <- step_list$values[[1]][2]
      }
      
      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            allow_stationary = ifelse(
              step_list$values[[1]][1] == 1, TRUE, FALSE
            ),
            increasing_tol = increasing_tol,
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_vals_regex") {

      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            regex = get_arg_value(step_list$values),
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_vals_within_spec") {
      
      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            spec = get_arg_value(step_list$values),
            na_pass = step_list$na_pass,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (grepl("col_is_", validation_fn) ||
               validation_fn == "col_exists") {

      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_vals_expr") {

      lst_step <- 
        list(
          validation_fn = list(
            expr = paste0("~", rlang::as_label(step_list$values[[1]])),
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn %in% c("rows_distinct", "rows_complete")) {

      column_text <- 
        get_column_text(
          step_list = step_list,
          expanded = expanded
        )
      
      lst_step <- 
        list(
          validation_fn = list(
            columns = column_text,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "row_count_match") {
      
      count <- step_list$values[[1]]
      
      # Disallow YAML writing if value obtained is a table object
      if (is_a_table_object(count)) {
        stop(
          "We cannot write a table object supplied as `count` to YAML:\n",
          "* Use a table-prep formula or a function that instead",
          call. = FALSE
        )
      }
      
      if (is.function(count)) {
        count <- capture_function(fn = count)
      }
      
      if (rlang::is_formula(count)) {
        count <- capture_formula(count, separate = FALSE)
      }
      
      if (is.numeric(count)) {
        count <- as.integer(count)
      }
        
      lst_step <- 
        list(
          validation_fn = list(
            count = count,
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_count_match") {
      
      count <- step_list$values[[1]]
      
      # Disallow YAML writing if value obtained is a table object
      if (is_a_table_object(count)) {
        stop(
          "We cannot write a table object supplied as `count` to YAML:\n",
          "* Use a table-prep formula or a function that instead",
          call. = FALSE
        )
      }
      
      if (is.function(count)) {
        count <- capture_function(fn = count)
      }
      
      if (rlang::is_formula(count)) {
        count <- capture_formula(count, separate = FALSE)
      }
      
      if (is.numeric(count)) {
        count <- as.integer(count)
      }
      
      lst_step <- 
        list(
          validation_fn = list(
            count = count,
            preconditions = as_list_preconditions(step_list$preconditions),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
    
    } else if (validation_fn == "tbl_match") {
      
      # TODO: disallow YAML writing if value obtained from
      # `get_arg_value(step_list$values)` is a table object or is
      # not a function or table-prep formula
      tbl_compare <- step_list$values
      
      lst_step <- 
        list(
          validation_fn = list(
            tbl_compare = as_list_preconditions(tbl_compare),
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "col_schema_match") {
      
      schema_list <- get_schema_list(schema = step_list$values[[1]])

      lst_step <- 
        list(
          validation_fn = list(
            schema = schema_list$schema,
            complete = schema_list$complete,
            in_order = schema_list$in_order,
            is_exact = schema_list$is_exact,
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "conjointly") {
      
      lst_step <- 
        list(
          validation_fn = list(
            fns = as.character(step_list$values[[1]]),
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "serially") {
      
      lst_step <- 
        list(
          validation_fn = list(
            fns = as.character(step_list$values[[1]]),
            preconditions = as_list_preconditions(step_list$preconditions),
            segments = as_list_segments(step_list$seg_expr),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
      
    } else if (validation_fn == "specially") {
      
      lst_step <- 
        list(
          validation_fn = list(
            fn = to_list_fn(
              capture_function(fn = step_list$values[[1]], escape = FALSE)
            ),
            preconditions = as_list_preconditions(step_list$preconditions),
            actions = as_action_levels(
              step_list$actions[[1]],
              action_levels_default
            ),
            label = step_list$label,
            active = as_list_active(step_list$active)
          )
        )
    }

    # Remove list elements that are representative of defaults
    lst_step <- prune_lst_step(lst_step)
    
    # Unlist labels as character vector/scalar
    lst_step$validation_fn$label <- lst_step$validation_fn$label[[1]]

    # Set the top level list-element name to that of
    # the validation function
    names(lst_step) <- validation_fn
    all_steps <- c(all_steps, list(lst_step))
  }
  
  c(
    type = "agent",               # YAML type: `agent`
    lst_read_fn,                  # table-prep formula (stored in key `tbl`)
    lst_tbl_name,                 # table name
    lst_label,                    # agent label
    lst_lang,                     # agent language
    lst_locale,                   # agent locale
    lst_action_levels,            # agent action levels statement
    lst_end_fns,                  # agent end functions statement
    lst_embed_report,             # agent embed report in saved file
    list(steps = all_steps)       # list of validation steps
  )
}

get_column_text <- function(step_list, expanded) {
  
  if (!expanded) {
    
    if (!is.na(step_list$column[[1]]) &&
        step_list$column[[1]] == step_list$columns_expr) {
      
      column_text <- as_c_fn(step_list$column[[1]])
      
    } else {
      column_text <- step_list$columns_expr
    }
    
    # Strip tidyselect namespacing for leaner yaml writing
    column_text <- gsub("\\btidyselect::", "", column_text)
    
  } else {
    
    column_text <- as_c_fn(columns = step_list$column[[1]])
  }
  
  column_text
}

as_informant_yaml_list <- function(informant) {

  if (is.null(informant$read_fn)) {
    stop(
      "The informant must have a `tbl` value that can be put into YAML.",
      call. = FALSE
    )
  }
  
  lst_tbl_name <- to_list_tbl_name(informant$tbl_name)
  lst_read_fn <- to_list_read_fn(informant$read_fn)
  lst_info_label <- to_list_info_label(informant$info_label)
  
  if (length(informant$meta_snippets) > 0) {
    
    lst_meta_snippets <- 
      list(
        meta_snippets = 
          lapply(
            informant$meta_snippets,
            FUN = function(x) {
              paste(as.character(x), collapse = "")
            }
          )
      )
  } else {
    lst_meta_snippets <- NULL
  }
  
  if (is.null(informant$lang)) {
    lst_lang <- "en"
  } else {
    lst_lang <- list(lang = informant$lang)
  }
  
  if (is.null(informant$locale)) {
    lst_locale <- "en"
  } else {
    lst_locale <- list(locale = informant$locale)
  }
  
  # Hide private field
  metadata <- informant$metadata[names(informant$metadata) != "_private"]
  
  c(
    type = "informant",           # YAML type: `informant`
    lst_read_fn,                  # table-prep formula (stored in key `tbl`)
    lst_tbl_name,                 # table name
    lst_info_label,               # informant label
    lst_lang,                     # informant language
    lst_locale,                   # informant locale
    lst_meta_snippets,            # informant metadata snippet statements
    metadata                      # informant metadata entries
  )
}

as_tbl_store_yaml_list <- function(tbl_store) {

  tbl_list <- list()
  
  for (i in seq_along(tbl_store)) {
    
    formula_rhs <- capture_formula(tbl_store[[i]])[2]
    tbl_name <- names(tbl_store[i])
    
    list_element <- list(formula_rhs)
    tbl_list[i] <- list_element
    
    names(tbl_list)[i] <- tbl_name
  }
  
  lst_tbls <- list(tbls = tbl_list)
  
  if (!is.null(attr(tbl_store, which = "pb_init", exact = TRUE))) {
    
    init_stmt <- attr(tbl_store, which = "pb_init", exact = TRUE)
    init_stmt <- capture_formula(init_stmt)[2]
    
    Sys.sleep(0.1)
    
    lst_init <- list(init = init_stmt)
    
  } else {
    lst_init <- NULL
  }
  
  c(
    type = "tbl_store",           # YAML type: `tbl_store`
    lst_tbls,                     # table store list of table-prep formulas
    lst_init                      # initialization statement
  )
}
