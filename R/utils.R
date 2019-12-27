#' Add properly formatted validation steps
#' 
#' @noRd
create_validation_step <- function(agent,
                                   assertion_type,
                                   column,
                                   value = NULL,
                                   set = NULL,
                                   regex = NULL,
                                   incl_na = NULL,
                                   preconditions = NULL,
                                   brief = NULL,
                                   warn_count = NULL,
                                   stop_count = NULL,
                                   notify_count = NULL,
                                   warn_fraction = NULL,
                                   stop_fraction = NULL,
                                   notify_fraction = NULL) {
  
  # Create a validation step as a single-row
  # `tbl_df` object
  validation_step_df <-
    dplyr::tibble(
      assertion_type = assertion_type,
      column = as.character(column),
      value = ifelse(is.null(value), NA_real_, as.numeric(value)),
      set = ifelse(is.null(set), list(NULL), list(set)),
      regex = ifelse(is.null(regex), NA_character_, as.character(regex)),
      incl_na = ifelse(is.null(incl_na), as.logical(NA), as.logical(incl_na)),
      preconditions = ifelse(is.null(preconditions), list(NULL), list(preconditions)),
      brief = ifelse(is.null(brief), NA_character_, as.character(brief)),
      all_passed = as.logical(NA),
      n = NA_integer_,
      n_passed = NA_integer_,
      n_failed = NA_integer_,
      f_passed = NA_real_,
      f_failed = NA_real_,
      warn_count = ifelse(is.null(warn_count), NA_real_, as.numeric(warn_count)),
      stop_count = ifelse(is.null(stop_count), NA_real_, as.numeric(stop_count)),
      notify_count = ifelse(is.null(notify_count), NA_real_, as.numeric(notify_count)),
      warn_fraction = ifelse(is.null(warn_fraction), NA_real_, as.numeric(warn_fraction)),
      stop_fraction = ifelse(is.null(stop_fraction), NA_real_, as.numeric(stop_fraction)),
      notify_fraction = ifelse(is.null(notify_fraction), NA_real_, as.numeric(notify_fraction))
    )
  
  # Append `validation_step` to `validation_set`
  agent$validation_set <- 
    dplyr::bind_rows(agent$validation_set, validation_step_df)
  
  agent
}

#' Acquire information on the coordinates of a remote table
#' 
#' If a table is remote (i.e., in a database), this function will be invoked to
#' set an entry point for the interrogation query.
#' 
#' @noRd
set_entry_point <- function(table,
                            db_type = NULL,
                            creds_file = NULL,
                            db_creds_env_vars = NULL,
                            initial_sql = NULL) {
  
  if (is.null(db_type) & inherits(table, "data.frame")) {
    
    # Create table entry object
    tbl_entry <- table
  }
  
  if (!is.null(db_type)) {
    
    if (db_type == "PostgreSQL") {
      
      # Establish a new PostgreSQL connection
      if (!is.null(creds_file)) {
        
        # Serialize the credentials RDS file
        credentials <- readRDS(creds_file)
        
        # Establish the connection with the serialized RDS object
        connection <-
          DBI::dbConnect(
            RPostgreSQL::PostgreSQL(max.con = 512),
            dbname = credentials[1],
            host = credentials[2],
            port = credentials[3],
            user = credentials[4],
            password = credentials[5]
          )
        
      } else if (!is.null(db_creds_env_vars)) {
        
        # Establish the connection with the environment variables
        connection <-
          DBI::dbConnect(
            RPostgreSQL::PostgreSQL(max.con = 512),
            dbname = Sys.getenv(db_creds_env_vars[[1]]),
            host = Sys.getenv(db_creds_env_vars[[2]]),
            port = Sys.getenv(db_creds_env_vars[[3]]),
            user = Sys.getenv(db_creds_env_vars[[4]]),
            password = Sys.getenv(db_creds_env_vars[[5]])
          )
        
      } else if (is.null(creds_file)) {
        
        stop("Environment variables or a credentials file is required to access the database.")
      }
      
      if (is.null(initial_sql)) {
        
        # Create table entry object
        tbl_entry <- dplyr::tbl(src = connection, table)
      }
      
      if (!is.null(initial_sql)) {
        
        # Remove extra spaces within the SQL string
        initial_sql <-
          gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", initial_sql, perl = TRUE)
        
        if (grepl("^(SELECT|select)", initial_sql)) {
          
          # If there is a `SELECT` or `select` keyword
          # in the `initial_sql` statement, provide
          # the entire SQL statement to dplyr::tbl
          # without changing the content
          tbl_entry <- dplyr::tbl(src = connection, dplyr::sql(initial_sql))
          
        } else {
          
          # If there is no `SELECT` or `select` lead in,
          # insert that line at the beginning with the
          # table name then carry on with the rest of the
          # statement
          tbl_entry <- 
            dplyr::tbl(
              src = connection,
              dplyr::sql(paste0("SELECT * FROM ", table, " ", initial_sql))
            )
        }
      }
    } else if (db_type == "MySQL") {
      
      # Establish a new MySQL connection
      if (!is.null(creds_file)) {
        
        # Serialize the credentials RDS file
        credentials <- readRDS(creds_file)
        
        # Establish the connection with the serialized RDS object
        connection <-
          RMySQL::dbConnect(
            RMySQL::MySQL(),
            dbname = credentials[1],
            host = credentials[2],
            port = as.integer(credentials[3]),
            user = credentials[4],
            password = credentials[5]
          )
        
      } else if (!is.null(db_creds_env_vars)) {
        
        # Establish the connection with the environment variables
        connection <-
          RMySQL::dbConnect(
            RMySQL::MySQL(),
            dbname = Sys.getenv(db_creds_env_vars[[1]]),
            host = Sys.getenv(db_creds_env_vars[[2]]),
            port = as.integer(Sys.getenv(db_creds_env_vars[[3]])),
            user = Sys.getenv(db_creds_env_vars[[4]]),
            password = Sys.getenv(db_creds_env_vars[[5]])
          )
        
      } else if (is.null(creds_file)) {
        
        stop("Environment variables or a credentials file is required to access the database.")
      }
      
      if (is.null(initial_sql)) {
        
        # Create a table entry object
        tbl_entry <- dplyr::tbl(src = connection, table)
      }
      
      if (!is.null(initial_sql)) {
        
        # Create a table entry object with an initial
        # SQL SELECT statement
        tbl_entry <- 
          dplyr::tbl(
            src = connection,
            dplyr::sql(paste0("SELECT * FROM ", table, " ", initial_sql))
          )
      }
    }
  }
  
  tbl_entry
}

get_tbl_object <- function(agent, idx) {

  if (agent$validation_set$db_type[idx] == "local_df") {
    
    # Create `table` object as the direct reference to a
    # local `data.frame` or `tbl_df` object
    tbl <- get(agent$validation_set$tbl_name[idx])
    
  } else if (agent$validation_set$db_type[idx] == "local_file") {
    
    file_path <- agent$validation_set$file_path[idx]
    col_types <- agent$validation_set$col_types[idx]
    
    # Infer the file type from the extension
    file_extension <- 
      (agent$validation_set$file_path[idx] %>% 
         basename() %>% 
         stringr::str_split(pattern = "\\.") %>% 
         unlist())[2] %>% 
      tolower()
    
    if (is.na(col_types)) {
      
      if (file_extension == "csv") {
        
        tbl <- 
          suppressMessages(
            readr::read_csv(
              file = file_path))
        
      } else if (file_extension == "tsv") {
        
        tbl <- 
          suppressMessages(
            readr::read_tsv(
              file = file_path))
      }
    }
    
    if (!is.na(col_types)) {
      
      if (file_extension == "csv") {
        
        tbl <- 
          suppressMessages(
            readr::read_csv(
              file = file_path,
              col_types = col_types))
        
      } else if (file_extension == "tsv") {
        
        tbl <- 
          suppressMessages(
            readr::read_tsv(
              file = file_path,
              col_types = col_types))
      }
    }
  } else if (agent$validation_set$db_type[idx] %in% c("PostgreSQL", "MySQL")) {
    
    # Determine if there is an initial SQL
    # statement available as part of the 
    # table focus (`focal_sql`) or as
    # part of the validation step (`init_sql`)
    if (!is.na(agent$validation_set$init_sql[idx])) {
      initial_sql_stmt <- agent$validation_set$init_sql[idx]
    } else if (!is.na(agent$focal_init_sql)) {
      initial_sql_stmt <- agent$focal_init_sql
    } else {
      initial_sql_stmt <- NULL
    }
    
    # Create `tbl` object as an SQL entry point for a remote table
    
    if (!is.na(agent$focal_db_cred_file_path)) {
      
      tbl <- 
        set_entry_point(
          table = agent$validation_set$tbl_name[idx],
          db_type = agent$validation_set$db_type[idx],
          creds_file = agent$validation_set$db_cred_file_path[idx],
          initial_sql = initial_sql_stmt)
      
    } else if (length(agent$focal_db_env_vars) != 0) {
      
      tbl <- 
        set_entry_point(
          table = agent$validation_set$tbl_name[idx],
          db_type = agent$validation_set$db_type[idx],
          db_creds_env_vars = agent$focal_db_env_vars,
          initial_sql = initial_sql_stmt)
    }
  }
  
  tbl
}

apply_preconditions_to_tbl <- function(agent, idx, tbl) {

  preconditions <- agent$validation_set$preconditions[[idx]]
  
  if (!is.null(preconditions)) {
    
    tbl <- 
      preconditions %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
  }
  
  tbl
}

get_focal_tbl_object <- function(agent) {
  agent$focal_tbl
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

get_column_incl_na_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "incl_na"]]
}

get_column_regex_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "regex"]]
}

#' Get all column names from the table currently in focus
#' 
#' @noRd
get_all_cols <- function(agent) {
  
  # Get vector of all columns table currently in focus
  agent$focal_col_names
}



#' Generate summary SVG files for the results of a validation pipeline
#' 
#' @noRd
generate_img_files_results <- function(agent) {
  
  if (!inherits(agent, "ptblank_agent")) {
    stop("The object provided must be a valid `ptblank_agent` object.")
  }
  
  # Extract the `validation_set` df from the `agent` object
  summary <- agent$validation_set
  
  # For every row in `summary`, re-work the associated SVG
  # template object into a finalized graphic
  for (i in 1:nrow(summary)) {
    
    if (i == 1) {
      if (dir.exists("temporary_images") == FALSE) {
        dir.create("temporary_images")
      } else {
        files <- list.files("temporary_images", full.names = TRUE)
        if (length(files) > 0) {
          file.remove(files)
        }
      }
    }
    
    index <- formatC(x = i, flag = " ", width = 4)
    
    pass <- 
      formatC(
        x = summary$n_passed[i] %>% as.integer(),
        flag = " ", width = 12
      ) %>%
      stringr::str_replace_all(" ", "&#160;")
    
    fail <- 
      formatC(
        x = (summary$n[i] - summary$n_passed[i]) %>% as.integer(),
        flag = " ", width = 12
      ) %>% 
      stringr::str_replace_all(" ", "&#160;")
    
    if (summary$notify[i] == TRUE) {
      outline_color <- "#B20000"
    } else if (summary$notify[i] == FALSE & summary$warn[i] == TRUE) {
      outline_color <- "#B7B700"
    }
    
    if (summary$all_passed[i] == TRUE) {
      outline_color <- "#008000"
    }
    
    # Construct the filename for the SVG file associated with the function
    icon <- paste0(summary$assertion_type[i], "_text.svg")
    
    # Copy the text-inclusive SVG file to a temporary directory
    file.copy(
      from = system.file("icons", icon, package = "pointblank"),
      to = paste0(
        "./temporary_images/",
        stringr::str_replace_all(index, " ", "0"), ".svg"
      ),
      overwrite = TRUE
    )
    
    # Modify the summary numbers
    modified_svg <-
      readLines(
        paste0(
          "./temporary_images/",
          stringr::str_replace_all(index, " ", "0"),
          ".svg"
        ),
        warn = FALSE
      ) %>%
      stringr::str_replace(">XXXX<", paste0(">", index, "<")) %>%
      stringr::str_replace(">PPPPPPPPPPPP<", paste0(">", pass, "<")) %>%
      stringr::str_replace(">FFFFFFFFFFFF<", paste0(">", fail, "<"))
    
    # Modify the outline color
    modified_svg <-
      modified_svg %>%
      stringr::str_replace(
        "(\"function.*? stroke=\")#979797",
        paste0("\\1", outline_color)
      )
    
    # Write the modified SVG file to disk
    modified_svg %>%
      cat(
        file = paste0(
          "./temporary_images/",
          stringr::str_replace_all(index, " ", "0"),
          "_.svg"
        )
      )
  }
}

#' Generate SVG files for the plan of a validation pipeline
#'
#' @noRd
generate_img_files_plan <- function(agent) {
  
  if (!inherits(agent, "ptblank_agent")) {
    stop("The object provided must be a valid `ptblank_agent` object.")
  }
  
  # Extract the `logical_plan` df from the `agent` object
  plan <- agent$logical_plan
  
  # For every row in `summary`, re-work the associated SVG
  # template object into a finalized graphic
  for (i in 1:nrow(plan)) {
    
    if (i == 1) {
      if (dir.exists("temporary_images_plan") == FALSE) {
        dir.create("temporary_images_plan")
      } else {
        files <- list.files("temporary_images_plan", full.names = TRUE)
        if (length(files) > 0) {
          file.remove(files)
        }
      }
    }
    
    index <- formatC(x = i, flag = " ", width = 4)
    
    # Construct the filename for the SVG file associated with the function
    icon <- paste0(plan$component_name[i], "_.svg")
    
    # Copy the text-inclusive SVG file to a temporary directory
    file.copy(
      from = system.file("icons", icon, package = "pointblank"),
      to = paste0(
        "./temporary_images_plan/",
        stringr::str_replace_all(index, " ", "0"), ".svg"
      ),
      overwrite = TRUE
    )
  }
}

#' Does the agent have no validation steps available in the object?
#' 
#' @noRd
is_agent_empty <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    
    if (nrow(agent$validation_set) == 0 &
        nrow(agent$logical_plan) == 1 ) {
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
    }
    
  } else {
    
    return(FALSE)
  }
}

#' Did the agent carry out an interrogation?
#' 
#' @noRd
did_agent_interrogate <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    return(ifelse(length(agent$validation_time) > 0, TRUE, FALSE))
  } else {
    return(NA)
  }
}

#' When did the agent carry out an interrogation?
#' 
#' @noRd
interrogation_time <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    if (did_agent_interrogate(agent)) {
      
      return(agent$validation_time)
      
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

#' How many validation steps are associated with the agent?
#' 
#' @noRd
number_of_validation_steps <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    return(agent$validation_set %>% nrow())
  } else {
    return(NA)
  }
}

#' How many validation steps are associated with the agent?
#' 
#' @noRd
create_autobrief <- function(agent,
                             assertion_type,
                             preconditions = NULL,
                             column = NULL,
                             value = NULL,
                             regex = NULL,
                             set = NULL,
                             left = NULL,
                             right = NULL) {
  
  if (assertion_type %in%
      c("col_vals_gt", "col_vals_gte",
        "col_vals_lt", "col_vals_lte",
        "col_vals_equal", "col_vals_not_equal")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    if (assertion_type == "col_vals_gt") {
      operator <- ">"
    } else if (assertion_type == "col_vals_gte") {
      operator <- ">="
    } else if (assertion_type == "col_vals_lt") {
      operator <- "<"
    } else if (assertion_type == "col_vals_lte") {
      operator <- "<="
    } else if (assertion_type == "col_vals_equal") {
      operator <- "=="
    } else if (assertion_type == "col_vals_not_equal") {
      operator <- "!="
    } 

    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should be ", operator, " ", value
      )
  }
  
  
  if (assertion_type == "cols_exist") {
    
    autobrief <- paste0("Expect that column `", column, "` exists")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)

    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_in_set", "not ", ""),
        "be part of set `", paste(set, collapse = ", "), "`"
      )
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_in_set", "not ", ""),
        "be part of set `", paste(set, collapse = ", "), "`"
      )
  }
  
  if (assertion_type %in%
      c("col_vals_between", "col_vals_not_between")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_between", "not ", ""),
        "be between `", left, "` and `", right, "`"
      )
  }
  
  if (assertion_type == "col_vals_regex") {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should match the regex expression `",
        regex, "`"
      )
  }
  
  if (assertion_type %in% c("col_vals_null", "col_vals_not_null")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_null", "not ", ""),
        "be NULL"
      )
  }
  
  if (grepl("col_is_.*", assertion_type)) {
    
    if (assertion_type %in% 
        c("col_is_numeric", "col_is_integer",
          "col_is_character", "col_is_logical",
          "col_is_factor")) {
      
      col_type <- gsub("col_is_", "", assertion_type)
    } else if (assertion_type == "col_is_posix") {
      col_type <- "POSIXct"
    } else if (assertion_type == "col_is_date") {
      col_type <- "Date"
    }
    
    autobrief <- 
      paste0("Expect that column `", column, "` is `", col_type, "`-based")
  }
  
  if (assertion_type == "rows_not_duplicated") {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")
        ),
        "rows from `", column, "` ", "have no duplicates"
      )
  }
  
  autobrief
}

ib_incl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} >= set[1] & {{ column }} <= set[2] ~ TRUE,
      {{ column }} < set[1] | {{ column }} > set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
ib_excl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} > set[1] & {{ column }} <= set[2] ~ TRUE,
      {{ column }} <= set[1] | {{ column }} > set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
ib_incl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} >= set[1] & {{ column }} < set[2] ~ TRUE,
      {{ column }} < set[1] | {{ column }} >= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
ib_excl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} > set[1] & {{ column }} < set[2] ~ TRUE,
      {{ column }} <= set[1] | {{ column }} >= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_incl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} < set[1] | {{ column }} > set[2] ~ TRUE,
      {{ column }} >= set[1] & {{ column }} <= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_excl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} <= set[1] | {{ column }} > set[2] ~ TRUE,
      {{ column }} > set[1] & {{ column }} <= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_incl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} < set[1] | {{ column }} >= set[2] ~ TRUE,
      {{ column }} >= set[1] & {{ column }} < set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_excl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} <= set[1] | {{ column }} >= set[2] ~ TRUE,
      {{ column }} > set[1] & {{ column }} < set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}

#' Perform a single column validation that can issue warnings
#' 
#' @noRd
evaluate_single <- function(x,
                            type,
                            column,
                            value = NULL,
                            set = NULL,
                            regex = NULL,
                            left = NULL,
                            right = NULL,
                            incl_na = NULL,
                            preconditions,
                            warn_count,
                            stop_count,
                            notify_count,
                            warn_fraction,
                            stop_fraction,
                            notify_fraction) {
  
  x_ret <- x
  tbl <- x

  # Get the `column` number
  col_number <- ((tbl %>% colnames()) %in% column) %>% which()

  # Apply any preconditions
  if (!is.null(preconditions)) {
    
    tbl <- 
      preconditions %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
  }
  
  if (type == "col_vals_equal") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) == value
  }
  
  if (type == "col_vals_not_equal") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) != value
  }
  
  if (type == "col_vals_gt") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) > value
  }
  
  if (type == "col_vals_gte") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) >= value
  }
  
  if (type == "col_vals_lt") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) < value
  }
  
  if (type == "col_vals_lte") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) <= value
  }
  
  if (type == "col_vals_between") {
    
    vals <- 
      tbl %>%
      dplyr::pull(col_number)
    
    logicals <- 
      vals >= left &
      vals <= right
    
    if (incl_na == TRUE) {
      logicals[which(is.na(logicals))] <- TRUE
    } else if (incl_na == FALSE) {
      logicals[which(is.na(logicals))] <- FALSE
    }
  }
  
  if (type == "col_vals_not_between") {
    
    vals <- 
      tbl %>%
      dplyr::pull(col_number)
    
    logicals <- 
      vals < left |
      vals > right
    
    if (incl_na == TRUE) {
      logicals[which(is.na(logicals))] <- TRUE
    } else if (incl_na == FALSE) {
      logicals[which(is.na(logicals))] <- FALSE
    }
  }
  
  if (type == "col_vals_in_set") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) %in% set
  }
  
  if (type == "col_vals_not_in_set") {
    
    logicals <- 
      !(tbl %>%
          dplyr::pull(col_number) %in% set)
  }
  
  if (type == "col_vals_regex") {
    
    vals <- 
      tbl %>%
      dplyr::pull(col_number)
    
    logicals <- 
      grepl(pattern = regex, x = vals)
  }
  
  if (type == "col_vals_not_null") {
    
    logicals <- 
      !is.na(tbl %>%
               dplyr::pull(col_number))
  }
  
  if (type == "col_vals_null") {
    
    logicals <- 
      is.na(tbl %>%
              dplyr::pull(col_number))
  }
  
  if (grepl("col_is_.*", type)) {
    
    # Get the column type
    column_type <-
      (tbl %>%
         dplyr::select(column) %>%
         utils::head(1) %>%
         dplyr::collect() %>%
         as.data.frame(stringsAsFactors = FALSE)
      )[1, 1] %>% 
      class()
    
    if (type == "col_is_numeric") {
      logicals <- ifelse(column_type[1] == "numeric", TRUE, FALSE)
    } else if (type == "col_is_integer") {
      logicals <- ifelse(column_type[1] == "integer", TRUE, FALSE)
    } else if (type == "col_is_character") {
      logicals <- ifelse(column_type[1] == "character", TRUE, FALSE)
    } else if (type == "col_is_logical") {
      logicals <- ifelse(column_type[1] == "logical", TRUE, FALSE)
    } else if (type == "col_is_factor") {
      logicals <- ifelse(column_type[1] == "factor", TRUE, FALSE)
    } else if (type == "col_is_posix") {
      logicals <- ifelse(column_type[1] == "POSIXct", TRUE, FALSE)
    } else if (type == "col_is_date") {
      logicals <- ifelse(column_type[1] == "Date", TRUE, FALSE)
    } else {
      logicals <- FALSE
    }
  }
  
  if (type == "cols_exist") {
    
    column_names <-
      tbl %>%
      utils::head(1) %>%
      dplyr::as_tibble() %>%
      colnames()
    
    logicals <- ifelse(column %in% column_names, TRUE, FALSE)
  }
  
  logicals[which(is.na(logicals))] <- FALSE
  
  total_count <- length(logicals)
  true_count <- sum(logicals)
  false_count <- total_count - true_count
  false_fraction <- false_count / total_count
  
  if (!is.null(notify_count)) {
    if (false_count >= notify_count) {
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `notify_count` threshold",
        " * `failing_count` ({false_count}) >= `notify_count` ({notify_count})",
        type = type,
        false_count = false_count,
        notify_count = notify_count,
        .format = "{text}"
      )
    }
  } else if (!is.null(notify_fraction)) {
    if ((false_count/total_count) >= notify_fraction) {
      
      false_fraction <- round(false_fraction, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `notify_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `notify_fraction` ({notify_fraction})",
        type = type,
        false_fraction = false_fraction,
        notify_fraction = notify_fraction,
        .format = "{text}"
      )
    }
  }
  
  if (!is.null(stop_count)) {
    if (false_count >= stop_count) {
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_count` threshold",
        " * `failing_count` ({false_count}) >= `stop_count` ({stop_count})",
        type = type,
        false_count = false_count,
        stop_count = stop_count,
        .format = "{text}"
      )
    }
  } else if (!is.null(stop_fraction)) {
    if ((false_count/total_count) >= stop_fraction) {
      
      false_fraction <- round(false_fraction, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `stop_fraction` ({stop_fraction})",
        type = type,
        false_fraction = false_fraction,
        stop_fraction = stop_fraction,
        .format = "{text}"
      )
    }
  }
  
  if (!is.null(warn_count)) {
    if (false_count >= warn_count) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) meets or exceeds the `warn_count` threshold",
        " * `failing_count` ({false_count}) >= `warn_count` ({warn_count})",
        type = type,
        false_count = false_count,
        warn_count = warn_count,
        .format = "{text}"
      )
      
    }
  } else if (!is.null(warn_fraction)) {
    if ((false_count/total_count) >= warn_fraction) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) meets or exceeds the `warn_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `warn_fraction` ({warn_fraction})",
        type = type,
        false_fraction = false_fraction,
        warn_fraction = warn_fraction,
        .format = "{text}"
      )
    }
  }
  
  x_ret
}
