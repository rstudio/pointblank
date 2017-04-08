
#' Create a properly formatted validation step.
#' Get a validation step as a tbl row.
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr nest_
create_validation_step <- function(agent,
                                   assertion_type,
                                   column,
                                   value = NULL,
                                   set = NULL,
                                   regex = NULL,
                                   preconditions = NULL,
                                   report_count,
                                   warn_count,
                                   notify_count,
                                   tbl_name = as.character(NA),
                                   db_type = as.character(NA),
                                   creds_file = as.character(NA),
                                   init_sql = as.character(NA),
                                   file_path = as.character(NA),
                                   col_types = as.character(NA)) {
  
  # Create a validation step as a single-row
  # `tbl_df` object
  validation_step <-
    tibble::tibble(
      tbl_name = as.character(agent$focal_tbl_name),
      db_type = as.character(agent$focal_db_type),
      assertion_type = assertion_type,
      column = as.character(column),
      value = ifelse(is.null(value), as.numeric(NA), as.numeric(value)),
      set = as.numeric(NA),
      regex = ifelse(is.null(regex), as.character(NA), as.character(regex)),
      preconditions = as.numeric(NA),
      all_passed = as.logical(NA),
      report_count = as.numeric(report_count),
      warn_count = as.numeric(warn_count),
      notify_count = as.numeric(notify_count),
      init_sql = as.character(agent$focal_init_sql),
      db_cred_file_path = as.character(agent$focal_db_cred_file_path),
      file_path = as.character(agent$focal_file_name),
      col_types = as.character(agent$focal_col_types))
  
  # If a set has been provided as a vector, include
  # these values as a nested `df_tbl` object in the
  # `set` column
  if (!is.null(set)) {
    for (i in 1:nrow(validation_step)) {
      validation_step$set[i] <- 
        set %>% 
        tibble::as_tibble() %>%
        tidyr::nest_(key_col = "set", nest_cols = names(.))
    }
  }
  
  # If one or more preconditions have been provided
  # as a vector, include these values as a nested `df_tbl`
  # object in the `preconditions` column
  if (!is.null(preconditions)) {
    for (i in 1:nrow(validation_step)) {
      validation_step$preconditions[i] <- 
        preconditions %>% 
        tibble::as_tibble() %>%
        tidyr::nest_(key_col = "preconditions", nest_cols = names(.))
    }
  }
  
  # If just `tbl_name` provided, assume it is
  # a local data frame
  if (!is.na(tbl_name)) {
    validation_step$tbl_name <- tbl_name
  }
  
  if (!is.na(db_type)) {
    validation_step$db_type <- db_type
  }
  
  if (!is.na(creds_file)) {
    validation_step$db_cred_file_path <- creds_file
  }
  
  if (!is.na(init_sql)) {
    validation_step$init_sql <- init_sql
  }
  
  if (!is.na(file_path)) {
    validation_step$file_path <- file_path
  }
  
  if (!is.na(col_types)) {
    validation_step$col_types <- col_types
  }
  
  return(validation_step)
}


#' Acquire information on the coordinates of
#' a remote table. If a table is remote (i.e.,
#' in a database), this function will be
#' invoked to set an entry point for the
#' interrogation query.
#' @param table the table with which an entry point
#' is required.
#' @param db_type if the table is located in a
#' database, the type of database is required here.
#' Currently, this can be either \code{PostgreSQL}
#' or \code{MySQL}.
#' @param creds_file if a connection to a database
#' is required for reaching the table specified in
#' \code{tbl_name}, then a path to a credentials file
#' can be used to establish that connection. The
#' credentials file is an \code{RDS} containing a
#' character vector with the following items in the
#' specified order: (1) database name (\code{dbname}),
#' (2) the \code{host} name, (3) the \code{port},
#' (4) the username (\code{user}), and (5) the
#' \code{password}. This file can be easily created
#' using the \code{create_creds_file()} function.
#' @param initial_sql when accessing a remote table,
#' this provides an option to provide an initial
#' query component before conducting validations. 
#' An entire SQL statement can be provided here, or,
#' as a shortcut, the initial \code{SELECT...}
#' statement can be omitted for simple queries (e.g.,
#' \code{WHERE a > 1 AND b = 'one'}).
#' @importFrom DBI dbListConnections dbListResults dbClearResult dbDisconnect
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom dplyr src_postgres src_mysql tbl sql
set_entry_point <- function(table,
                            db_type = NULL,
                            creds_file = NULL,
                            initial_sql = NULL) {
  
  if (is.null(db_type) & inherits(table, "data.frame")) {
    
    # Create table entry object
    tbl_entry <- table
  }
  
  if (!is.null(db_type)) {
    
    if (db_type == "PostgreSQL") {
      
      # Establish a new PostgreSQL connection
      if (!is.null(creds_file)) {
        
        # Disconnect any existing PostgreSQL connections
        disconnect_postgres()
        
        # Serialize the credentials RDS file
        credentials <- readRDS(creds_file)
        
        # Establish the connection with the serialized RDS object
        connection <-
          dplyr::src_postgres(
            dbname = credentials[1],
            host = credentials[2],
            port = credentials[3],
            user = credentials[4],
            password = credentials[5])
      } else if (is.null(creds_file)) {
        stop("A credentials RDS file is required.")
      }
      
      if (is.null(initial_sql)) {
        # Create table entry object
        tbl_entry <- dplyr::tbl(src = connection, table)
      }
      
      if (!is.null(initial_sql)) {
        
        if (grepl("^(SELECT|select)", initial_sql)) {
          
          # If there is a `SELECT` or `select` keyword
          # in the `initial_sql` statement, provide
          # the entire SQL statement to dplyr::tbl
          # without changing the content
          tbl_entry <- 
            dplyr::tbl(src = connection, sql(initial_sql))
          
        } else {
          
          # If there is no `SELECT` or `select` lead in,
          # insert that line at the beginning with the
          # table name then carry on with the rest of the
          # statement
          tbl_entry <- 
            dplyr::tbl(
              src = connection,
              sql(paste0(
                "SELECT * FROM ", table, " ", initial_sql)))
        }
      }
    } else if (db_type == "MySQL") {
      
      # Establish a new MySQL connection
      if (!is.null(creds_file)) {
        
        # Serialize the credentials RDS file
        credentials <- readRDS(creds_file)
        
        # Establish the connection with the serialized RDS object
        connection <-
          dplyr::src_mysql(
            dbname = credentials[1],
            host = credentials[2],
            port = as.integer(credentials[3]),
            user = credentials[4],
            password = credentials[5])
      } 
      
      else if (is.null(creds_file)) {
        stop("A credentials RDS file is required.")
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
            sql(paste0(
              "SELECT * FROM ", table, " ", initial_sql)))
      }
    }
  }
  
  invisible(tbl_entry)
}


#' With any `all_cols()` call, return a wildcard operator
#' @export all_cols
all_cols <- function() {
  return("*")
}

#' Get all column names from the table currently in focus
#' @param agent an agent object of class
#' \code{ptblank_agent}.
get_all_cols <- function(agent) {
  
  # Get vector of all columns
  # table currently in focus
  col_names <- agent$focal_col_names
  return(col_names)
}


#' Determine the course of action for a
#' given verification step. Based on a recent
#' judgment, what actions are taken now?
#' @importFrom tibble tibble
determine_action <- function(false_count,
                             report_count,
                             warn_count,
                             notify_count) {
  
  if (false_count >= report_count) {
    report <- TRUE
  } else {
    report <- FALSE
  }
  
  if (false_count >= warn_count) {
    warn <- TRUE
  } else {
    warn <- FALSE
  }
  
  if (false_count >= notify_count) {
    notify <- TRUE
  } else {
    notify <- FALSE
  }
  
  # Generate a tbl with action information
  action_df <-
    tibble::tibble(
      report = report,
      warn = warn,
      notify = notify)
  
  return(action_df)
}

#' Disconnect from any open PostgreSQL connections
#' @importFrom DBI dbListConnections dbListResults dbClearResult dbDisconnect
#' @importFrom RPostgreSQL PostgreSQL
disconnect_postgres <- function() {
  
  # Get list of all open PostgreSQL connections
  cons <- DBI::dbListConnections(RPostgreSQL::PostgreSQL())
  
  # Iterate through all open PostgreSQL connections and disconnect
  for (con in cons) {
    if (length(DBI::dbListResults(con)) != 0) {
      DBI::dbClearResult(DBI::dbListResults(con)[[1]])}
    DBI::dbDisconnect(con)
  }
}

#' Add function to generate summary SVG files
#' @importFrom stringr str_replace str_replace_all
generate_img_files <- function(summary) {
  
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
        x = summary$n_passed[i],
        flag = " ", width = 12) %>%
      stringr::str_replace_all(" ", "&#160;")
    
    fail <- 
      formatC(
        x = summary$n[i] - summary$n_passed[i],
        flag = " ", width = 12) %>%
      stringr::str_replace_all(" ", "&#160;")
    
    icon <- paste0(summary$assertion_type[i], "_text.svg")
    
    file.copy(
      from = system.file("icons", icon, package = "pointblank"),
      to = paste0("./temporary_images/",
                  stringr::str_replace_all(index, " ", "0"), ".svg"),
      overwrite = TRUE)
    
    readLines(paste0("./temporary_images/",
                     stringr::str_replace_all(index, " ", "0"), ".svg"),
              warn = FALSE) %>%
      stringr::str_replace(">XXXX<", paste0(">", index, "<")) %>%
      stringr::str_replace(">PPPPPPPPPPPP<", paste0(">", pass, "<")) %>%
      stringr::str_replace(">FFFFFFFFFFFF<", paste0(">", fail, "<")) %>%
      cat(file = paste0("./temporary_images/", str_replace_all(index, " ", "0"), "_.svg"))
  }
}
