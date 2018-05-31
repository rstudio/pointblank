# Add properly formatted validation steps
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_df
#' @importFrom dplyr select bind_rows
create_validation_step <- function(agent,
                                   assertion_type,
                                   column,
                                   value = NULL,
                                   set = NULL,
                                   regex = NULL,
                                   incl_na = NULL,
                                   incl_nan = NULL,
                                   preconditions = NULL,
                                   brief = NULL,
                                   warn_count = NULL,
                                   notify_count = NULL,
                                   warn_fraction = NULL,
                                   notify_fraction = NULL,
                                   tbl_name = as.character(NA),
                                   db_type = as.character(NA),
                                   creds_file = as.character(NA),
                                   init_sql = as.character(NA),
                                   file_path = as.character(NA),
                                   col_types = as.character(NA)) {
  
  # Bind variable
  x <- NULL
  
  # Create a validation step as a single-row
  # `tbl_df` object
  validation_step_df <-
    tibble::tibble(
      tbl_name = as.character(agent$focal_tbl_name),
      db_type = as.character(agent$focal_db_type),
      assertion_type = assertion_type,
      column = as.character(column),
      value = ifelse(is.null(value), as.numeric(NA), as.numeric(value)),
      regex = ifelse(is.null(regex), as.character(NA), as.character(regex)),
      brief = ifelse(is.null(brief), as.character(NA), as.character(brief)),
      all_passed = as.logical(NA),
      warn_count = ifelse(is.null(warn_count), as.numeric(NA), as.numeric(warn_count)),
      notify_count = ifelse(is.null(notify_count), as.numeric(NA), as.numeric(notify_count)),
      warn_fraction = ifelse(is.null(warn_fraction), as.numeric(NA), as.numeric(warn_fraction)),
      notify_fraction = ifelse(is.null(notify_fraction), as.numeric(NA), as.numeric(notify_fraction)),
      init_sql = as.character(agent$focal_init_sql),
      db_cred_file_path = as.character(agent$focal_db_cred_file_path),
      file_path = as.character(agent$focal_file_name),
      col_types = as.character(agent$focal_col_types))
  
  # If just `tbl_name` provided, assume it is
  # a local data frame
  if (!is.na(tbl_name)) {
    validation_step_df$tbl_name <- tbl_name
  }
  
  if (!is.na(db_type)) {
    validation_step_df$db_type <- db_type
  }
  
  if (!is.na(creds_file)) {
    validation_step_df$db_cred_file_path <- creds_file
  }
  
  if (!is.na(init_sql)) {
    validation_step_df$init_sql <- init_sql
  }
  
  if (!is.na(file_path)) {
    validation_step_df$file_path <- file_path
  }
  
  if (!is.na(col_types)) {
    validation_step_df$col_types <- col_types
  }
  
  # If a set has been provided as a vector, include
  # these values as a `df_tbl` object
  if (!is.null(set)) {
    
    set_df <-
      1:nrow(validation_step_df) %>%
      purrr::map_df(
        function(x) {
          tibble::tibble(
            x = x,
            set = paste(set, collapse = ","),
            class = class(set),
            incl_na = ifelse(is.null(incl_na), FALSE, incl_na),
            incl_nan = ifelse(is.null(incl_nan), FALSE, incl_nan))
          }) %>%
      dplyr::select(-x)
    
  } else {
    
    set_df <-
      1:nrow(validation_step_df) %>%
      purrr::map_df(
        function(x) {
          tibble::tibble(
            x = x,
            set = as.character(NA),
            class = as.character(NA),
            incl_na = FALSE,
            incl_nan = FALSE)
          }) %>%
      dplyr::select(-x)
  }
  
  # If preconditions have been provided as a vector, include
  # these values as a `df_tbl` object
  if (!is.null(preconditions)) {
    
    preconditions_df <-
      1:nrow(validation_step_df) %>%
      purrr::map_df(
        function(x) {
          tibble::tibble(
            x = x,
            precondition = paste(preconditions, collapse = ";"))}) %>%
      dplyr::select(-x)
    
  } else {
    
    preconditions_df <-
      1:nrow(validation_step_df) %>%
      purrr::map_df(
        function(x) {
          tibble::tibble(
            x = x,
            precondition = as.character(NA))}) %>%
      dplyr::select(-x)
  }
  
  # Append `validation_step` to `validation_set`
  agent$validation_set <-
    dplyr::bind_rows(
      agent$validation_set,
      validation_step_df)
  
  # Append `sets`
  agent$sets <-
    dplyr::bind_rows(
      agent$sets,
      set_df)
  
  # Append `preconditions`
  agent$preconditions <-
    dplyr::bind_rows(
      agent$preconditions,
      preconditions_df)
  
  agent
}

# Acquire information on the coordinates
# of a remote table; if a table is remote
# (i.e., in a database), this function
# will be invoked to set an entry point
# for the interrogation query.
#' @importFrom dplyr tbl sql
#' @importFrom DBI dbConnect
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom RMySQL dbConnect MySQL
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
            password = credentials[5])
        
      } else if (!is.null(db_creds_env_vars)) {
        
        # Establish the connection with the environment variables
        connection <-
          DBI::dbConnect(
            RPostgreSQL::PostgreSQL(max.con = 512),
            dbname = Sys.getenv(db_creds_env_vars[[1]]),
            host = Sys.getenv(db_creds_env_vars[[2]]),
            port = Sys.getenv(db_creds_env_vars[[3]]),
            user = Sys.getenv(db_creds_env_vars[[4]]),
            password = Sys.getenv(db_creds_env_vars[[5]]))
        
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
          gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
               initial_sql, perl = TRUE)
          
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
          RMySQL::dbConnect(
            RMySQL::MySQL(),
            dbname = credentials[1],
            host = credentials[2],
            port = as.integer(credentials[3]),
            user = credentials[4],
            password = credentials[5])
      
      } else if (!is.null(db_creds_env_vars)) {
        
        # Establish the connection with the environment variables
        connection <-
          RMySQL::dbConnect(
            RMySQL::MySQL(),
            dbname = Sys.getenv(db_creds_env_vars[[1]]),
            host = Sys.getenv(db_creds_env_vars[[2]]),
            port = as.integer(Sys.getenv(db_creds_env_vars[[3]])),
            user = Sys.getenv(db_creds_env_vars[[4]]),
            password = Sys.getenv(db_creds_env_vars[[5]]))
        
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
            sql(paste0(
              "SELECT * FROM ", table, " ", initial_sql)))
      }
    }
  }
  
  tbl_entry
}

# Get all column names from the table
# currently in focus
get_all_cols <- function(agent) {
  
  # Get vector of all columns
  # table currently in focus
  agent$focal_col_names
}

# Determine the course of action for a
# given verification step. Based on a recent
# judgment, what actions are taken now?
#' @importFrom tibble tibble
determine_action <- function(n,
                             false_count,
                             warn_count,
                             notify_count,
                             warn_fraction,
                             notify_fraction) {
  
  if (is.na(warn_count)) {
    warn <- FALSE
  } else {
    if (false_count >= warn_count) {
      warn <- TRUE
    } else {
      warn <- FALSE
    }
  }
  
  if (is.na(notify_count)) {
    notify <- FALSE
  } else {
    if (false_count >= notify_count) {
      notify <- TRUE
    } else {
      notify <- FALSE
    }
  }
  
  if (!is.na(warn_fraction)) {
    
    warn_count <- round(warn_fraction * n, 0)
    
    if (false_count >= warn_count) {
      warn <- TRUE
    } else {
      warn <- FALSE
    }
  }
  
  if (!is.na(notify_fraction)) {
    
    notify_count <- round(notify_fraction * n, 0)
    
    if (false_count >= notify_count) {
      notify <- TRUE
    } else {
      notify <- FALSE
    }
  }
  
  # Generate a tbl with action information
  tibble::tibble(
    warn = warn,
    notify = notify)
}

# Generate summary SVG files for the results of a
# validation pipeline
#' @importFrom stringr str_replace str_replace_all
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
        flag = " ", width = 12) %>%
      stringr::str_replace_all(" ", "&#160;")
    
    fail <- 
      formatC(
        x = (summary$n[i] - summary$n_passed[i]) %>% as.integer(),
        flag = " ", width = 12) %>% 
      stringr::str_replace_all(" ", "&#160;")
    
    if (summary$notify[i] == TRUE) {
      outline_color <- "#B20000"
    } else if (summary$notify[i] == FALSE &
               summary$warn[i] == TRUE) {
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
      to = paste0("./temporary_images/",
                  stringr::str_replace_all(index, " ", "0"), ".svg"),
      overwrite = TRUE)
    
    # Modify the summary numbers
    modified_svg <-
      readLines(
        paste0("./temporary_images/",
               stringr::str_replace_all(index, " ", "0"),
               ".svg"),
        warn = FALSE) %>%
      stringr::str_replace(">XXXX<", paste0(">", index, "<")) %>%
      stringr::str_replace(">PPPPPPPPPPPP<", paste0(">", pass, "<")) %>%
      stringr::str_replace(">FFFFFFFFFFFF<", paste0(">", fail, "<"))
    
    # Modify the outline color
    modified_svg <-
      modified_svg %>%
      stringr::str_replace(
        "(\"function.*? stroke=\")#979797",
        paste0("\\1", outline_color))
    
    # Write the modified SVG file to disk
    modified_svg %>%
      cat(
        file = paste0(
          "./temporary_images/",
          str_replace_all(index, " ", "0"),
          "_.svg"))
  }
}

# Generate SVG files for the plan of a validation pipeline
#' @importFrom stringr str_replace_all
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
      to = paste0("./temporary_images_plan/",
                  stringr::str_replace_all(index, " ", "0"), ".svg"),
      overwrite = TRUE)
  }
}

# Does the agent have no validation steps
# available in the object?
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

# Did the agent carry out an interrogation?
did_agent_interrogate <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    return(ifelse(length(agent$validation_time) > 0, TRUE, FALSE))
  } else {
    return(NA)
  }
}

# When did the agent carry out an interrogation?
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

# How many validation steps are associated
# with the agent?
number_of_validation_steps <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    return(agent$validation_set %>% nrow())
  } else {
    return(NA)
  }
}

# How many validation steps are associated
# with the agent?
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
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
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
           paste0("when ", "`", preconditions, "`, "),
           paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should be ", operator, " ", value)
  }
  
  
  if (assertion_type == "col_exists") {
    
    autobrief <-
      paste0(
        "Expect that column `", column, "` exists")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0("when ", "`", preconditions, "`, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_in_set", "not ", ""),
        "be part of set `", paste(set, collapse = ", "), "`")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0("when ", "`", preconditions, "`, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_in_set", "not ", ""),
        "be part of set `", paste(set, collapse = ", "), "`")
  }
  
  if (assertion_type %in%
      c("col_vals_between", "col_vals_not_between")) {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0("when ", "`", preconditions, "`, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_between", "not ", ""),
        "be between `", left, "` and `", right, "`")
  }
  
  if (assertion_type == "col_vals_regex") {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0("when ", "`", preconditions, "`, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should match the regex expression `",
        regex, "`")
  }
  
  if (assertion_type %in% c("col_vals_null", "col_vals_not_null")) {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0("when ", "`", preconditions, "`, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_null", "not ", ""),
        "be NULL")
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
      paste0(
        "Expect that column `", column,
        "` is `",
        col_type,
        "`-based")
  }
  
  if (assertion_type == "rows_not_duplicated") {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0("when ", "`", preconditions, "`, "),
          paste0("")),
        "rows from `",
        column, "` ",
        "have no duplicates")
  }
  
  autobrief
}

# Perform a single column validation that
# returns a vector of logical values
#' @importFrom dplyr pull collect as_tibble
evaluate_single <- function(object,
                            type,
                            column,
                            value = NULL,
                            set = NULL,
                            regex = NULL,
                            left = NULL,
                            right = NULL,
                            incl_na = NULL,
                            incl_nan = NULL,
                            warn_count,
                            notify_count,
                            warn_fraction,
                            notify_fraction) {
  
  # Get the `column` number
  col_number <- ((object %>% colnames()) %in% column) %>% which()
  
  if (type == "col_vals_equal") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) == value
  }
  
  if (type == "col_vals_not_equal") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) != value
  }
  
  if (type == "col_vals_gt") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) > value
  }
  
  if (type == "col_vals_gte") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) >= value
  }
  
  if (type == "col_vals_lt") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) < value
  }
  
  if (type == "col_vals_lte") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) <= value
  }
  
  if (type == "col_vals_between") {
    
    vals <- 
      object %>%
      dplyr::pull(col_number)
    
    logicals <- 
      vals >= left &
      vals <= right
    
    if (incl_na == TRUE) {
      logicals[which(is.na(logicals))] <- TRUE
    } else if (incl_na == FALSE) {
      logicals[which(is.na(logicals))] <- FALSE
    }
    
    if (incl_nan == TRUE) {
      logicals[which(is.nan(logicals))] <- TRUE
    } else if (incl_nan == FALSE) {
      logicals[which(is.nan(logicals))] <- FALSE
    }
  }
  
  if (type == "col_vals_not_between") {
    
    vals <- 
      object %>%
      dplyr::pull(col_number)
    
    logicals <- 
      vals < left |
      vals > right
    
    if (incl_na == TRUE) {
      logicals[which(is.na(logicals))] <- TRUE
    } else if (incl_na == FALSE) {
      logicals[which(is.na(logicals))] <- FALSE
    }
    
    if (incl_nan == TRUE) {
      logicals[which(is.nan(logicals))] <- TRUE
    } else if (incl_nan == FALSE) {
      logicals[which(is.nan(logicals))] <- FALSE
    }
  }
  
  if (type == "col_vals_in_set") {
    
    logicals <- 
      object %>%
      dplyr::pull(col_number) %in% set
  }
  
  if (type == "col_vals_not_in_set") {
    
    logicals <- 
      !(object %>%
          dplyr::pull(col_number) %in% set)
  }
  
  if (type == "col_vals_regex") {
    
    vals <- 
      object %>%
      dplyr::pull(col_number)
    
    logicals <- 
      grepl(pattern = regex, x = vals)
  }
  
  if (type == "col_vals_not_null") {
    
    logicals <- 
      !is.na(object %>%
               dplyr::pull(col_number))
  }
  
  if (type == "col_vals_null") {
    
    logicals <- 
      is.na(object %>%
              dplyr::pull(col_number))
  }
  
  if (grepl("col_is_.*", type)) {
    
    # Get the column type
    column_type <-
      (object %>%
         dplyr::select(column) %>%
         head(1) %>%
         dplyr::collect() %>%
         as.data.frame(stringsAsFactors = FALSE))[1, 1] %>% 
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
  
  if (type == "col_exists") {
    
    column_names <-
      object %>%
      head(1) %>%
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
        "The validation (`{type}()`) is above the `notify_count` threshold",
        " * `failing_count` ({false_count}) > `notify_count` ({notify_count})",
        type = type,
        false_count = false_count,
        notify_count = notify_count,
        .format = "ERROR {text}")
    }
  } else if (!is.null(notify_fraction)) {
    if ((false_count/total_count) >= notify_fraction) {
      
      messaging::emit_error(
        "The validation (`{type}()`) is above the `notify_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) > `notify_fraction` ({notify_fraction})",
        type = type,
        false_fraction = false_fraction,
        notify_fraction = notify_fraction,
        .format = "ERROR {text}")
    }
  }
  
  if (!is.null(warn_count)) {
    if (false_count >= warn_count) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) is above the `warn_count` threshold",
        " * `failing_count` ({false_count}) > `warn_count` ({warn_count})",
        type = type,
        false_count = false_count,
        warn_count = warn_count,
        .format = "WARN {text}")

    }
  } else if (!is.null(warn_fraction)) {
    if ((false_count/total_count) >= warn_fraction) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) is above the `warn_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) > `warn_fraction` ({warn_fraction})",
        type = type,
        false_fraction = false_fraction,
        warn_fraction = warn_fraction,
        .format = "WARN {text}")
    }
  }
  
  logicals
}
