#' Place certain access details more to the fore 
#' @description Allows for a change of the 
#' focus on the table name, database type, and
#' the location of the credentials file.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @param tbl_name the name of the local or remote
#' table.
#' @param file_name the name of a file to be
#' loaded as a table. Valid types are CSV and TSV
#' files.
#' @param col_types if validating a CSV or TSV file,
#' an optional column specification can be provided
#' here as a string. This string representation is
#' where each character represents one column and the
#' mappings are: \code{c} -> character, \code{i} ->
#' integer, \code{n} -> number, \code{d} -> double, 
#' \code{l} -> logical, \code{D} -> date, \code{T} ->
#' date time, \code{t} -> time, \code{?} -> guess, 
#' or \code{_/-}, which skips the column.
#' @param db_type if the table is located in a
#' database, the type of database is required here.
#' Currently, this can be either \code{PostgreSQL}
#' or \code{MySQL}.
#' @param initial_sql when accessing a table in a
#' database (MySQL and PostgreSQL), this provides
#' an option to provide an initial SQL query
#' that is applied to the table before conducting
#' validations. An entire SQL statement can be
#' provided here, or, as a shortcut, the initial
#' \code{SELECT...} statement can be omitted for
#' simple queries that filter the table in focus
#' (e.g., \code{WHERE a > 1 AND b = 'one'}).
#' @param brief an optional, text-based description
#' for the new focus.
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
#' @param db_creds_env_vars if a connection to a
#' database is required for reaching the table
#' specified in \code{tbl_name}, then a set of
#' environment variables can be used to establish
#' that connection. Separate environment variables
#' with the following items should be available:
#' (1) database name (\code{dbname}),
#' (2) the \code{host} name, (3) the \code{port},
#' (4) the username (\code{user}), and (5) the
#' \code{password}. To pass the names of the
#' environment variables to the \code{agent}
#' object, one can use the \code{db_creds_env_vars()}
#' function directly.
#' @return an agent object.
#' @examples
#' # Create a simple data frame with a column
#' # of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 4, 3, 5, 1, 2))
#' 
#' # Validate that values in column `a` are
#' # always less than 6; this requires the
#' # use of `create_agent()` (to begin the 
#' # validation process) and `focus_on()`
#' # to point to the table that will undergo
#' # validation in subsequent steps
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_lt(
#'     column = a,
#'     value = 6) %>%
#'   interrogate()
#' 
#' # Determine if this column validation has
#' # passed by using `all_passed()`
#' all_passed(agent)
#' #> [1] TRUE
#' @importFrom dplyr filter bind_rows group_by filter ungroup collect row_number
#' @importFrom readr read_csv read_tsv
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble glimpse
#' @importFrom utils capture.output
#' @export
focus_on <- function(agent,
                     tbl_name = NULL,
                     file_name = NULL,
                     col_types = NULL,
                     db_type = NULL,
                     initial_sql = NULL,
                     brief = NULL,
                     creds_file = NULL,
                     db_creds_env_vars = NULL) {
  
  if (is.null(tbl_name) & is.null(file_name)) {
    stop("A table name or a file name must be provided.")
  }
  
  if (is.null(tbl_name)) {
    agent$focal_tbl_name <- as.character(NA)
  } else if (!is.null(tbl_name)) {
    agent$focal_tbl_name <- tbl_name
  }
  
  if (is.null(file_name)) {
    agent$focal_file_name <- as.character(NA)
  } else if (!is.null(file_name)) {
    agent$focal_file_name <- file_name
  }
  
  if (is.null(col_types)) {
    agent$focal_col_types <- as.character(NA)
  } else if (!is.null(col_types)) {
    agent$focal_col_types <- col_types
  }
  
  # If a `tbl_name` provided without a `file_name`
  # or a `db_type`, then assume a local data frame 
  # (`local_df`) is the focus; if a `file_name`
  # provided, assume a local file (`local_file`)
  # is the focus
  if (is.null(file_name) & is.null(db_type) & !is.null(tbl_name)) {
    agent$focal_db_type <- "local_df"
  } else if (!is.null(db_type)) {
    if (db_type %in% c("PostgreSQL", "MySQL")) {
      agent$focal_db_type <- db_type
    }
  } else if (!is.null(file_name)) {
    agent$focal_db_type <- "local_file"
  }
  
  if (is.null(creds_file)) {
    agent$focal_db_cred_file_path <- as.character(NA)
  } else if (!is.null(creds_file)) {
    agent$focal_db_cred_file_path <- creds_file
  }
  
  if (!is.null(db_creds_env_vars)) {
    if (inherits(db_creds_env_vars, "list")) {
      
      agent$focal_db_env_vars <- db_creds_env_vars
    }
  }
  
  if (is.null(initial_sql)) {
    agent$focal_init_sql <- as.character(NA)
  } else if (!is.null(initial_sql)) {
    agent$focal_init_sql <- initial_sql
  }
  
  # Make the table accessible to obtain basic
  # information from it
  if (agent$focal_db_type == "local_df") {
    
    # Create `table` object as the direct reference to a
    # local `data.frame` or `tbl_df` object
    table <- get(tbl_name)
    
    agent$focal_col_names <- colnames(table)
    
  } else if (agent$focal_db_type == "local_file") {
    
    # Infer the file type from the extension
    file_extension <- 
      (agent$focal_file_name %>% 
         basename() %>% 
         stringr::str_split(pattern = "\\.") %>% 
         unlist())[2] %>% 
      tolower()
    
    # Get the file name without the extension
    file_name_no_ext <- 
      (agent$focal_file_name %>% 
         basename() %>% 
         stringr::str_split(pattern = "\\.") %>% 
         unlist())[1] %>% 
      tolower()
    
    if (file_extension == "csv") {
      table <-
        suppressMessages(readr::read_csv(agent$focal_file_name, n_max = 1000))
    } else if (file_extension == "tsv") {
      table <-
        suppressMessages(readr::read_tsv(agent$focal_file_name, n_max = 1000))
    }
    
    if (is.na(agent$focal_tbl_name)) {
      agent$focal_tbl_name <- file_name_no_ext
    }
    
    agent$focal_col_names <- colnames(table)
    
  } else if (agent$focal_db_type == "PostgreSQL") {
    
    # Create `table` object as an SQL entry point
    # for a remote PostgreSQL table
    
    if (!is.null(creds_file)) {
      
      table <- 
        set_entry_point(
          table = tbl_name,
          db_type = db_type,
          creds_file = creds_file)
      
    } else if (!is.null(db_creds_env_vars)) {
      
      table <-
        set_entry_point(
          table = tbl_name,
          db_type = db_type,
          db_creds_env_vars = db_creds_env_vars)
    }
    
    # Get the column names from the table
    # captured <- utils::capture.output({
    #   table_colnames <- tibble::glimpse(table) %>% names() })
    agent$focal_col_names <-  
      table %>%
      dplyr::group_by() %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      sapply(class) %>%
      lapply(`[[`, 1) %>%
      names()
    
  } else if (agent$focal_db_type == "MySQL") {
    
    # Create `table` object as an SQL entry point
    # for a remote MySQL table
    if (!is.null(creds_file)) {
      
      table <- 
        set_entry_point(
          table = tbl_name,
          db_type = db_type,
          creds_file = creds_file)
      
    } else if (!is.null(db_creds_env_vars)) {
      
      table <-
        set_entry_point(
          table = tbl_name,
          db_type = db_type,
          db_creds_env_vars = db_creds_env_vars)
    }
    
    agent$focal_col_names <-  
      table %>%
      dplyr::group_by() %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      sapply(class) %>%
      lapply(`[[`, 1) %>%
      names()
  }
  
  
  # If no `brief` provided, autogenerate one
  if (is.null(brief)) {
    
    brief <-
      paste0(
        "Focus on table `",
        tbl_name, "` (",
        agent$focal_db_type, ")")
  }
  
  # Place the validation step in the logical plan
  agent$logical_plan <-
    dplyr::bind_rows(
      agent$logical_plan,
      tibble::tibble(
        component_name = "focus_on",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
