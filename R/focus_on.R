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
#' @param description an optional, text-based
#' description for the validation step. Used primarily
#' in the Logical Plan section of the report generated
#' by the \code{html_summary} function.
#' @return an agent object.
#' @importFrom dplyr filter
#' @importFrom readr read_csv read_tsv
#' @importFrom tibble as_tibble
#' @export focus_on

focus_on <- function(agent,
                     tbl_name = NULL,
                     file_name = NULL,
                     col_types = NULL,
                     db_type = NULL,
                     creds_file = NULL,
                     initial_sql = NULL,
                     description = NULL) {
  
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
    
  } else if (agent$focal_db_type == "local_file") {
    
    # Infer the file type from the extension
    file_extension <- 
      (agent$focal_file_name %>% 
         basename() %>% 
         str_split(pattern = "\\.") %>% 
         unlist())[2] %>% 
      tolower()
    
    # Get the file name without the extension
    file_name_no_ext <- 
      (agent$focal_file_name %>% 
         basename() %>% 
         str_split(pattern = "\\.") %>% 
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
    
  } else if (agent$focal_db_type == "PostgreSQL") {
    
    # Create `table` object as an SQL entry point
    # for a remote PostgreSQL table
    table <- 
      set_entry_point(
        table = tbl_name,
        db_type = db_type,
        creds_file = creds_file)
    
  } else if (agent$focal_db_type == "MySQL") {
    
    # Create `table` object as an SQL entry point
    # for a remote MySQL table
    table <- 
      set_entry_point(
        table = tbl_name,
        db_type = db_type,
        creds_file = creds_file) 
  }
  
  # Get the column names from the table
  agent$focal_col_names <-
    table %>%
    dplyr::filter(row_number() == 1) %>%
    tibble::as_tibble() %>%
    names()
  
  # If no `description` provided, set as `NA`
  if (is.null(description)) {
    description <- as.character(NA)
  }
  
  # Place the validation step in the logical plan
  agent$logical_plan <-
    bind_rows(
      agent$logical_plan,
      tibble::tibble(
        component_name = "focus_on",
        parameters = as.character(NA),
        description = description))
  
  return(agent)
}