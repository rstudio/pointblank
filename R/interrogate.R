#' Given an agent that is fully loaded with
#' tasks, perform an interrogation
#' @description The agent has all the
#' information on what to do, so now all
#' interrogations can proceed efficiently,
#' and, according to plan.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @param get_problem_rows an option to 
#' collect rows that didn't pass a
#' particular validation step. The default
#' is \code{TRUE} and further options
#' allow for fine control of how these
#' rows are collected.
#' @param get_first_n if the option to
#' collect non-passing rows is chosen,
#' there is the option here to collect
#' the first \code{n} rows here. Supply
#' the number of rows to extract from
#' the top of the non-passing rows table
#' (the ordering of data from the
#' original table is retained).
#' @param sample_n if the option to
#' collect non-passing rows is chosen,
#' this option allows for the sampling
#' of \code{n} rows. Supply the number
#' of rows to sample from the non-passing
#' rows table. If \code{n} is greater
#' than the number of non-passing rows,
#' then all the rows will be returned.
#' @param sample_frac if the option to
#' collect non-passing rows is chosen,
#' this option allows for the sampling
#' of a fraction of those rows. Provide a
#' number in the range of \code{0} and
#' \code{1}. The number of rows to return
#' may be extremely large (and this is
#' especially when querying remote
#' databases), however, the
#' \code{sample_limit} option will apply
#' a hard limit to the returned rows.
#' @param sample_limit a value that
#' limits the possible number of rows
#' returned when sampling non-passing
#' rows using the \code{sample_frac}
#' option.
#' @return an agent object.
#' @examples 
#' # Create 2 simple data frames
#' # with 2 columns of numerical
#' # values in each
#' df_1 <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3))
#'     
#' df_2 <-
#'   data.frame(
#'     c = c(8, 8, 8, 6, 1, 3),
#'     d = c(9, 8, 7, 2, 3, 3))
#' 
#' # Validate that values in column
#' # `a` from `df_1` are always >= 5,
#' # and also validate that, in `df_2`,
#' # values in `c` are always == 8
#' # when values in `d` are >= 5  
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df_1") %>%
#'   col_vals_gte(
#'     column = a,
#'     value = 5) %>%
#'   focus_on(tbl_name = "df_2") %>%
#'   col_vals_equal(
#'     column = c,
#'     value = 8,
#'     preconditions = d >= 5) %>%
#'   interrogate()
#'   
#' # Get a basic summary with
#' # `get_interrogation_summary()`
#' get_interrogation_summary(agent)[, 1:7]
#' #> # A tibble: 2 x 7
#' #>   tbl_name  db_type assertion_type column value regex all_passed
#' #>      <chr>    <chr>          <chr>  <chr> <dbl> <chr>      <lgl>
#' #> 1     df_1 local_df   col_vals_gte      a     5  <NA>       TRUE
#' #> 2     df_2 local_df col_vals_equal      c     8  <NA>       TRUE
#' @importFrom rlang sym UQ
#' @importFrom dplyr group_by group_by_ mutate mutate_ filter filter_ select
#' @importFrom dplyr select_ collect ungroup summarize row_number n sample_n
#' @importFrom dplyr sample_frac everything case_when as_tibble
#' @importFrom tidyr nest_
#' @importFrom stringr str_split str_trim
#' @importFrom purrr flatten_chr flatten_dbl
#' @importFrom readr read_csv read_tsv
#' @importFrom httr POST add_headers
#' @importFrom glue glue
#' @importFrom stats setNames
#' @importFrom utils head URLencode
#' @export
interrogate <- function(agent,
                        get_problem_rows = TRUE,
                        get_first_n = NULL,
                        sample_n = NULL,
                        sample_frac = NULL,
                        sample_limit = 5000) {
  
  # Create bindings for global variables
  notify <- warn <- NULL
  
  # Get the starting time for the interrogation
  interrogation_start_time <- Sys.time()
  
  # Add the starting time to the `agent` object
  agent$validation_time <- interrogation_start_time
  
  # Create bindings for variables
  pb_is_good_ <- set <- pb_step_ <- NULL
  
  # Get number of rows in `validation_set`
  n_validations <- nrow(agent$validation_set)
  
  for (i in 1:n_validations) {
    
    # Get the starting time for the validation step
    validation_start_time <- Sys.time()
    
    if (agent$validation_set$db_type[i] == "local_df") {
      
      # Create `table` object as the direct reference to a
      # local `data.frame` or `tbl_df` object
      table <- get(agent$validation_set$tbl_name[i])
      
    } else if (agent$validation_set$db_type[i] == "local_file") {
      
      file_path <- agent$validation_set$file_path[i]
      col_types <- agent$validation_set$col_types[i]
      
      # Infer the file type from the extension
      file_extension <- 
        (agent$validation_set$file_path[i] %>% 
           basename() %>% 
           stringr::str_split(pattern = "\\.") %>% 
           unlist())[2] %>% 
        tolower()
      
      if (is.na(col_types)) {
        
        if (file_extension == "csv") {
          
          table <- 
            suppressMessages(
              readr::read_csv(
                file = file_path))
          
        } else if (file_extension == "tsv") {
          
          table <- 
            suppressMessages(
              readr::read_tsv(
                file = file_path))
        }
      }
      
      if (!is.na(col_types)) {
        
        if (file_extension == "csv") {
          
          table <- 
            suppressMessages(
              readr::read_csv(
                file = file_path,
                col_types = col_types))
          
        } else if (file_extension == "tsv") {
          
          table <- 
            suppressMessages(
              readr::read_tsv(
                file = file_path,
                col_types = col_types))
        }
      }
    } else if (agent$validation_set$db_type[i] %in% c("PostgreSQL", "MySQL")) {
      
      # Determine if there is an initial SQL
      # statement available as part of the 
      # table focus (`focal_sql`) or as
      # part of the validation step (`init_sql`)
      if (!is.na(agent$validation_set$init_sql[i])) {
        initial_sql_stmt <- agent$validation_set$init_sql[i]
      } else if (!is.na(agent$focal_init_sql)) {
        initial_sql_stmt <- agent$focal_init_sql
      } else {
        initial_sql_stmt <- NULL
      }
      
      # Create `table` object as an SQL entry point for a remote table
      
      if (!is.na(agent$focal_db_cred_file_path)) {
        
        table <- 
          set_entry_point(
            table = agent$validation_set$tbl_name[i],
            db_type = agent$validation_set$db_type[i],
            creds_file = agent$validation_set$db_cred_file_path[i],
            initial_sql = initial_sql_stmt)
        
      } else if (length(agent$focal_db_env_vars) != 0) {
        
        table <- 
          set_entry_point(
            table = agent$validation_set$tbl_name[i],
            db_type = agent$validation_set$db_type[i],
            db_creds_env_vars = agent$focal_db_env_vars,
            initial_sql = initial_sql_stmt)
      }
    }
    
    # Use preconditions to modify the table
    if (!is.na(agent$preconditions[[i, 1]]) &&
        agent$preconditions[[i, 1]] != "NULL") {
      
      # Get the preconditions as a character vector
      preconditions <-
        stringr::str_trim(
          agent$preconditions[[i, 1]] %>%
            strsplit(";") %>%
            unlist())
      
      if (!is.null(preconditions)) {
        for (j in 1:length(preconditions)) {
          
          # Apply the preconditions to filter the table
          # before any validation occurs
          table <-
            table %>%
            dplyr::filter_(preconditions[j])
        }
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # rely on betweenness checking
    
    if (agent$validation_set$assertion_type[i] %in%
        c("col_vals_between", "col_vals_not_between")) {
      
      # Get the `left` and `right` bounding values
      bounding_vals <- 
        (agent[["sets"]] %>%
           dplyr::select(set) %>%
           purrr::flatten_chr())[[i]]
      
      left <-
        (bounding_vals %>%
           strsplit(",") %>%
           unlist() %>%
           as.numeric())[[1]]
      
      right <-
        (bounding_vals %>%
           strsplit(",") %>%
           unlist() %>%
           as.numeric())[[2]]
      
      incl_na <- 
        (agent[["sets"]] %>%
           dplyr::select(incl_na) %>%
           purrr::flatten_lgl())[[i]]
      
      incl_nan <- 
        (agent[["sets"]] %>%
           dplyr::select(incl_nan) %>%
           purrr::flatten_lgl())[[i]]
      
      # Use the column name as an `rlang` symbol
      column <- rlang::sym(agent$validation_set$column[i])
      
      if (agent$validation_set$assertion_type[i] == "col_vals_between") {
        
        # Get the final judgment on the table and the query
        judgment <-
          table %>%
          dplyr::mutate(pb_is_good_ = case_when(
            rlang::UQ(column) >= left & rlang::UQ(column) <= right ~ TRUE,
            rlang::UQ(column) < left | rlang::UQ(column) > right ~ FALSE,
            is.na(rlang::UQ(column)) & incl_na ~ TRUE,
            is.na(rlang::UQ(column)) & incl_na == FALSE ~ FALSE,
            is.nan(rlang::UQ(column)) & incl_nan ~ TRUE,
            is.nan(rlang::UQ(column)) & incl_nan == FALSE ~ FALSE
          ))
      }
      
      if (agent$validation_set$assertion_type[i] == "col_vals_not_between") {
        
        excl_na <- incl_na
        excl_nan <- incl_nan
        
        # Get the final judgment on the table and the query
        judgment <-
          table %>%
          dplyr::mutate(pb_is_good_ = case_when(
            rlang::UQ(column) < left | rlang::UQ(column) > right ~ TRUE,
            rlang::UQ(column) >= left & rlang::UQ(column) <= right ~ FALSE,
            is.na(rlang::UQ(column)) & excl_na ~ TRUE,
            is.na(rlang::UQ(column)) & excl_na == FALSE ~ FALSE,
            is.nan(rlang::UQ(column)) & excl_nan ~ TRUE,
            is.nan(rlang::UQ(column)) & excl_nan == FALSE ~ FALSE
          ))
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # rely on set membership
    
    if (agent$validation_set$assertion_type[i] %in%
        c("col_vals_in_set", "col_vals_not_in_set")) {
      
      # Get the set values for the expression
      set <- 
        (agent[["sets"]] %>%
           dplyr::select(set) %>%
           purrr::flatten_chr())[[i]]
      
      set <- 
        stringr::str_trim(
          set %>%
            strsplit(",") %>%
            unlist())
      
      # Get the class of the set components
      set_class <- 
        (agent[["sets"]] %>%
           dplyr::select(class) %>%
           purrr::flatten_chr())[[i]]
      
      if (set_class == "numeric") {
        set <- as.numeric(set)
      }
      
      if (agent$validation_set$assertion_type[i] == "col_vals_in_set") {
        
        # Use the column name as an `rlang` symbol
        column <- rlang::sym(agent$validation_set$column[i])
        
        # Get the final judgment on the table and the query
        judgment <-
          table %>%
          dplyr::mutate(pb_is_good_ = case_when(
            rlang::UQ(column) %in% set ~ TRUE,
            !(rlang::UQ(column) %in% set) ~ FALSE
          ))
      }
      
      if (agent$validation_set$assertion_type[i] == "col_vals_not_in_set") {
        
        # Use the column name as an `rlang` symbol
        column <- rlang::sym(agent$validation_set$column[i])
        
        # Get the final judgment on the table and the query
        judgment <-
          table %>%
          dplyr::mutate(pb_is_good_ = case_when(
            !(rlang::UQ(column) %in% set) ~ TRUE,
            rlang::UQ(column) %in% set ~ FALSE
          ))
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on regex matching
    
    if (agent$validation_set$assertion_type[i] == "col_vals_regex") {
      
      # Get the regex matching statement
      regex <- agent$validation_set$regex[i]
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("grepl(\"",
                 agent$validation_set$regex[i],
                 "\", ", agent$validation_set$column[i],
                 ")"),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on the presence
    # of NULL values
    
    if (agent$validation_set$assertion_type[i] == "col_vals_null") {
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("is.na(",
                 agent$validation_set$column[i],
                 ")"),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on the absence
    # of NULL values
    
    if (agent$validation_set$assertion_type[i] == "col_vals_not_null") {
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("!is.na(",
                 agent$validation_set$column[i],
                 ")"),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # check the table structure
    
    if (agent$validation_set$assertion_type[i] == "col_exists") {
      
      # Get the column names for the table
      column_names <-
        table %>%
        dplyr::filter(row_number() == 1) %>%
        dplyr::as_tibble() %>%
        colnames()
      
      judgment <-
        ifelse(
          agent$validation_set$column[i] %in% column_names,
          TRUE, FALSE)
      
      agent$validation_set$n[i] <- 1
      
      agent$validation_set$n_passed[i] <- 
        agent$validation_set$f_passed[i] <-
        ifelse(judgment, 1, 0)
      
      agent$validation_set$n_failed[i] <-
        agent$validation_set$f_failed[i] <-
        ifelse(judgment, 0, 1)
      
      agent$validation_set$all_passed[i] <-
        ifelse(judgment, TRUE, FALSE)
      
      if (judgment) {
        n_failed <- false_count <- 0
      } else {
        n_failed <- false_count <- 1  
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that rely on
    # comparison operators
    
    if (agent$validation_set$assertion_type[i] %in%
        c("col_vals_gt", "col_vals_gte",
          "col_vals_lt", "col_vals_lte",
          "col_vals_equal", "col_vals_not_equal")) {
      
      # Get operator values for all assertion types involving
      # simple operator comparisons
      if (agent$validation_set$assertion_type[i] == "col_vals_gt") {
        operator <- ">"
      } else if (agent$validation_set$assertion_type[i] == "col_vals_gte") {
        operator <- ">="
      } else if (agent$validation_set$assertion_type[i] == "col_vals_lt") {
        operator <- "<"
      } else if (agent$validation_set$assertion_type[i] == "col_vals_lte") {
        operator <- "<="
      } else if (agent$validation_set$assertion_type[i] == "col_vals_equal") {
        operator <- "=="
      } else if (agent$validation_set$assertion_type[i] == "col_vals_not_equal") {
        operator <- "!="
      } 
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0(
            agent$validation_set$column[i],
            operator,
            agent$validation_set$value[i]),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Determine the `false_count` for all validations
    # that validate individual rows in one or more table columns
    
    if (grepl("col_vals.*", agent$validation_set$assertion_type[i])) {
      
      # Get total count of rows
      row_count <-
        judgment %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      # Get total count of TRUE rows
      n_passed <-
        judgment %>%
        dplyr::filter(pb_is_good_ == TRUE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      # Get total count of FALSE rows
      n_failed <-
        judgment %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      agent$validation_set$n[i] <- row_count
      agent$validation_set$n_passed[i] <- n_passed
      agent$validation_set$n_failed[i] <- n_failed
      agent$validation_set$f_passed[i] <- round((n_passed / row_count), 5)
      agent$validation_set$f_failed[i] <- round((n_failed / row_count), 5)
      
      # Get count of rows where `pb_is_good_ == FALSE`
      false_count <-
        judgment %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(pb_is_not_good_ = n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      if (false_count > 0) {
        
        # State that `all_passed` is FALSE
        agent$validation_set$all_passed[i] <- FALSE
        
        # Collect problem rows if requested
        
        if (get_problem_rows) {
          
          problem_rows <- 
            judgment %>%
            dplyr::filter(pb_is_good_ == FALSE) %>%
            dplyr::select(-pb_is_good_)
          
          if (!is.null(get_first_n)) {
            
            problem_rows <-
              problem_rows %>%
              utils::head(get_first_n) %>%
              dplyr::as_tibble()
            
          } else if (!is.null(sample_n) &
                     !(agent$validation_set$db_type[i] %in% c("PostgreSQL", "MySQL"))) {
            
            problem_rows <-
              dplyr::sample_n(
                tbl = problem_rows,
                size = sample_n,
                replace = FALSE) %>%
              dplyr::as_tibble()
            
          } else if (!is.null(sample_frac) &
                     !(agent$validation_set$db_type[i] %in% c("PostgreSQL", "MySQL"))) {
            
            problem_rows <-
              dplyr::sample_frac(
                tbl = problem_rows,
                size = sample_frac,
                replace = FALSE) %>%
              dplyr::as_tibble() %>%
              utils::head(sample_limit)
            
          } else {
            
            problem_rows <-
              problem_rows %>%
              utils::head(5000) %>%
              dplyr::as_tibble()
          }
          
          problem_rows <-
            problem_rows %>%
            dplyr::mutate(pb_step_ = i) %>%
            dplyr::select(pb_step_, dplyr::everything())
        }
        
        # Place the sample of problem rows in
        # the `agent$validation_set` tbl_df
        # as a nested tbl_df
        names_problem_rows <- names(problem_rows)
        
        agent$validation_set$row_sample[i] <- 
          tidyr::nest_(
            data = problem_rows,
            key_col = "data",
            nest_cols = names_problem_rows)
        
      } else if (false_count == 0) {
        agent$validation_set$all_passed[i] <- TRUE
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables on expected column types
    
    if (grepl("col_is_.*", agent$validation_set$assertion_type[i])) {
      
      if (inherits(table, "data.frame")) {
        
        column_type <-
          (table %>%
             dplyr::select_(agent$validation_set$column[i]) %>%
             dplyr::filter(row_number() == 1) %>%
             dplyr::collect() %>%
             as.data.frame(stringsAsFactors = FALSE))[1, 1] %>% 
          class()
        
        agent$validation_set$n[i] <- 1
        
        if (agent$validation_set$assertion_type[i] == "col_is_numeric") {
          passed <- ifelse(column_type[1] == "numeric", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_integer") {
          passed <- ifelse(column_type[1] == "integer", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_character") {
          passed <- ifelse(column_type[1] == "character", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_logical") {
          passed <- ifelse(column_type[1] == "logical", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_factor") {
          passed <- ifelse(column_type[1] == "factor", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_posix") {
          passed <- ifelse(column_type[1] == "POSIXct", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_date") {
          passed <- ifelse(column_type[1] == "Date", TRUE, FALSE)
        } else {
          passed <- FALSE
        }
        
        if (passed == TRUE) {
          agent$validation_set$n_passed[i] <- 
            agent$validation_set$f_passed[i] <- 1
          agent$validation_set$n_failed[i] <- 
            agent$validation_set$f_failed[i] <- 0
          false_count <- 0
        } else {
          agent$validation_set$n_passed[i] <- 
            agent$validation_set$f_passed[i] <- 0
          agent$validation_set$n_failed[i] <- 
            agent$validation_set$f_failed[i] <- 1
          false_count <- 1
        }
        
        if (false_count > 0) {
          agent$validation_set$all_passed[i] <- FALSE
        } else if (false_count == 0) {
          agent$validation_set$all_passed[i] <- TRUE
        }
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables on expectation of non-duplicated rows
    
    if (agent$validation_set$assertion_type[i] == "rows_not_duplicated") {
      
      # Determine if grouping columns are provided in the test
      # for distinct rows and parse the column names
      
      if (!is.na(agent$validation_set$column[i])) {
        if (grepl("(,|&)", agent$validation_set$column[i])) {
          columns <-
            stringr::str_trim(
              stringr::str_split(
                agent$validation_set$column[i],
                pattern = "(,|&)") %>%
                purrr::flatten_chr())
        } else {
          columns <- agent$validation_set$column[i]
        }
      } else if (is.na(agent$validation_set$column[i])) {
        columns <-
          table %>%
          dplyr::filter(row_number() == 1) %>%
          dplyr::as_tibble() %>%
          names()
      }
      
      # Get total count of rows
      row_count <-
        table %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      # Get the rows that are duplicate rows, if any
      duplicate_rows <- 
        table %>%
        dplyr::select_(paste0("c(", paste(columns, collapse = ", ")) %>% paste0(")")) %>%
        dplyr::group_by_(.dots = columns) %>%
        dplyr::filter(n() > 1) %>%
        dplyr::ungroup()
      
      # Determine whether the test for duplicated passed
      # (no duplicates) or failed (one or more duplicates)
      passed <-
        ifelse(
          duplicate_rows %>%
            dplyr::group_by() %>%
            dplyr::summarize(row_count = n()) %>%
            dplyr::as_tibble() %>%
            purrr::flatten_dbl() == 0, TRUE, FALSE)
      
      if (passed == TRUE) {
        n_passed <- row_count
        n_failed <- false_count <- 0
      } else if (passed == FALSE) {
        n_failed <- false_count <-
          duplicate_rows %>%
          dplyr::group_by() %>%
          dplyr::summarize(row_count = n()) %>%
          dplyr::as_tibble() %>%
          purrr::flatten_dbl()
        
        n_passed <- row_count - n_failed
      }
      
      agent$validation_set$n[i] <- row_count
      agent$validation_set$n_passed[i] <- n_passed
      agent$validation_set$n_failed[i] <- n_failed
      agent$validation_set$f_passed[i] <- round((n_passed / row_count), 5)
      agent$validation_set$f_failed[i] <- round((n_failed / row_count), 5)
      
      if (false_count > 0) {
        agent$validation_set$all_passed[i] <- FALSE
      } else if (false_count == 0) {
        agent$validation_set$all_passed[i] <- TRUE
      }
    }
    
    # ---------------------------------------------------------------
    # Determine the course of action for each validation check
    
    actions <-
      determine_action(
        n = agent$validation_set$n[i],
        false_count = false_count,
        warn_count = agent$validation_set$warn_count[i],
        notify_count = agent$validation_set$notify_count[i],
        warn_fraction = agent$validation_set$warn_fraction[i],
        notify_fraction = agent$validation_set$notify_fraction[i])
    
    agent$validation_set$notify[i] <- actions$notify
    agent$validation_set$warn[i] <- actions$warn
    
    # Get the ending time for the validation step
    validation_end_time <- Sys.time()
    
    # Get the duration for the validation step    
    time_diff_s <- (validation_end_time - validation_start_time)[[1]] %>% round(4)
    
    # Add the timing information to the `agent` object
    agent$validation_set$time_processed[i] <- validation_start_time
    agent$validation_set$proc_duration_s[i] <- time_diff_s
  }
  
  # Disconnect any open PostgreSQL connections --------------------
  #disconnect_postgres()
  
  # Notification Step - email ---------------------------------------------
  if (length(agent$email_creds_file_path) > 0 &
      length(agent$email_notification_recipients) > 0 & 
      agent$email_notifications_active == TRUE &
      nrow(agent$validation_set) > 0 &
      any(agent$validation_set$notify == TRUE)) {
    
    # TODO: perform notification via notification method
  }
  
  # Notification Step - Slack ---------------------------------------------
  if (length(agent$slack_webhook_url) > 0 & 
      agent$slack_notifications_active == TRUE &
      nrow(agent$validation_set) > 0 &
      any(agent$validation_set$notify == TRUE)) {
    
    # TODO: perform notification via notification method
    notify_count <- 
      agent$validation_set %>%
      dplyr::select(notify) %>%
      dplyr::filter(notify == TRUE) %>%
      nrow()
    
    warning_count <- 
      agent$validation_set %>%
      dplyr::select(warn) %>%
      dplyr::filter(warn == TRUE) %>%
      nrow()
    
    # If a value for `slack_footer_timestamp` is not
    # provided, use the system time as the timestamp
    if (is.null(slack_footer_timestamp)) {
      slack_footer_timestamp <- Sys.time() %>% as.integer()
    }
    
    if (notify_count > 0) {
      
      notify_text <-
        ifelse(
          notify_count == 1,
          glue::glue("There is {notify_count} validation step that resulted in significant failures."),
          glue::glue("There are {notify_count} validation steps that resulted in significant failures."))
    }
    
    if (warning_count > 0) {
      
      warning_text <-
        ifelse(
          warning_count == 1,
          glue::glue("There is {warning_count} validation step that issued a warning."),
          glue::glue("There are {warning_count} validation steps that issued warnings."))
    }
    
    if (notify_count > 0 & warning_count > 0) {
      notification_text <- paste0(
        gsub("\\.", "", notify_text),
        " and ",
        gsub("There", "there", warning_text))
    }
    
    if (notify_count > 1 & warning_count == 0) {
      notification_text <- notify_text
    }
    
    slack_footer_timestamp <- Sys.time() %>% as.integer()
    
    httr::POST(
      url = agent$slack_webhook_url,
      encode = "form",
      httr::add_headers(
        `Content-Type` = "application/x-www-form-urlencoded",
        Accept = "*/*"),
      body = utils::URLencode(
        glue::glue(
          "payload={{
                   \"channel\": \"{agent$slack_channel}\",
                   \"username\": \"{agent$slack_username}\",
                   \"attachments\": [
                   {{
                   \"fallback\": \"{agent$slack_title}\",
                   \"color\": \"danger\",
                   \"author_name\": \"{agent$slack_author_name}\",
                   \"title\": \"{agent$slack_title}\",
                   \"title_link\": \"{agent$slack_report_url}\",
                   \"text\": \"{notification_text}\",
                   \"thumb_url\": \"{agent$slack_footer_thumb_url}\",
                   \"footer\": \"{agent$slack_footer_text}\",
                   \"ts\": {slack_footer_timestamp}
                   }}
                   ]
                   }}")))
  }
  
  agent
}
