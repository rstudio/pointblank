is_ptblank_agent <- function(x) {
  inherits(x, "ptblank_agent")
}

is_ptblank_informant <- function(x) {
  inherits(x, "ptblank_informant")
}

is_a_table_object <- function(x) {
  inherits(x, c("data.frame", "tbl_df", "tbl_dbi", "tbl_spark"))
}

is_tbl_spark <- function(x) {
  inherits(x, "tbl_spark")
}

is_tbl_dbi <- function(x) {
  inherits(x, "tbl_dbi")
}

get_tbl_object <- function(agent) {
  agent$tbl
}

# Generate a label for the `agent` or `informant` object
generate_label <- function(label = NULL) {
  
  if (!is.null(label)) return(as.character(label))
  
  paste0("[", gsub(" ", "|", as.character(Sys.time())), "]")
}

safely_transformer <- function(otherwise = NA) {
  
  function(text, envir) {
    tryCatch(
      eval(parse(text = text, keep.source = FALSE), envir),
      error = function(e) if (is.language(otherwise)) eval(otherwise) else otherwise)
  }
}

glue_safely <- function(..., .otherwise = NA, .envir = parent.frame()) {
  as.character(
    glue::glue(
      ...,
      .transformer = safely_transformer(.otherwise),
      .envir = .envir
    )
  )
}

has_agent_intel <- function(agent) {
  inherits(agent, "has_intel")
}

is_agent_empty <- function(agent) {
  if (!is_ptblank_agent(agent)) return(FALSE)
  if (nrow(agent$validation_set) == 0) TRUE else FALSE
}

interrogation_time <- function(agent) {
  if (has_agent_intel(agent)) agent$time_start else NA
}

number_of_validation_steps <- function(agent) {
  if (is_ptblank_agent(agent)) agent$validation_set %>% nrow() else NA
}

get_assertion_type_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "assertion_type"]]
}

get_column_as_sym_at_idx <- function(agent, idx) {
  rlang::sym(agent$validation_set[[idx, "column"]] %>% unlist() %>% gsub("'", "", .))
}

get_values_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "values"]] %>% unlist(recursive = FALSE)
}

get_column_na_pass_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "na_pass"]]
}

get_all_cols <- function(agent) {
  agent$col_names
}

resolve_expr_to_cols <- function(tbl, var_expr) {
  
  var_expr <- enquo(var_expr)
  
  if ((var_expr %>% rlang::get_expr() %>% as.character())[1] == "vars") {
    
    cols <- (var_expr %>% rlang::get_expr() %>% as.character())[-1]
    return(cols)
  }
  
  tidyselect::vars_select(.vars = colnames(tbl), {{ var_expr }}) %>% unname()
}

resolve_columns <- function(x, var_expr, preconditions) {
  
  # Return an empty character vector if the expr is NULL
  if (inherits(var_expr, "quosure") && var_expr %>% rlang::as_label() == "NULL") {
    return(character(0))
  } 
  
  # Get the column names
  if (is.null(preconditions)) {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      column <- resolve_expr_to_cols(tbl = x, var_expr = !!var_expr)
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
    
  } else {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      tbl <- apply_preconditions(tbl = x, preconditions = preconditions)
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      
      tbl <- apply_preconditions(tbl = tbl, preconditions = preconditions)
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
  }
  
  column
}

normalize_step_id <- function(step_id, columns, agent) {

  if (is.null(step_id)) {

    if (length(agent$validation_set$i) < 1) {
      i <- 1
    } else {
      i <- max(agent$validation_set$i) + 1
    }
    
    steps <- length(columns) - 1
    i <- seq(from = i, to = i + steps)
    
    # Generate an ID based on `i`
    step_id <- formatC(i, width = 4, format = "d", flag = "0")
    return(step_id)
  }
  
  # Ensure that `step_id` is coerced to character
  step_id <- as.character(step_id)
  
  if (length(columns) == 1) {
    
    if (length(step_id) > 1) {
      warning(
        "Multiple `step_id` values provided for a single column:\n",
        "* Only the first `step_id` element will be used.",
        call. = FALSE
      )
    }
    
    return(step_id[1])
    
  } else if (length(columns) > 1) {
    
    if (length(step_id) == length(columns)) {
      
     if (anyDuplicated(step_id) != 0) {
       
       # Issue warning about duplicated `step_id` values
       warning(
         "Duplicate `step_id` values provided:\n",
         "* Only the first `step_id` element will be used.",
         call. = FALSE
       )
       
       step_id <- generate_indexed_vals(step_id[1], seq_along(columns))
     }
    
      return(step_id)
      
    } else if (length(step_id) == 1) {
      
      # Generate multiple `step_id` values with single `step_id`
      step_id <- generate_indexed_vals(step_id[1], seq_along(columns))
      
    } else if (length(step_id) != 1 && length(step_id) != length(columns)) {

      # Issue warning about length of `step_id`
      warning(
        "The number of `step_id` values is neither `1` nor the number of `columns`:\n",
        "* Only the first `step_id` element will be used.",
        call. = FALSE
      )
      
      step_id <- generate_indexed_vals(step_id[1], seq_along(columns))
    }
  }
  
  step_id
}

check_step_id_duplicates <- function(step_id, agent) {
  
  if (any(step_id %in% agent$validation_set$step_id)) {

    error_at_index <- max(agent$validation_set$i)
    a_duplicate_step_id <- step_id[step_id %in% agent$validation_set$step_id][1]
    
    stop(
      "Just after step index `", error_at_index, "`, the following `step_id` has been ",
      "seen as used in a previous validation step:\n",
      " * \"", a_duplicate_step_id, "\"",
      call. = FALSE
    )
  }
}

generate_indexed_vals <- function(x, numbers, sep = ".") {
  paste0(x, sep, formatC(numbers, width = 4, format = "d", flag = "0"))
}

get_threshold_type <- function(threshold) {
  
  if (threshold >= 1) {
    threshold_type <- "absolute"
  } else if (threshold >= 0 && threshold < 1) {
    threshold_type <- "proportional"
  }
}

row_based_step_fns_vector <- function() {
  
  c(
    "col_vals_lt",
    "col_vals_lte",
    "col_vals_equal",
    "col_vals_not_equal",
    "col_vals_gte",
    "col_vals_gt",
    "col_vals_between",
    "col_vals_not_between",
    "col_vals_in_set",
    "col_vals_not_in_set",
    "col_vals_null",
    "col_vals_not_null",
    "col_vals_regex",
    "col_vals_expr",
    "conjointly",
    "rows_distinct"
  )
}

get_tbl_dbi_src_info <- function(tbl) {
  utils::capture.output(tbl %>% unclass() %>% .$src)
}

get_tbl_dbi_src_details <- function(tbl) {
  tbl_src_info <- get_tbl_dbi_src_info(tbl)
  tbl_src_info[grepl("^src:", tbl_src_info)] %>% gsub("src:\\s*", "", .)
}

get_r_column_names_types <- function(tbl) {
  
  suppressWarnings(
    column_names_types <-
      tbl %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      vapply(
        FUN.VALUE = character(1),
        FUN = function(x) class(x)[1]
      )
  )
  
  list(
    col_names = names(column_names_types),
    r_col_types = unname(unlist(column_names_types))
  )
}

get_tbl_information <- function(tbl) {
  
  if (is.data.frame(tbl)) {
    
    tbl_information <- get_tbl_information_df(tbl)
    
  } else if (is_tbl_spark(tbl)) {
    
    tbl_information <- get_tbl_information_spark(tbl)
    
  } else if (is_tbl_dbi(tbl)) {
    
    tbl_information <- get_tbl_information_dbi(tbl)
    
  } else {
    warning("Information on this table type cannot be obtained at present.",
            call. = FALSE)
  } 
  
  tbl_information
}

get_tbl_information_df <- function(tbl) {
  
  r_column_names_types <- get_r_column_names_types(tbl)
  
  tbl_src <- if (tibble::is_tibble(tbl)) "tbl_df" else "data.frame"
  
  list(
    tbl_src = tbl_src,
    tbl_src_details = NA_character_,
    db_tbl_name = NA_character_,
    col_names = r_column_names_types$col_names,
    r_col_types = r_column_names_types$r_col_types,
    db_col_types = NA_character_
  )
}

get_tbl_information_spark <- function(tbl) {
  
  r_column_names_types <- get_r_column_names_types(tbl)
  
  tbl_schema <- sparklyr::sdf_schema(tbl)
  
  db_col_types <- 
    lapply(tbl_schema, `[[`, 2) %>%
    unlist() %>%
    unname() %>%
    tolower()
  
  list(
    tbl_src = "tbl_spark",
    tbl_src_details = "Spark",
    db_tbl_name = NA_character_,
    col_names = r_column_names_types$col_names,
    r_col_types = r_column_names_types$r_col_types,
    db_col_types = db_col_types
  )
}

get_tbl_information_dbi <- function(tbl) {
  
  tbl_connection <- tbl$src$con

  tbl_src_details <- tolower(get_tbl_dbi_src_details(tbl))
  
  if (grepl("sql server|sqlserver", tbl_src_details)) {
    tbl_src <- "mssql"
  } else {
    tbl_src <- gsub("^([a-z]*).*", "\\1", get_tbl_dbi_src_details(tbl))
  }
  
  db_tbl_name <- as.character(dbplyr::remote_name(tbl))
  
  r_column_names_types <- get_r_column_names_types(tbl)
  
  n_cols <- length(r_column_names_types$col_names)
  
  if (tbl_src == "postgres") {
    
    db_tbl_name_no_schema <- gsub('.*\\.', '', db_tbl_name)
    
    q_types <- 
      glue::glue(
        "select column_name,data_type from information_schema.columns where table_name = '{db_tbl_name_no_schema}'"
      )

  } else {
    
    q_types <- 
      ifelse(
        tbl_src == "mssql",
        glue::glue("SELECT TOP 9 {n_cols} DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = '{db_tbl_name}'"),
        glue::glue("SELECT DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = '{db_tbl_name}' LIMIT {n_cols}")
      )
  }
  
  if (tbl_src == "postgres") {
    
    db_col_types <- 
      DBI::dbGetQuery(tbl_connection, q_types) %>%
      dplyr::pull(data_type) %>%
      tolower()
  }
  
  if (tbl_src == "sqlite") {
    db_col_types <-
      vapply(
        r_column_names_types$col_names,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          
          DBI::dbDataType(
            tbl_connection,
            tbl %>%
              dplyr::select(x) %>%
              utils::head(1) %>%
              dplyr::collect() %>%
              dplyr::pull(x)
          )
        }
      ) %>%
      tolower()
  }
  
  if (tbl_src != "sqlite" & tbl_src != "postgres") {

    db_col_types <- 
      DBI::dbGetQuery(tbl_connection, q_types) %>%
      dplyr::collect() %>%
      dplyr::pull(DATA_TYPE) %>%
      tolower()
  }
  
  if (!exists("db_col_types")) {
    db_col_types <- NA_character_
  }

  list(
    tbl_src = tbl_src,
    tbl_src_details = get_tbl_dbi_src_details(tbl),
    db_tbl_name = db_tbl_name,
    col_names = r_column_names_types$col_names,
    r_col_types = r_column_names_types$r_col_types,
    db_col_types = db_col_types
  )
}

pb_fmt_number <- function(x,
                          decimals = 2,
                          n_sigfig = NULL,
                          drop_trailing_zeros = FALSE,
                          drop_trailing_dec_mark = TRUE,
                          use_seps = TRUE,
                          scale_by = 1,
                          suffixing = FALSE,
                          pattern = "{x}",
                          sep_mark = ",",
                          dec_mark = ".",
                          locale = NULL) {
  
  if (is.null(x)) return(NULL)
  
  if (length(x) == 1 && (!inherits(x, "numeric") && !inherits(x, "integer"))) {
    return(x)
  } 
  
  ((dplyr::tibble(a = x) %>%
      gt::gt() %>%
      gt::fmt_number(
        vars(a),
        decimals = decimals,
        n_sigfig = n_sigfig,
        drop_trailing_zeros = drop_trailing_zeros,
        drop_trailing_dec_mark = drop_trailing_dec_mark,
        use_seps = use_seps,
        scale_by = scale_by,
        suffixing = suffixing,
        pattern = pattern,
        sep_mark = sep_mark,
        dec_mark = dec_mark,
        locale = locale
      ))$`_formats`[[1]][[1]][[1]])(x)
}

add_icon_img <- function(icon,
                         height = 30,
                         v_align = "middle") {

  file <- 
    system.file(
      "img", "function_icons", paste0(icon, ".png"),
      package = "pointblank"
    )
  
  image_raw <-
    readBin(
      con = file,
      what = "raw",
      n = file.info(file)$size
    )
  
  image_uri <- 
    paste0(
      "data:image/png;base64,",
      base64enc::base64encode(image_raw, 0)
    )
  
  img <- 
    htmltools::tags$img(
      src = image_uri,
      alt = icon,
      style = htmltools::css(
        height = htmltools::validateCssUnit(height),
        `vertical-align` = v_align
      )
    )
  
  gsub("\\s\\s+", " ", htmltools::HTML(as.character(img)))
}

add_icon_svg <- function(icon,
                         height = 30,
                         v_align = "middle") {
  
  file <- 
    system.file(
      "img", "function_icons", paste0(icon, ".svg"),
      package = "pointblank"
    )
  
  htmltools::tags$div(
    style = htmltools::css(
      margin = 0,
      padding = 0,
      display = "inline-block",
      height = "30px",
      `vertical-align` = "middle"
    ),
    htmltools::HTML(
      paste(readLines(con = file, warn = FALSE), collapse = "") %>%
        tidy_gsub("width=\"[0-9]*?px", paste0("width=\"", height, "px")) %>%
        tidy_gsub("height=\"[0-9]*?px", paste0("height=\"", height, "px"))
    )
  ) %>% as.character()
}

tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {
  gsub(pattern, replacement, x, fixed = fixed)
}
