
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
            class = class(set))}) %>%
      dplyr::select(-x)
  } else {
    set_df <-
      1:nrow(validation_step_df) %>%
      purrr::map_df(
        function(x) {
          tibble::tibble(
            x = x,
            set = as.character(NA),
            class = as.character(NA))}) %>%
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
  
  tbl_entry
}


# With any `all_cols()` call, return a
# wildcard operator
all_cols <- function() {"*"}

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

# Send an email notification
#' @importFrom mailR send.mail
#' @importFrom dplyr filter mutate select rename
#' @importFrom pixiedust dust sprinkle sprinkle_print_method
pb_notify <- function(agent,
                      recipients,
                      creds_file) {
  
  # Create bindings for variables
  notify <- step <- tbl_name <- db_type <-
    assertion_type <- notify_count <- notify_fraction <- 
    tbl_name_chars <- tbl_name_abbrev <- column <-
    n_passed <- n_failed <- f_passed <- f_failed <- NULL
  
  # Read in email credentials from `creds_file`
  credentials <- readRDS(creds_file)
  sender <- credentials[1]
  host <- credentials[2]
  port <- as.integer(credentials[3])
  user <- credentials[4]
  password <- credentials[5]
  use_ssl <- as.logical(credentials[6])
  authenticate <- as.logical(credentials[7])
  
  # Create `mutate_when()` function
  mutate_when <- function(data, ...) {
    dots <- eval(substitute(alist(...)))
    for (i in seq(1, length(dots), by = 2)) {
      condition <- eval(dots[[i]], envir = data)
      mutations <- eval(dots[[i + 1]], envir = data[condition, ])
      data[condition, names(mutations)] <- mutations
    }
    data
  }
  
  # Get the number of validation tests that
  # resulted in a `notify` action
  number_notify <-
    agent$validation_set %>%
    dplyr::filter(notify == TRUE) %>%
    nrow()
  
  # Get the validation time
  validation_time <- agent$validation_time
  
  # Get the interrogation date
  interrogation_date <-
    paste0(
      format(validation_time, "%A, %B "),
      format(validation_time, "%d") %>% as.numeric(),
      ", ",
      format(validation_time, "%Y"),
      " at ",
      format(validation_time, "%l:%M") %>% trimws(),
      toupper(format(validation_time, " %p")),
      format(validation_time, " (%Z)"))
  
  # Create the subject line of the email
  # notification message
  subject <- 
    paste0(
      "pointblank notifier: ",
      ifelse(number_notify > 1,
             paste0(number_notify, " tests have execeed their set threshold values. "),
             paste0(number_notify, " test has exceeded its set threshold value.")))
  
  # Generate introductory text for the message body
  text_1 <-
    paste0(
      "For the pointblank interrogation that was conducted on ",
      interrogation_date,
      ", ",
      ifelse(
        number_notify > 1,
        paste0(
          number_notify, " validation steps have resulted in ",
          "execeedances over their set threshold values."),
        paste0("a validation step has exceeded its set threshold value.")))
  
  # Generate closing text for the message body
  text_2 <-
    paste0(
      "It is hoped that this notification is helpful and won't lead to very ",
      "much consternation. Yes, the road to completely validated and as-correct-as-possible ",
      "data is quite long but it is a journey worth taking.")
  
  # Generate an HTML table with information on the validation
  # tests that exceeded threshold values
  table_summary <- 
    agent$validation_set %>%
    dplyr::mutate(step = row_number()) %>%
    dplyr::filter(notify == TRUE) %>%
    dplyr::select(
      step, tbl_name, db_type, assertion_type, column,
      n, n_passed, n_failed, f_passed, f_failed, notify_count, notify_fraction) %>%
    mutate_when(
      is.na(notify_count) & !is.na(notify_fraction),
      list(Threshold = paste0((notify_fraction * 100), "%")),
      !is.na(notify_count) & is.na(notify_fraction),
      list(Threshold = paste0("n = ", notify_count))) %>%
    select(-notify_count, -notify_fraction) %>%
    mutate(tbl_name_chars = nchar(tbl_name)) %>%
    mutate(tbl_name_abbrev = substr(tbl_name, 0, 20)) %>%
    mutate_when(
      tbl_name_chars > 20,
      list(tbl_name =
             paste0(
               "<span title=\"", tbl_name, "\">",
               tbl_name_abbrev, "...</span>"))) %>%
    select(-tbl_name_chars, -tbl_name_abbrev) %>%
    dplyr::mutate(f_failed = paste0(as.character(f_failed * 100), "%")) %>%
    dplyr::mutate(f_passed = paste0(as.character(f_passed * 100), "%")) %>%
    pixiedust::dust() %>%
    pixiedust::sprinkle_print_method("html") %>%
    pixiedust::sprinkle_colnames(step = "Step",
                                 tbl_name = "Table Name",
                                 db_type = "Database Type",
                                 assertion_type = "Assertion",
                                 column = "Column",
                                 n_passed = "Number Passed",
                                 n_failed = "Number Failed",
                                 f_passed = " % Passed",
                                 f_failed = " % Failed") %>%
    pixiedust::sprinkle(
      part = "head",
      border = "top",
      border_color = "#979797",
      border_thickness = 2,
      pad = 5,
      font_family = "Helvetica",
      font_size = 8) %>% 
    pixiedust::sprinkle(
      part = "body",
      pad = 5,
      font_family = "Monaco",
      font_size = 8,
      border = c("top", "bottom"),
      border_color = "#979797",
      bg_pattern = c("#F8F8F8", "#FFFFFF")) %>%
    pixiedust::sprinkle(
      part = "head",
      cols = c(1:5),
      halign = "left") %>%
    pixiedust::sprinkle(
      part = "head",
      cols = c(6:10),
      halign = "right") %>%
    pixiedust::sprinkle(
      part = "body",
      cols = c(1:5),
      halign = "left") %>%
    pixiedust::sprinkle(
      part = "body",
      cols = c(6:10),
      halign = "right") %>%
    print(asis = FALSE)
  
  # Generate the email message body
  body <-
    paste0(
      "<!doctype html>
      <html>
      <head>
      <meta name='viewport' content='width=device-width' />
      <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
      <title>pointblank Notification Message</title>
      <style>
      /* -------------------------------------
      GLOBAL RESETS
      ------------------------------------- */
      img {
      border: none;
      -ms-interpolation-mode: bicubic;
      max-width: 100%; }
      
      body {
      background-color: #f6f6f6;
      font-family: sans-serif;
      -webkit-font-smoothing: antialiased;
      font-size: 14px;
      line-height: 1.4;
      margin: 0;
      padding: 0; 
      -ms-text-size-adjust: 100%;
      -webkit-text-size-adjust: 100%; }
      
      table {
      border-collapse: separate;
      mso-table-lspace: 0pt;
      mso-table-rspace: 0pt;
      width: 100%; }
      table td {
      font-family: sans-serif;
      font-size: 14px;
      vertical-align: top; }
      
      /* -------------------------------------
      BODY & CONTAINER
      ------------------------------------- */
      
      .body {
      background-color: #f6f6f6;
      width: 100%; }
      
      .container {
      display: block;
      Margin: 0 auto !important;
      max-width: 620px;
      padding: 10px;
      width: 620px; }
      
      .content {
      box-sizing: border-box;
      display: block;
      Margin: 0 auto;
      max-width: 620px;
      padding: 10px; }
      
      /* -------------------------------------
      HEADER, FOOTER, MAIN
      ------------------------------------- */
      .main {
      background: #fff;
      border-radius: 3px;
      width: 100%; }
      
      .wrapper {
      box-sizing: border-box;
      padding: 20px; }
      
      .footer {
      clear: both;
      padding-top: 10px;
      text-align: center;
      width: 100%; }
      .footer td,
      .footer p,
      .footer span,
      .footer a {
      color: #999999;
      font-size: 12px;
      text-align: center; }
      
      /* -------------------------------------
      TYPOGRAPHY
      ------------------------------------- */
      h1,
      h2,
      h3,
      h4 {
      color: #000000;
      font-family: sans-serif;
      font-weight: 400;
      line-height: 1.4;
      margin: 0;
      Margin-bottom: 30px; }
      
      h1 {
      font-size: 35px;
      font-weight: 300;
      text-align: center;
      text-transform: capitalize; }
      
      p,
      ul,
      ol {
      font-family: sans-serif;
      font-size: 14px;
      font-weight: normal;
      margin: 0;
      Margin-bottom: 15px; }
      p li,
      ul li,
      ol li {
      list-style-position: inside;
      margin-left: 5px; }
      
      a {
      color: #3498db;
      text-decoration: underline; }
      
      /* -------------------------------------
      BUTTONS
      ------------------------------------- */
      .btn {
      box-sizing: border-box;
      width: 100%; }
      .btn > tbody > tr > td {
      padding-bottom: 15px; }
      .btn table {
      width: auto; }
      .btn table td {
      background-color: #ffffff;
      border-radius: 5px;
      text-align: center; }
      .btn a {
      background-color: #ffffff;
      border: solid 1px #3498db;
      border-radius: 5px;
      box-sizing: border-box;
      color: #3498db;
      cursor: pointer;
      display: inline-block;
      font-size: 14px;
      font-weight: bold;
      margin: 0;
      padding: 12px 25px;
      text-decoration: none;
      text-transform: capitalize; }
      
      .btn-primary table td {
      background-color: #3498db; }
      
      .btn-primary a {
      background-color: #3498db;
      border-color: #3498db;
      color: #ffffff; }
      
      /* -------------------------------------
      OTHER STYLES
      ------------------------------------- */
      .last {
      margin-bottom: 0; }
      
      .first {
      margin-top: 0; }
      
      .align-center {
      text-align: center; }
      
      .align-right {
      text-align: right; }
      
      .align-left {
      text-align: left; }
      
      .clear {
      clear: both; }
      
      .mt0 {
      margin-top: 0; }
      
      .mb0 {
      margin-bottom: 0; }
      
      .preheader {
      color: transparent;
      display: none;
      height: 0;
      max-height: 0;
      max-width: 0;
      opacity: 0;
      overflow: hidden;
      mso-hide: all;
      visibility: hidden;
      width: 0; }
      
      .powered-by a {
      text-decoration: none; }
      
      hr {
      border: 0;
      border-bottom: 1px solid #f6f6f6;
      Margin: 20px 0; }
      
      /* -------------------------------------
      RESPONSIVE AND MOBILE FRIENDLY STYLES
      ------------------------------------- */
      @media only screen and (max-width: 620px) {
      table[class=body] h1 {
      font-size: 28px !important;
      margin-bottom: 10px !important; }
      table[class=body] p,
      table[class=body] ul,
      table[class=body] ol,
      table[class=body] td,
      table[class=body] span,
      table[class=body] a {
      font-size: 16px !important; }
      table[class=body] .wrapper,
      table[class=body] .article {
      padding: 10px !important; }
      table[class=body] .content {
      padding: 0 !important; }
      table[class=body] .container {
      padding: 0 !important;
      width: 100% !important; }
      table[class=body] .main {
      border-left-width: 0 !important;
      border-radius: 0 !important;
      border-right-width: 0 !important; }
      table[class=body] .btn table {
      width: 100% !important; }
      table[class=body] .btn a {
      width: 100% !important; }
      table[class=body] .img-responsive {
      height: auto !important;
      max-width: 100% !important;
      width: auto !important; }}
      
      /* -------------------------------------
      PRESERVE THESE STYLES IN THE HEAD
      ------------------------------------- */
      @media all {
      .ExternalClass {
      width: 100%; }
      .ExternalClass,
      .ExternalClass p,
      .ExternalClass span,
      .ExternalClass font,
      .ExternalClass td,
      .ExternalClass div {
      line-height: 100%; }
      .apple-link a {
      color: inherit !important;
      font-family: inherit !important;
      font-size: inherit !important;
      font-weight: inherit !important;
      line-height: inherit !important;
      text-decoration: none !important; } 
      .btn-primary table td:hover {
      background-color: #34495e !important; }
      .btn-primary a:hover {
      background-color: #34495e !important;
      border-color: #34495e !important; } }
      
      </style>
      </head>
      <body class=''>
      <table border='0' cellpadding='0' cellspacing='0' class='body'>
      <tr>
      <td>&nbsp;</td>
      <td class='container'>
      <div class='content'>
      
      <!-- START CENTERED WHITE CONTAINER -->
      <span class='preheader'>This is preheader text. Some clients will show this text as a preview.</span>
      <table class='main'>
      
      <!-- START MAIN CONTENT AREA -->
      <tr>
      <td class='wrapper'>
      <table border='0' cellpadding='0' cellspacing='0'>
      <tr>
      <td>
      <img src='https://raw.githubusercontent.com/rich-iannone/pointblank/master/inst/graphics/pointblank_logo.png'>
      <p>Hi there, we've got some unfortunate news...</p>
      <p>", text_1, "</p>",
      table_summary,
      "<p>", text_2, "</p>",
      "</td>
      </tr>
      
      <!-- END MAIN CONTENT AREA -->
      </table>
      
      <!-- START FOOTER -->
      <div class='footer'>
      <table border='0' cellpadding='0' cellspacing='0'>
      <tr>
      <td class='content-block powered-by'>
      Sent by the <a href='https://github.com/rich-iannone/pointblank'>pointblank</a> R package.
      </td>
      </tr>
      </table>
      </div>
      <!-- END FOOTER -->
      
      <!-- END CENTERED WHITE CONTAINER -->
      </div>
      </td>
      <td>&nbsp;</td>
      </tr>
      </table>
      </body>
      </html>
      ")
  
  # Send the notification
  mailR::send.mail(
    from = sender,
    to = recipients,
    subject = subject,
    body = body,
    smtp = list(
      host.name = host,
      port = port, 
      user.name = user,            
      passwd = password,
      ssl = use_ssl),
    authenticate = authenticate,
    send = TRUE,
    html = TRUE,
    encoding = "utf-8")
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
