#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#

#' Execute all agent and informant YAML tasks
#' 
#' @description
#' The `yaml_exec()` function takes all relevant **pointblank** YAML files in a
#' directory and executes them. Execution involves interrogation of agents for
#' YAML agents and incorporation of informants for YAML informants. Under the
#' hood, this uses [yaml_agent_interrogate()] and [yaml_informant_incorporate()]
#' and then [x_write_disk()] to save the processed objects to an output
#' directory. These written artifacts can be read in at any later time with the
#' [x_read_disk()] function or the [read_disk_multiagent()] function.
#' 
#' The output RDS files are named according to the object type processed, the
#' target table, and the date-time of processing. For convenience and
#' modularity, this setup is ideal when a table store YAML file (typically named
#' `"tbl_store.yml"` and produced via the [tbl_store()] and [yaml_write()]
#' workflow) is available in the directory, and when table-prep formulas are
#' accessed by name through [tbl_source()].
#' 
#' A typical directory of files set up for execution in this way might have the
#' following contents:
#' 
#' - a `"tbl_store.yml"` file for holding table-prep formulas (created with
#' [tbl_store()] and written to YAML with [yaml_write()])
#' - one or more YAML *agent* files to validate tables (ideally using
#' [tbl_source()])
#' - one or more YAML *informant* files to provide refreshed metadata on tables
#' (again, using [tbl_source()] to reference table preparations is ideal)
#' - an output folder (default is `"output"`) to save serialized versions of
#' processed agents and informants
#' 
#' @param path The path that contains the YAML files for agents and informants.
#' @param files A vector of YAML files to use in the execution workflow. By
#'   default, `yaml_exec()` will attempt to process every valid YAML file in
#'   `path` but supplying a vector here limits the scope to the specified files.
#' @param write_to_disk Should the execution workflow include a step that writes
#'   output files to disk? This internally calls [x_write_disk()] to write RDS
#'   files and uses the base filename of the agent/informant YAML file as part
#'   of the output filename, appending the date-time to the basename.
#' @param output_path The output path for any generated output files. By
#'   default, this will be a subdirectory of the provided `path` called
#'   `"output"`.
#' @param keep_tbl,keep_extracts For agents, the table may be kept if it is a
#'   data frame object (databases tables will never be pulled for storage) and
#'   *extracts*, collections of table rows that failed a validation step, may
#'   also be stored. By default, both of these options are set to `FALSE`.
#'   
#' @family pointblank YAML
#' @section Function ID:
#' 11-8
#' 
#' @export
yaml_exec <- function(path = NULL,
                      files = NULL,
                      write_to_disk = TRUE, 
                      output_path = file.path(path, "output"),
                      keep_tbl = FALSE,
                      keep_extracts = FALSE) {
  
  # If `path` isn't provided then the working directory
  # is the path containing the input files
  if (is.null(path)) {
    path <- fs::path_abs(fs::path_wd())
  } else {
    
    initial_wd <- fs::path_abs(fs::path_wd())
    wd_path <- fs::path_abs(path)
    
    if (!fs::dir_exists(wd_path)) {
      stop(
        "The `path` provided (", as.character(wd_path), ") does not exist.",
        call. = FALSE
      )
    }
    
    if (initial_wd != wd_path) {
      setwd(path)
      on.exit(setwd(initial_wd))
    }
    
    path <- wd_path
  }
  
  # Construct paths to files
  if (!is.null(files)) {
    files_paths <- fs::path(fs::path_wd(), files)
  } else {
    files_paths <- fs::path(fs::path_wd(), fs::dir_ls(regexp = ".ya?ml$"))
  }
  
  # Normalize the output path
  if (is.null(output_path)) {
    output_path <- fs::path_norm(output_path)
  } else {
    if (!fs::is_absolute_path(output_path)) {
      output_path <- fs::path_norm(fs::path(initial_wd, output_path))
    }
  }
  
  agent_file_paths <- c()
  informant_file_paths <- c()
  
  # Determine which of the files are agents
  for (file_path in files_paths) {
    
    y <- yaml::read_yaml(file_path)
    
    if (all(c("read_fn", "tbl_name", "locale", "steps") %in% names(y))) {
      agent_file_paths <- c(agent_file_paths, file_path)
    }
  }
  
  # Determine which of the files are informants
  for (file_path in files_paths) {
    
    y <- yaml::read_yaml(file_path)
    
    if (all(c("table", "columns") %in% names(y))) {
      informant_file_paths <- c(informant_file_paths, file_path)
    }
  }
  
  if (length(agent_file_paths) > 0) {
    
    for (agent_yml_file in agent_file_paths) {
      
      agent <- yaml_agent_interrogate(agent_yml_file)
      
      if (write_to_disk) {

        fs::dir_create(path = output_path)
        
        # Construct the filename/path for the output RDS file
        file_name <- 
          fs::path(
            output_path,
            paste0(basename(tools::file_path_sans_ext(agent_yml_file)), ".rds")
          )
        
        # Write the file to disk, affixing the date and time to
        # the filename for ease of parsing by other functions
        x_write_disk(
          x = agent,
          filename = affix_datetime(
            filename = file_name,
            delimiter = "-"
          ),
          keep_tbl = keep_tbl,
          keep_extracts = keep_extracts
        )
      }
    }
  }

  if (length(informant_file_paths) > 0) {
    
    for (informant_yml_file in informant_file_paths) {
      
      informant <- yaml_informant_incorporate(informant_yml_file)
      
      if (write_to_disk) {
        
        fs::dir_create(path = output_path)
        
        # Construct the filename/path for the output RDS file
        file_name <- 
          fs::path(
            output_path,
            paste0(
              basename(
                tools::file_path_sans_ext(informant_yml_file)
              ),
              ".rds"
            )
          )
        
        # Write the file to disk, affixing the date and time to
        # the filename for ease of parsing by other functions
        x_write_disk(
          x = informant,
          filename = affix_datetime(
            filename = file_name,
            delimiter = "-"
          )
        )
      }
    }
  }
  
  invisible(NULL)
}
