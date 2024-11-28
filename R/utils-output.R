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


# nocov start

create_rds_tbl <- function(path = NULL, files = NULL) {

  agent_rds_files <-
    fs::dir_ls(
      path = path,
      regexp = paste0(
        "agent.*?[0-9]{4}-[0-9]{2}-[0-9]{2}_",
        "[0-9]{2}-[0-9]{2}-[0-9]{2}.rds$"
      )
    ) %>%
    basename()

  agent_rds_tbl_names <-
    unique(
      gsub(
        paste0(
          "^agent-(.*?)-[0-9]{4}-[0-9]{2}-[0-9]{2}_",
          "[0-9]{2}-[0-9]{2}-[0-9]{2}.rds"
        ),
        "\\1",
        agent_rds_files
      )
    )

  informant_rds_files <-
    fs::dir_ls(
      path = path,
      regexp = paste0(
        "informant.*?[0-9]{4}-[0-9]{2}-[0-9]{2}_",
        "[0-9]{2}-[0-9]{2}-[0-9]{2}.rds$"
      ),
    ) %>%
    basename()

  informant_rds_tbl_names <-
    unique(
      gsub(
        paste0(
          "^informant-(.*?)-[0-9]{4}-[0-9]{2}-[0-9]{2}_",
          "[0-9]{2}-[0-9]{2}-[0-9]{2}.rds"
        ),
        "\\1",
        informant_rds_files
      )
    )

  shared_tbl_names <- unique(c(agent_rds_tbl_names, informant_rds_tbl_names))

  main_tbl <-
    dplyr::tibble(
      tbl_name = character(0),
      group = character(0),
      order = integer(0),
      validation_files = list(),
      information_files = list()
    )

  agent_tbl0 <- informant_tbl0 <-
    dplyr::tibble(
      filename = character(0),
      time_str = character(0)
    )

  for (i in seq_along(shared_tbl_names)) {

    main_tbl[i, "tbl_name"] <- shared_tbl_names[i]

    # Initialize `agent_tbl`
    agent_tbl <- agent_tbl0

    if (shared_tbl_names[i] %in% agent_rds_tbl_names) {

      agent_rds_name <-
        agent_rds_files[
          grepl(paste0("agent-", shared_tbl_names[i]), agent_rds_files)
        ]

      for (j in seq_along(agent_rds_name)) {
        agent_tbl[j, "filename"] <- agent_rds_name[j]
        agent_tbl[j, "time_str"] <-
          gsub(
            ".*([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}).*",
            "\\1",
            agent_rds_name[j]
          )
      }

      agent_tbl <-
        agent_tbl %>%
        dplyr::arrange(dplyr::desc(time_str))
    }

    # Initialize `informant_tbl`
    informant_tbl <- informant_tbl0

    if (shared_tbl_names[i] %in% informant_rds_tbl_names) {

      informant_rds_name <-
        informant_rds_files[
          grepl(paste0("informant-", shared_tbl_names[i]), informant_rds_files)
        ]

      for (j in seq_along(informant_rds_name)) {
        informant_tbl[j, "filename"] <- informant_rds_name[j]
        informant_tbl[j, "time_str"] <-
          gsub(
            ".*([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}).*",
            "\\1",
            informant_rds_name[j]
          )
      }

      informant_tbl <-
        informant_tbl %>%
        dplyr::arrange(dplyr::desc(time_str))
    }

    main_tbl[[i, "validation_files"]] <- list(agent_tbl)
    main_tbl[[i, "information_files"]] <- list(informant_tbl)
  }

  main_tbl
}

get_rds_tbl_val_files_tbl <- function(rds_tbl, tbl_name) {

  if (!(tbl_name %in% rds_tbl$tbl_name)) {

    stop("The `tbl_name` is not available in the `rds_tbl`.")
  }

  rds_tbl[rds_tbl$tbl_name == tbl_name, "validation_files"][[1]][[1]]
}

get_rds_tbl_info_files_tbl <- function(rds_tbl, tbl_name) {

  if (!(tbl_name %in% rds_tbl$tbl_name)) {

    stop("The `tbl_name` is not available in the `rds_tbl`.")
  }

  rds_tbl[rds_tbl$tbl_name == tbl_name, "information_files"][[1]][[1]]
}

check_quarto <- function() {
  Sys.getenv("QUARTO_BIN_PATH") != ""
}

# nocov end
