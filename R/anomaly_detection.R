anomaly_detection_ts <- function(data_tbl,
                                 time_col,
                                 value_col) {
  
  data_tbl <-
    data_tbl %>%
    dplyr::select(dplyr::all_of(c(time_col, value_col))) %>%
    dplyr::rename(time = 1, value = 2) %>%
    dplyr::filter(!is.na(time)) %>%
    dplyr::filter(!is.na(value))
  
  n_rows_data_tbl <-
    data_tbl %>%
    dplyr::count() %>%
    dplyr::pull(n)
  
  mean <- get_tbl_value_mean(data_tbl)
  sd <- get_tbl_value_sd(data_tbl)
  
  # Create a standardized version of the input data set; this centers the
  # data (yielding a `z` value of approximately 0) and scales the data so that
  # the standard deviation is 1
  data_tbl_standardized <-
    data_tbl %>%
    dplyr::select(time, value) %>%
    dplyr::mutate(z = (value - mean) / sd)
  
  # Sample random datapoints from the db table; this is needed for the fit
  # model which will be used for detrending the standardized dataset
  random_rows <-
    c(
      1:5,
      sample(
        x = seq_len(n_rows_data_tbl),
        size = floor(0.05 * n_rows_data_tbl),
        replace = FALSE
      ),
      (n_rows_data_tbl - 5):n_rows_data_tbl
    ) %>%
    sort() %>%
    unique()
  
  # Collect random rows from the standardized dataset
  data_tbl_standardized_sample <-
    data_tbl_standardized %>%
    dplyr::filter(dplyr::row_number() %in% random_rows) %>%
    dplyr::collect()
  
  # Prepare a data frame suitable for spline generation via
  # the `mgcv::gam()` function
  data_tbl_gam_df <-
    data_tbl_standardized_sample %>%
    dplyr::mutate(time = as.integer(time)) %>%
    dplyr::rename(x = time, y = z) %>%
    dplyr::select(-value) %>%
    as.data.frame()
  
  # Generate a GAM model for the sampled points
  gam_model <- mgcv::gam(y ~ s(x), data = data_tbl_gam_df)
  
  # Create a table with the predicted values
  gam_model_tbl <-
    dplyr::tibble(
      time = data_tbl_standardized_sample$time,
      fit = unname(mgcv::predict.gam(gam_model)),
    )
  
  # # Plot the sampled data points along with the model fit
  # ggplot(data_tbl_standardized_sample) +
  #   geom_point(aes(x = time, y = z)) +
  #   geom_line(aes(x = time, y = fit), data = gam_model_tbl, color = "blue")
  
  # Perform an analysis on the standardized data; this focuses only on the
  # unchanged `time` variable, giving us columns that indicate how many data
  # points are in each time grouping
  time_interval_n_analysis <- add_time_granularity_cols(data_tbl_standardized)
  
  # This provides a time interval summary and should be
  # used to detemrine which time granularity to use
  time_interval_summary <-
    time_interval_n_analysis %>%
    dplyr::select(starts_with("n_")) %>%
    dplyr::group_by() %>%
    dplyr::summarize_all(
      .funs = list(
        ~ mean(., na.rm = TRUE),
        ~ min(., na.rm = TRUE),
        ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::collect() %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_prefix = "n_",
      names_to = c("timespan", "stat"),
      values_to = "values",
      names_sep = "_"
    ) %>%
    tidyr::pivot_wider(
      names_from = "stat",
      values_from = "values"
    )
  
  # Automatically select the time granularity with the highest
  # average number of data points above 20 per window
  time_int_gt_20 <-
    time_interval_summary %>% dplyr::filter(mean >= 20)
  
  if (nrow(time_int_gt_20) < 1) {
    stop(
      "There aren't enough data points in any of the possible time granularity types.",
      call. = FALSE
    )
  }
  
  time_granularity_list <-
    time_int_gt_20 %>% 
    dplyr::arrange(dplyr::desc(mean)) %>%
    utils::head(1) %>%
    as.list()
  
  timespan_select <- time_granularity_list$timespan
  
  # This provides the min and max of `z`
  z_interval_n_analysis <-
    time_interval_n_analysis %>%
    dplyr::group_by() %>%
    dplyr::summarize(
      min = min(z, na.rm = TRUE),
      max = max(z, na.rm = TRUE)
    )
  
  z_interval_breaks <-
    c(
      seq(from = 0, to =  1, by = 0.5),
      seq(from = 0, to = -1, by = -0.5)
    ) %>%
    unique() %>%
    sort()
  
  # Simplify the `time_interval_n_analysis` by only keeping key columns
  data_tbl <-
    time_interval_n_analysis %>%
    dplyr::select(1, 2, z, dplyr::all_of(paste0("time_", timespan_select)))
  
  # Create a local lookup table that contains all of the time groups and
  # an empty column with the `model_fit` values; this is important because the
  # sampling used to create the spline fit likely won't contain values for all
  # time groups, but, we can use this template to account for all available
  # time groups and fill in missing values
  time_groups <-
    data_tbl %>%
    dplyr::select(starts_with("time_")) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::mutate(model_fit = NA_real_)
  
  # Create a piecewise representation of the GAM model. This necessary (but
  # also not very lossy approximation) makes it possible to join the GAM
  # predictions to the DB data
  gam_model_piecewise_tbl <-
    gam_model_tbl %>%
    add_time_granularity_cols() %>%
    dplyr::select(1, 2, ends_with(timespan_select)) %>%
    dplyr::group_by(time_1d) %>%
    dplyr::summarize(
      model_fit = mean(fit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::bind_rows(time_groups) %>%
    dplyr::arrange(time_1d) %>%
    tidyr::fill(model_fit) %>%
    dplyr::distinct()
  
  data_tbl <-
    data_tbl %>%
    dplyr::left_join(
      gam_model_piecewise_tbl,
      by = paste0("time_", timespan_select),
      copy = TRUE
    ) %>%
    dplyr::mutate(z_detrend = model_fit - z)
  
  encoded_windows <-
    data_tbl %>%
    dplyr::group_by(time_1d) %>%
    dplyr::summarize(
      mean = mean(z_detrend, na.rm = TRUE)
    ) %>%
    dplyr::mutate(code_n_w = dplyr::case_when(
      mean < -2.0 ~ 1,
      mean < -1.5 ~ 2,
      mean < -1.0 ~ 3,
      mean < -0.5 ~ 4,
      mean < -0.0 ~ 5,
      mean <  0.5 ~ 6,
      mean <  1.0 ~ 7,
      mean <  1.5 ~ 8,
      mean <  2.0 ~ 9,
      mean < 9999 ~ 10
    )) %>%
    dplyr::mutate(code_l_w = dplyr::case_when(
      mean < -2.0 ~ "A",
      mean < -1.5 ~ "B",
      mean < -1.0 ~ "C",
      mean < -0.5 ~ "D",
      mean < -0.0 ~ "E",
      mean <  0.5 ~ "F",
      mean <  1.0 ~ "G",
      mean <  1.5 ~ "H",
      mean <  2.0 ~ "I",
      mean < 9999 ~ "J"
    ))
  
  encoded_points <-
    data_tbl %>%
    dplyr::mutate(code_n_p = dplyr::case_when(
      z_detrend < -2.0 ~ 1,
      z_detrend < -1.5 ~ 2,
      z_detrend < -1.0 ~ 3,
      z_detrend < -0.5 ~ 4,
      z_detrend < -0.0 ~ 5,
      z_detrend <  0.5 ~ 6,
      z_detrend <  1.0 ~ 7,
      z_detrend <  1.5 ~ 8,
      z_detrend <  2.0 ~ 9,
      z_detrend < 9999 ~ 10
    )) %>%
    dplyr::mutate(code_l_p = dplyr::case_when(
      z_detrend < -2.0 ~ "A",
      z_detrend < -1.5 ~ "B",
      z_detrend < -1.0 ~ "C",
      z_detrend < -0.5 ~ "D",
      z_detrend < -0.0 ~ "E",
      z_detrend <  0.5 ~ "F",
      z_detrend <  1.0 ~ "G",
      z_detrend <  1.5 ~ "H",
      z_detrend <  2.0 ~ "I",
      z_detrend < 9999 ~ "J"
    ))
  
  unified_data <-
    encoded_points %>%
    dplyr::left_join(
      encoded_windows, by = "time_1d"
    ) %>%
    dplyr::mutate(code_n_dist = abs(code_n_p - code_n_w)) %>%
    dplyr::mutate(
      sd_z_detrend_ul = 2,
      sd_z_detrend_ll = -2
    ) %>%
    dplyr::mutate(outlier_sd_z = dplyr::case_when(
      z_detrend >= sd_z_detrend_ul | z_detrend <= sd_z_detrend_ll ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(outlier_dist = dplyr::case_when(
      code_n_dist > 2 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(outlier = dplyr::if_else(
      outlier_sd_z | outlier_dist, TRUE, FALSE
    )) %>%
    dplyr::mutate(`pb_is_good_` = !outlier)
  
  
  # plot_data <-
  #   unified_data %>%
  #   dplyr::select(1, 2, z, z_detrend, sd_z_detrend_ul, sd_z_detrend_ll, outlier) %>%
  #   dplyr::collect()
  # 
  # # Plot of actual data values with seeded values and marked outliers
  # ggplot() +
  #   geom_point(
  #     data = plot_data %>% dplyr::filter(outlier),
  #     aes(x = time, y = value), color = "red"
  #   ) +
  #   geom_point(
  #     data = plot_data %>% dplyr::filter(!outlier),
  #     aes(x = time, y = value)
  #   ) +
  #   scale_x_datetime(date_breaks = "1 month")
  # 
  # # Plot of standardized data values with seeded values and marked outliers
  # ggplot() +
  #   geom_point(
  #     data = plot_data %>% dplyr::filter(outlier),
  #     aes(x = time, y = z_detrend), color = "red"
  #   ) +
  #   geom_point(
  #     data = plot_data %>% dplyr::filter(!outlier),
  #     aes(x = time, y = z_detrend)
  #   ) +
  #   geom_hline(yintercept = 1, color = "gray") +
  #   geom_hline(yintercept = -1, color = "gray") +
  #   geom_line(data = plot_data, aes(x = time, y = sd_z_detrend_ul), color = "red") +
  #   geom_line(data = plot_data, aes(x = time, y = sd_z_detrend_ll), color = "red") +
  #   scale_y_continuous(breaks = seq(-5.0, 5.0, 0.5)) +
  #   scale_x_datetime(date_breaks = "1 month")
  
  
  unified_data
}

get_tbl_value_mean <- function(data_tbl) {
  
  data_tbl %>%
    dplyr::summarize(value_mean = mean(value, na.rm = TRUE)) %>%
    dplyr::pull(value_mean)
}

get_tbl_value_sd <- function(data_tbl) {
  
  mean <- get_tbl_value_mean(data_tbl = data_tbl)
  
  variance <-
    data_tbl %>%
    dplyr::select(value) %>%
    dplyr::mutate(
      "diff" = (!!mean - value)^2
    ) %>%
    dplyr::group_by() %>%
    dplyr::summarize(
      "var"  = mean(diff, na.rm = TRUE)
    ) %>%
    dplyr::pull(var)
  
  (abs(variance))^0.5
}

add_time_granularity_cols <- function(data_tbl) {
  
  data_tbl %>%
    dplyr::mutate(
      time_1d = substr(as.character(time), 1, 10),
      time_12h =
        dplyr::case_when(
          substr(as.character(time), 12, 13) %in%
            c("00", "01", "02", "03", "04", "05", "06",
              "07", "08", "09", "10", "11") ~
            paste0(substr(as.character(time), 1, 11), "00"),
          substr(as.character(time), 12, 13) %in%
            c("12", "13", "14", "15", "16", "17", "18",
              "19", "20", "21", "22", "23") ~
            paste0(substr(as.character(time), 1, 11), "12")
        ),
      time_8h =
        dplyr::case_when(
          substr(as.character(time), 12, 13) %in%
            c("00", "01", "02", "03", "04", "05", "06", "07") ~
            paste0(substr(as.character(time), 1, 11), "00"),
          substr(as.character(time), 12, 13) %in%
            c("08", "09", "10", "11", "12", "13", "14", "15") ~
            paste0(substr(as.character(time), 1, 11), "08"),
          substr(as.character(time), 12, 13) %in%
            c("16", "17", "18", "19", "20", "21", "22", "23") ~
            paste0(substr(as.character(time), 1, 11), "16")
        ),
      time_6h =
        dplyr::case_when(
          substr(as.character(time), 12, 13) %in%
            c("00", "01", "02", "03", "04", "05") ~
            paste0(substr(as.character(time), 1, 11), "00"),
          substr(as.character(time), 12, 13) %in%
            c("06", "07", "08", "09", "10", "11") ~
            paste0(substr(as.character(time), 1, 11), "06"),
          substr(as.character(time), 12, 13) %in%
            c("12", "13", "14", "15", "16", "17") ~
            paste0(substr(as.character(time), 1, 11), "12"),
          substr(as.character(time), 12, 13) %in%
            c("18", "19", "20", "21", "22", "23") ~
            paste0(substr(as.character(time), 1, 11), "16")
        ),
      time_1h = substr(as.character(time), 1, 13),
      time_30m =
        dplyr::case_when(
          substr(as.character(time), 15, 15) %in% c("3", "4", "5") ~
            paste0(substr(as.character(time), 1, 14), "3"),
          substr(as.character(time), 15, 15) %in% c("0", "1", "2") ~
            paste0(substr(as.character(time), 1, 14), "0")
        ),
      time_15m =
        dplyr::case_when(
          substr(as.character(time), 15, 16) %in%
            c("00", "01", "02", "03", "04", "05", "06", "07",
              "08", "09", "10", "11", "12", "13", "14") ~
            paste0(substr(as.character(time), 1, 14), "00"),
          substr(as.character(time), 15, 16) %in%
            c("15", "16", "17", "18", "19", "20", "21", "22",
              "23", "24", "25", "26", "27", "28", "29") ~
            paste0(substr(as.character(time), 1, 14), "15"),
          substr(as.character(time), 15, 16) %in%
            c("30", "31", "32", "33", "34", "35", "36", "37",
              "38", "39", "40", "41", "42", "43", "44") ~
            paste0(substr(as.character(time), 1, 14), "30"),
          substr(as.character(time), 15, 16) %in%
            c("45", "46", "47", "48", "49", "50", "51", "52",
              "53", "54", "55", "56", "57", "58", "59") ~
            paste0(substr(as.character(time), 1, 14), "45")
        ),
      time_1m = substr(as.character(time), 1, 16)
    ) %>%
    dplyr::group_by(time_1d) %>%
    dplyr::mutate(n_1d = n()) %>%
    dplyr::group_by(time_12h) %>%
    dplyr::mutate(n_12h = n()) %>%
    dplyr::group_by(time_8h) %>%
    dplyr::mutate(n_8h = n()) %>%
    dplyr::group_by(time_6h) %>%
    dplyr::mutate(n_6h = n()) %>%
    dplyr::group_by(time_1h) %>%
    dplyr::mutate(n_1h = n()) %>%
    dplyr::group_by(time_30m) %>%
    dplyr::mutate(n_30m = n()) %>%
    dplyr::group_by(time_15m) %>%
    dplyr::mutate(n_15m = n()) %>%
    dplyr::group_by(time_1m) %>%
    dplyr::mutate(n_1m = n()) %>%
    dplyr::ungroup()
}
