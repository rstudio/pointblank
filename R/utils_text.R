reporting_languages <- c("en", "fr", "de", "it", "es")

translations_file <- system.file("text", "translations", package = "pointblank")

autobriefs_text <- yaml::read_yaml(translations_file)$autobriefs
agent_report <- yaml::read_yaml(translations_file)$agent_report
table_scan <- yaml::read_yaml(translations_file)$table_scan

#
# Text for autobriefs [`autobriefs_text`]
#

precondition_text <- unlist(autobriefs_text$precondition_text)

column_computed_text <- unlist(autobriefs_text$column_computed_text)

values_text <- unlist(autobriefs_text$values_text)

compare_expectation_text <- unlist(autobriefs_text$compare_expectation_text)

compare_failure_text <- unlist(autobriefs_text$compare_failure_text)

in_set_expectation_text <- unlist(autobriefs_text$in_set_expectation_text)

in_set_failure_text <- unlist(autobriefs_text$in_set_failure_text)

not_in_set_expectation_text <- unlist(autobriefs_text$not_in_set_expectation_text)

not_in_set_failure_text <- unlist(autobriefs_text$not_in_set_failure_text)

between_expectation_text <- unlist(autobriefs_text$between_expectation_text)

between_failure_text <- unlist(autobriefs_text$between_failure_text)

not_between_expectation_text <- unlist(autobriefs_text$not_between_expectation_text)

not_between_failure_text <- unlist(autobriefs_text$not_between_failure_text)

null_expectation_text <- unlist(autobriefs_text$null_expectation_text)

null_failure_text <- unlist(autobriefs_text$null_failure_text)

not_null_expectation_text <- unlist(autobriefs_text$not_null_expectation_text)

not_null_failure_text <- unlist(autobriefs_text$not_null_failure_text)

col_vals_expr_expectation_text <- unlist(autobriefs_text$col_vals_expr_expectation_text)

col_vals_expr_failure_text <- unlist(autobriefs_text$col_vals_expr_failure_text)

regex_expectation_text <- unlist(autobriefs_text$regex_expectation_text)

regex_failure_text <- unlist(autobriefs_text$regex_failure_text)

conjointly_expectation_text <- unlist(autobriefs_text$conjointly_expectation_text)

conjointly_failure_text <- unlist(autobriefs_text$conjointly_failure_text)

col_exists_expectation_text <- unlist(autobriefs_text$col_exists_expectation_text)

col_exists_failure_text <- unlist(autobriefs_text$col_exists_failure_text)

col_is_expectation_text <- unlist(autobriefs_text$col_is_expectation_text)

col_is_failure_text <- unlist(autobriefs_text$col_is_failure_text)

all_row_distinct_expectation_text <- unlist(autobriefs_text$all_row_distinct_expectation_text)

all_row_distinct_failure_text <- unlist(autobriefs_text$all_row_distinct_failure_text)

across_row_distinct_expectation_text <- unlist(autobriefs_text$across_row_distinct_expectation_text)

across_row_distinct_failure_text <- unlist(autobriefs_text$across_row_distinct_failure_text)

col_schema_match_expectation_text <- unlist(autobriefs_text$col_schema_match_expectation_text)

col_schema_match_failure_text <- unlist(autobriefs_text$col_schema_match_failure_text)

#
# Text for agent report [`agent_report`]
#

pointblank_validation_title_text <- unlist(agent_report$pointblank_validation_title_text)

pointblank_validation_plan_text <- unlist(agent_report$pointblank_validation_plan_text)

no_interrogation_performed_text <- unlist(agent_report$no_interrogation_performed_text)

report_col_step <- unlist(agent_report$report_col_step)

report_col_steps <- unlist(agent_report$report_col_steps)

report_col_columns <- unlist(agent_report$report_col_steps)

report_col_values <- unlist(agent_report$report_col_values)

report_col_units <- unlist(agent_report$report_col_units)

report_column_schema <- unlist(agent_report$report_column_schema)

report_r_col_types <- unlist(agent_report$report_r_col_types)

report_r_sql_types <- unlist(agent_report$report_r_sql_types)

#
# Text for table scan (ts) report (`table_scan`)
#

nav_title_ts <- unlist(table_scan$nav_title_ts)

nav_overview_ts <- unlist(table_scan$nav_overview_ts)

nav_variables_ts <- unlist(table_scan$nav_variables_ts)

nav_interactions_ts <- unlist(table_scan$nav_interactions_ts)

nav_correlations_ts <- unlist(table_scan$nav_correlations_ts)

nav_missing_values_ts <- unlist(table_scan$nav_missing_values_ts)

nav_sample_values_ts <- unlist(table_scan$nav_sample_values_ts)

section_title_overview_of_ts <- unlist(table_scan$section_title_overview_of_ts)

section_title_overview_ts <- nav_overview_ts
section_title_variables_ts <- nav_variables_ts
section_title_interactions_ts <- nav_interactions_ts
section_title_correlations_ts <- nav_correlations_ts
section_title_missing_values_ts <- nav_missing_values_ts
section_title_sample_values_ts <- nav_sample_values_ts


button_label_overview_overview_ts <- nav_overview_ts

button_label_overview_reproducibility_ts <- unlist(table_scan$button_label_overview_reproducibility_ts)

tab_label_variables_statistics_ts <- unlist(table_scan$tab_label_variables_statistics_ts)

tab_label_variables_common_values_ts <- unlist(table_scan$tab_label_variables_common_values_ts)

tab_label_variables_max_min_slices_ts <- unlist(table_scan$tab_label_variables_max_min_slices_ts)

subsection_title_overview_table_overview <- unlist(table_scan$subsection_title_overview_table_overview)

subsection_title_overview_column_types <- unlist(table_scan$subsection_title_overview_column_types)

subsection_title_overview_reproducibility_information <- unlist(table_scan$subsection_title_overview_reproducibility_information)

subsection_title_variables_quantile_statistics <- unlist(table_scan$subsection_title_variables_quantile_statistics)

subsection_title_variables_descriptive_statistics <- unlist(table_scan$subsection_title_variables_descriptive_statistics)

subsection_title_variables_common_values <- tab_label_variables_common_values_ts

subsection_title_variables_maximum_values <- unlist(table_scan$subsection_title_variables_maximum_values)

subsection_title_variables_minimum_values <- unlist(table_scan$subsection_title_variables_minimum_values)

subsection_title_variables_string_lengths <- unlist(table_scan$subsection_title_variables_string_lengths)

subsection_title_variables_histogram <- unlist(table_scan$subsection_title_variables_histogram)

btn_toggle_details <- unlist(table_scan$btn_toggle_details)

tbl_lab_columns <- unlist(table_scan$tbl_lab_columns)

tbl_lab_rows <- unlist(table_scan$tbl_lab_rows)

tbl_lab_NAs <- unlist(table_scan$tbl_lab_NAs)

tbl_lab_duplicate_rows <- unlist(table_scan$tbl_lab_duplicate_rows)

tbl_lab_scan_build_time <- unlist(table_scan$tbl_lab_scan_build_time)

tbl_lab_pointblank_version <- unlist(table_scan$tbl_lab_pointblank_version)

tbl_lab_r_version <- unlist(table_scan$tbl_lab_r_version)

tbl_lab_system_os <- unlist(table_scan$tbl_lab_system_os)

tbl_lab_distinct <- unlist(table_scan$tbl_lab_distinct)

tbl_lab_mean <- unlist(table_scan$tbl_lab_mean)

tbl_lab_minimum <- unlist(table_scan$tbl_lab_minimum)

tbl_lab_maximum <- unlist(table_scan$tbl_lab_maximum)

tbl_lab_5_percentile <- unlist(table_scan$tbl_lab_5_percentile)

tbl_lab_median <- unlist(table_scan$tbl_lab_median)

tbl_lab_95_percentile <- unlist(table_scan$tbl_lab_95_percentile)

tbl_lab_range <- unlist(table_scan$tbl_lab_range)

tbl_lab_variance <- unlist(table_scan$tbl_lab_variance)

tbl_lab_standard_deviation <- unlist(table_scan$tbl_lab_standard_deviation)

tbl_lab_cov <- unlist(table_scan$tbl_lab_cov)

tbl_lab_value <- unlist(table_scan$tbl_lab_value)

tbl_lab_count <- unlist(table_scan$tbl_lab_count)

tbl_lab_frequency <- unlist(table_scan$tbl_lab_frequency)

plot_lab_count <- tbl_lab_count

plot_lab_string_length <- unlist(table_scan$plot_lab_string_length)

other_values_text <- unlist(table_scan$other_values_text)

footer_text_fragment <- unlist(table_scan$footer_text_fragment)
