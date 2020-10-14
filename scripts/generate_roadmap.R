library(projmgr)
library(gt)
library(tidyverse)

myrepo <- create_repo_ref('rich-iannone', 'pointblank')
issues <- get_issues(myrepo, state = "open")
issues_df <- parse_issues(issues)

tasks_gt <- 
  issues_df %>%
  tidyr::separate(
    col = labels_name,
    into = c("difficulty", "effort", "priority", "type"),
    sep = ",",
    remove = FALSE
  ) %>%
  dplyr::filter(!grepl("Question", labels_name)) %>%
  dplyr::filter(!grepl("Help", labels_name)) %>%
  dplyr::filter(!is.na(milestone_title)) %>%
  dplyr::select(
    number, title, milestone_title,
    type, priority, difficulty, effort
  ) %>%
  dplyr::mutate(difficulty = gsub(".*?([1-3]).*", "\\1", difficulty)) %>%
  dplyr::mutate(effort = gsub(".*?([1-3]).*", "\\1", effort)) %>%
  dplyr::mutate(priority = gsub(".*?([1-3]).*", "\\1", priority)) %>%
  dplyr::mutate(priority = ifelse(grepl("[^1-3]", priority), 4, priority)) %>%
  dplyr::mutate(type = gsub(".*?Type: (.*?)\\\"\\)", "\\1", type)) %>%
  dplyr::arrange(milestone_title, desc(priority, type, difficulty, effort)) %>%
  dplyr::mutate_at(.vars = vars(priority, difficulty, effort), .funs = as.numeric) %>%
  dplyr::mutate(number = paste0("#", number)) %>%
  dplyr::as_tibble() %>%
  gt(
    rowname_col = "number",
    groupname_col = "milestone_title",
    id = "report"
  ) %>%
  tab_header(title = md("Upcoming Tasks and Milestones for **pointblank**")) %>%
  fmt_markdown(vars(title)) %>%
  data_color(
    columns = vars(priority, difficulty, effort),
    colors = scales::col_numeric(
      palette = c("#0e8a16", "#fbca04", "#d93f0b", "#c12e49"),
      domain = c(1, 4)
    ),
    alpha = 0.6,
    apply_to = "fill"
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "600"),
    locations = cells_body(columns = vars(priority, difficulty, effort))
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left", style = "dashed", weight = px(2), color = "#F0F1F3"),
    locations = cells_body(columns = vars(type))
  ) %>%
  cols_label(
    title = "",
    type = ""
  ) %>%
  cols_align("center", columns = vars(priority, difficulty, effort)) %>%
  cols_width(
    1 ~ px(60),
    vars(title) ~ px(400),
    vars(priority, difficulty, effort) ~ px(75),
    TRUE ~ px(140)
  ) %>%
  opt_table_font(font = google_font("IBM Plex Sans")) %>%
  opt_align_table_header("left") %>%
  opt_all_caps() %>%
  opt_table_outline(style = "none") %>%
  tab_options(
    heading.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.font.size = px(11),
    data_row.padding = px(4)
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(priority)),
    fn = function(x) {
      ifelse(x == "4", "♨︎", x)
    } 
  )

svg_object <-
  glue::glue(
    "<svg fill=\"none\" viewBox=\"0 0 900 2200\" width=\"900\" height=\"2200\" xmlns=\"http://www.w3.org/2000/svg\">
      <foreignObject width=\"100%\" height=\"100%\">
      <div xmlns=\"http://www.w3.org/1999/xhtml\">
    {gt::as_raw_html(tasks_gt)}
    </div>
    </foreignObject>
    </svg>
    "
  ) %>%
  as.character() %>%
  gsub("style>", ">", ., fixed = TRUE) %>% cat()

cat(svg_object, file = "./man/figures/pointblank-milestones.svg")
