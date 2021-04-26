library(projmgr)
library(gt)
library(tidyverse)
library(png)

myrepo <- create_repo_ref("rich-iannone", "pointblank")
issues <- get_issues(myrepo, state = "open")
issues_df <- parse_issues(issues)

tbl <- 
  issues_df %>%
  dplyr::as_tibble() %>%
  dplyr::filter(!is.na(milestone_title)) %>%
  tidyr::separate(
    col = labels_name,
    into = c("difficulty", "effort", "priority", "type"),
    sep = ",",
    remove = FALSE
  ) %>%
  tidyr::separate(
    col = milestone_title,
    into = c("major", "minor", "patch"),
    sep = "\\.",
    remove = FALSE
  ) %>%
  dplyr::mutate(major = gsub("v", "", major)) %>% 
  dplyr::mutate_at(.vars = vars(major, minor, patch), .funs = as.integer) %>%
  dplyr::mutate(type = ifelse(labels_name == "Release", "✈ Release", type)) %>%
  dplyr::filter(!grepl("Question", labels_name)) %>%
  dplyr::filter(!grepl("Help", labels_name)) %>%
  dplyr::filter(!grepl("Good", labels_name)) %>%
  dplyr::select(
    number, title, milestone_title, major, minor, patch,
    type, priority, difficulty, effort
  ) %>%
  dplyr::mutate(difficulty = gsub(".*?([1-3]).*", "\\1", difficulty)) %>%
  dplyr::mutate(effort = gsub(".*?([1-3]).*", "\\1", effort)) %>%
  dplyr::mutate(priority = gsub(".*?([1-3]).*", "\\1", priority)) %>%
  dplyr::mutate(priority = ifelse(grepl("[^1-3]", priority), 4, priority)) %>%
  dplyr::mutate(type = gsub(".*?Type: (.*?)\\\"\\)", "\\1", type)) %>%
  dplyr::mutate_at(.vars = vars(priority, difficulty, effort), .funs = as.numeric) %>%
  dplyr::arrange(
    major,
    minor,
    dplyr::desc(priority),
    type,
    dplyr::desc(difficulty),
    dplyr::desc(effort)
  ) %>%
  dplyr::mutate(number = paste0("#", number)) %>%
  dplyr::select(-c(major, minor, patch))

gt_tbl <- 
  tbl %>%
  gt(
    rowname_col = "number",
    groupname_col = "milestone_title",
    id = "report"
  ) %>%
  tab_header(title = md("Upcoming Tasks and Milestones for **pointblank**")) %>%
  fmt_markdown(vars(title)) %>%
  fmt_missing(vars(priority, difficulty, effort), missing_text = "") %>%
  data_color(
    columns = vars(priority, difficulty, effort),
    colors = scales::col_numeric(
      palette = c("#0e8a16", "#fbca04", "#d93f0b", "#c12e49"),
      domain = c(1, 4),
      na.color = "#FFFFFF"
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
  tab_style(
    style = cell_borders(
      sides = "right", style = "solid", weight = px(1), color = "#F0F1F3"),
    locations = cells_body(columns = vars(priority, difficulty))
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
  ) %>%
  text_transform(
    locations = cells_body(columns = vars(priority)),
    fn = function(x) {
      ifelse(x == "4", "♨︎", x)
    } 
  ) %>%
  tab_style(
    style = "height: 50px",
    locations = cells_body(columns = TRUE, rows = !grepl("Release", type))
  ) %>%
  tab_style(
    style = "
    height: 75px;
    background: linear-gradient(180deg, #F5D6F8, #9DFEE5, #CEF5FB);
    background-size: 100% 100%;
    -webkit-animation: AnimationName 5s ease infinite;
    -moz-animation: AnimationName 5s ease infinite;
    -o-animation: AnimationName 5s ease infinite;
    animation: AnimationName 5s ease infinite;",
    locations = cells_body(
      columns = vars(title, type, priority, difficulty, effort),
      rows = grepl("Release", type)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      style = "solid",
      weight = "0"
    ),
    locations = cells_body(
      columns = vars(title, type, priority, difficulty, effort),
      rows = grepl("Release", type)
    )
  )

# Save image with `gtsave()` and obtain the image dimensions
temp_file <- tempfile(fileext = ".png")
gtsave(gt_tbl, filename = temp_file)
tbl_img <- png::readPNG(temp_file, native = TRUE, info = TRUE)
tbl_dim <- attr(tbl_img, "info")[["dim"]]
tbl_w <- ceiling(tbl_dim[1] / 2)
tbl_h <- ceiling(tbl_dim[2] / 2) + 5

# Generation of SVG
svg_object <-
  glue::glue(
    "<svg fill=\"none\" viewBox=\"0 0 {tbl_w} {tbl_h}\" width=\"{tbl_w}\" height=\"{tbl_h}\" xmlns=\"http://www.w3.org/2000/svg\">

     <foreignObject width=\"100%\" height=\"100%\">
     <div xmlns=\"http://www.w3.org/1999/xhtml\">
    {gt::as_raw_html(gt_tbl)}
    </div>
    </foreignObject>
    </svg>
    "
  ) %>%
  as.character() %>%
  gsub("style>", ">", ., fixed = TRUE) %>%
  gsub("<p>", "<p style='margin:0'>", ., fixed = TRUE) %>%
  gsub(
    "; width: 0px\">", 
"; width: 0px; 
@-webkit-keyframes AnimationName {0% {background-position:50% 0%} 50% {background-position:51% 100%} 100% {background-position:50% 0%}}
@-moz-keyframes AnimationName {0% {background-position:50% 0%} 50% {background-position:51% 100%} 100% {background-position:50% 0%}}
@-o-keyframes AnimationName {0% {background-position:50% 0%} 50% {background-position:51% 100%} 100% {background-position:50% 0%}}
@keyframes AnimationName {0% {background-position:50% 0%} 50% {background-position:51% 100%} 100% {background-position:50% 0%}}
\">
",
., fixed = TRUE)


cat(svg_object, file = "./man/figures/pointblank-milestones.svg")
