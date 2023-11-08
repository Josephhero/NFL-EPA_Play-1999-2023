library(tidyverse)
library(nflverse)
library(gt)
library(gtExtras)

# Data-----
source("../../jefe_functions/functions.R")

epa_raw <- load_pbp(seasons = 1999:2023)

nfl_epa <- epa_raw |> 
  filter(pass == 1 | rush == 1, !is.na(epa), !is.na(posteam)) |> 
  mutate(rush_epa = case_when(rush == 1 ~ epa)) |> 
  mutate(pass_epa = case_when(pass == 1 ~ epa)) |> 
  group_by(season) |> 
  summarize(pass_epa = mean(pass_epa, na.rm = TRUE), 
            rush_epa = mean(rush_epa, na.rm = TRUE), 
            epa = mean(epa)
  ) |> 
  arrange(-epa) |> 
  mutate(avg_rank = as.character(row_number())) |> 
  arrange(-rush_epa) |> 
  mutate(rush_rank = as.character(row_number())) |> 
  arrange(-pass_epa) |> 
  mutate(pass_rank = as.character(row_number())) |> 
  arrange(season)

split_epa <- split_table(nfl_epa) |> 
  mutate_at(c("pass_rank_2", "rush_rank_2", "avg_rank_2"), ~replace_na(.,""))

(
tab <- 
  gt(split_epa) |>  
  gt_theme_538() |> 
  split_cols_label() |> 
  # cols_label(drv = "drive-train", 
  #            trans = "transmission"
  # ) |> 
    # Sub out NA's
  sub_missing(columns = everything(), 
              rows = everything(), 
              missing_text = "") |> 
# Header text and format
  tab_header(title = "Offense is Down Bad in 2023", 
             subtitle = "EPA/play  |  Subscript is season rank") |> 
  tab_style(
    style = list(
      cell_text(weight = "bold", 
                align = "center")
    ),
    locations = cells_title(groups = c("title", "subtitle"))
  ) |> 
  gt_merge_stack(col1 = pass_epa_1, col2 = pass_rank_1, 
                 palette = c("black", "gray20")) |> 
  gt_merge_stack(col1 = pass_epa_2, col2 = pass_rank_2, 
                 palette = c("black", "gray20")) |> 
  gt_merge_stack(col1 = rush_epa_1, col2 = rush_rank_1, 
                 palette = c("black", "gray20")) |> 
  gt_merge_stack(col1 = rush_epa_2, col2 = rush_rank_2, 
                 palette = c("black", "gray20")) |> 
  gt_merge_stack(col1 = epa_1, col2 = avg_rank_1, 
                 palette = c("black", "gray20")) |> 
  gt_merge_stack(col1 = epa_2, col2 = avg_rank_2, 
                 palette = c("black", "gray20")) |> 
# Add border lines around column
  tab_style(
    style = list(
      cell_borders(
        side = c("left", "right"), 
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c(epa_1, epa_2)
    )
  ) |> 
  fmt_number(contains("epa_"), decimals = 3) |> 
  gt_hulk_col_numeric(columns = contains("epa_"), 
                      trim = TRUE, 
                      na.color = "white") |> 
  tab_source_note(
  source_note = md(
    "<div style=\"width: 100%; display: table;\">
          <div style=\"display: table-row\">
              <div style=\"width: 40%; display: table-cell;\">
                <img src=\"https://github.com/Josephhero/Jefe-Logo/raw/main/Jefe%20Logo%20Skyline.png\" style=\"height:35px;\">
              </div>
              <div style=\"display: table-cell;vertical-align: middle;text-align: right\">Data: nflverse.com</div>
          </div>
      </div>"
    )
  )
)

gtsave(tab, 
       path = "./images", 
       filename = "Offensive EPA Since 1999.png", 
       expand = 10)
       



