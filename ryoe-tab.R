gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

tab_20 <- rushers_2020 %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  filter(rushes > 75)
  
tab_20 <- tab_20 %>%
  arrange(desc(avg_ryoe_20)) %>%
  ungroup() %>%
  mutate(rank = row_number()) %>%
  select(rank, rusher_player_name, team_logo_espn, rushes, sum_ryoe_20, mean_epa_20, avg_ryoe_20)

tab_20 <- tab_20 %>%
  mutate_if(is.numeric, ~round(., 2))

tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(team_logo_espn)),
      fn = function(x){
        web_image(
          url = x,
          height = px(25)
        )
      }
    ) %>% 
    cols_label(
      rank = "Rank",
      rusher_player_name = "Rusher",
      team_logo_espn = "",
      rushes = "Rushes",
      sum_ryoe_20 = "Total RYOE",
      mean_epa_20 = "EPA/Rush",
      avg_ryoe_20 = "RYOE/rush") %>%
    data_color(
      columns = vars(avg_ryoe_20),
      colors = scales::col_numeric(
        palette = c("white", "#3fc1c9"),
        domain = c(-1.6, 2.5)
      )
    ) %>% 
    tab_source_note(
      source_note = md("By Tej Seth | @mfbanalytics <br>Table: @thomas_mock")
    ) %>% 
    gt_theme_538(table.width = px(550))}
    
gt_tab1 <- tab_20 %>%
  filter(rank < 31) %>%
  tab_function()
gt_tab1
gtsave(gt_tab1, "gt-tab1.png")

gt_tab2 <-  tab_20 %>% 
  filter(rank >= 31) %>% 
  tab_function()
gt_tab2    
gtsave(gt_tab2, "gt-tab2.png")

img1 <- magick::image_read("gt-tab1.png")
img2 <- magick::image_read("gt-tab2.png")

img3 <- magick::image_append(c(img1, img2))
img3

ggsave(img3, "ryoe-538-tab.png")

    
    