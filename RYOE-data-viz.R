rushers_2020 <- rushers_2020 %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) 

scatter_plot <- rushers_2020 %>% 
  ggplot() +
  geom_smooth(aes(x = mean_epa, y = avg_ryoe), method = "lm", color = "grey") +
  ggrepel::geom_text_repel(
    aes(x = mean_epa, y = avg_ryoe, label = rusher_player_name),
    box.padding = 0.3, size = 5
  ) + 
  geom_point(
    aes(x = mean_epa, y = avg_ryoe, size = rushes, fill = team_color, color = team_color2), 
    shape = 21
  ) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  scale_size(name = "Designed Rushes") +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "EPA Per Rush",
       y = "Rushing Yards Over Expected Per Rush",
       title = "RYOE and EPA Are Correlated",
       subtitle = "RYOE is a xgboost model, min. of 105 designed rushes",
       caption = "By Tej Seth | @mfbanalytics | @deceptivespeed_") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) 
scatter_plot
ggsave(
  "ryoe-1.png", scatter_plot, 
  height = 10, width = 16, dpi = "retina"
)


rusher_faces<- pbp_2020 %>%
  filter(!is.na(rusher_player_name)) %>%
  group_by(rusher_player_name, posteam) %>%
  summarize(rushes = n(),
            sum_ryoe = sum(ryoe, na.rm = T),
            avg_ryoe = mean(ryoe, na.rm =T),
            mean_epa = mean(epa, na.rm = T)) %>%
  filter(rushes > 75) %>%
  arrange(desc(avg_ryoe))

#Download the csv and manually put in the links for the faces 
#I provided the faces in github on a CSV falled "rusher_faces.csv"
write.csv(rusher_faces, "rusher_faces.csv")
rusher_faces <- read.csv("~/RYOE/rusher_faces.csv")

tab_data <- rusher_faces %>% 
  mutate(RK = as.integer(rank)) %>% 
  select(RK, rusher, headshot, mean_epa, avg_ryoe) %>%
  arrange(RK)

tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot)),
      fn = function(x){
        web_image(
          url = x,
          height = px(25)
        )
      }
    ) %>% 
    cols_label(
      RK = "Rank",
      rusher = "Rusher",
      headshot = "",
      mean_epa = "EPA/Rush",
      avg_ryoe = "RYOE") %>%
    data_color(
      columns = vars(avg_ryoe),
      colors = scales::col_numeric(
        palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"),
        domain = c(-3, 2)
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(RK, rusher)
      )
    ) %>% 
    tab_header(
      title = "Top 30 RYOE Rushers",
      subtitle = "RYOE = Rushing Yards Over Expected"
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
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
      heading.align = "middle",
      ...
    ) %>%
    opt_table_font(
      font = list(
        default_fonts()
      )
    ) 
}

gt_tab1 <- tab_data %>%
  filter(RK < 31) %>%
  tab_function()
gt_tab1
gtsave(gt_tab1, "gt-tab1.png")

gt_tab2 <-  tab_data %>% 
  filter(RK >= 31) %>% 
  tab_function() %>% 
  tab_header(
    title = "Rushers 31-60",
    subtitle = "RYOE is a xgboost model made by Tej Seth (@mfbanalytics)"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "black",
      weight = px(3)
    ),
    locations = 
      list(
        cells_body(
          columns = 1
        ),
        cells_column_labels(1)
      )
  )
gt_tab2
gtsave(gt_tab2, "gt-tab2.png")

img1 <- magick::image_read("gt-tab1.png")
img2 <- magick::image_read("gt-tab2.png")

img3 <- magick::image_append(c(img1, img2))
img3

ggsave(img3, "ryoe-2.png")

rusher_faces2 <- read.csv("~/RYOE/rusher_faces.csv")
rusher_faces2 <- rusher_faces2 %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) 
rusher_faces2 <- rusher_faces2 %>%
  arrange(rank)

teams_xp <- pbp_2020 %>%
  group_by(posteam) %>%
  summarize(team_avg = mean(x_rush_yards, na.rm = T))

rusher_faces2 <- rusher_faces2 %>%
  left_join(teams_xp)

rusher_faces2 <- rusher_faces2 %>%
  mutate(yards_gained = avg_ryoe + team_avg,
         new_rank = row_number()) 

rusher_faces2 %>%
  ggplot() +
  geom_link(
    mapping = aes(x = team_avg, y = new_rank, xend = yards_gained, yend = new_rank, size = 2, color = team_color)
  ) +
  theme_bw() +
  scale_colour_identity() +
  geom_image(aes(x = team_avg, y = new_rank, image = team_logo_espn), size = 0.04, asp = 16/9) +
  geom_image(aes(x = yards_gained, y = new_rank, image = headshot), size = 0.04, asp = 16/9) +
  labs(
    x = "Rushing Yards Average",
    y = "",
    title = "Each Team's Most Used Rusher's RYOE",
    subtitle = "RYOE = Rushing Yards Over Expected, if a player's face is to the right of their logo they have a positive RYOE",
    caption = "By Tej Seth | @mfbanalytics | @deceptivespeed_"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10))

ggsave('ryoe-3.png', dpi=300, height=9*.8, width=16*.8)

top_rushers <- pbp_2020 %>% 
  filter(!is.na(rusher_player_name)) %>%
  group_by(rusher_player_name) %>%
  summarize(rushes = n(),
            sum_ryoe = sum(ryoe, na.rm = T)) %>%
  filter(rushes > 107) %>%
  arrange(desc(sum_ryoe)) %>%
  filter(sum_ryoe > 0)

top_rushers <- top_rushers %>%
  left_join(rusher_faces)

top_rushers <- top_rushers %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  filter(!is.na(rank)) %>%
  mutate(rank = row_number())

link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

bar_plot <- top_rushers %>% 
  mutate(label = link_to_img(headshot),
         rank = as.integer(rank)) %>% 
  ggplot() +
  geom_col(
    aes(
      x = rank, y = sum_ryoe,
      fill = team_color, color = team_color2
    ),
    width = 0.4
  ) + 
  geom_image(aes(x = rank, y = sum_ryoe + 5  , image = headshot), asp = 16/9, size = 0.06) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = NULL,
       y = "Total RYOE\n",
       title = "The Running Backs 10 in Rushing Yards Over Expected",
       caption = "By Tej Seth | @mfbanalytics") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )
bar_plot

ggsave(
  "ryoe-4.png", bar_plot, 
  height = 10, width = 16, dpi = "retina"
)
