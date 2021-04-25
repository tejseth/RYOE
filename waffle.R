library(tidyverse)
library(waffle)
library(na.tools)
library(ggimage)
library(nflfastR)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)

pbp_2020 <- pbp_2020 %>%
  mutate(Label = case_when(
    ryoe < 0 ~ "1: Less than Zero RYOE",
    ryoe >= 0 & ryoe < 5 ~ "2: 0 to 5 RYOE",
    ryoe >= 5 & ryoe < 15 ~ "3: 5 to 15 RYOE",
    ryoe >= 15 ~ "4: 15+ RYOE"
  ))

top_rushers_grouped <- pbp_2020 %>%
  filter(rusher_player_name %in% c("D.Harris", "C.Newton", "S.Michel", "R.Burkhead")) %>%
  group_by(rusher_player_name, Label) %>%
  summarize(count = n())

ggplot(top_rushers_grouped, aes(fill = Label, values = count)) +
  geom_waffle(color = "white", size = 0.40, n_rows = 10, flip = TRUE) +
  facet_wrap(~rusher_player_name, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "The RYOE for Each Patriots Rusher in 2020",
    subtitle = "Based off of a Rushing Yards over Expected (RYOE) model trained on @nflfastR data",
    x = "Rusher",
    y = "Count",
    caption = "By Tej Seth | @mfbanaytics"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave("ryoe-waffle-4.png", height = 8, width = 13, dpi = "retina")

  