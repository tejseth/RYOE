library(ggbeeswarm)

pbp_2020 <- pbp_2020 %>%
  mutate(Label = case_when(
    ryoe < 0 ~ "Less than Zero RYOE",
    ryoe >= 0 & ryoe < 5 ~ "0 to 5 RYOE",
    ryoe >= 5 & ryoe < 15 ~ "5 to 15 RYOE",
    ryoe >= 15 ~ "15+ RYOE"
  ))

pbp_2020 %>%
  filter(rusher_player_name %in% c("T.Gurley", "P.Lindsay", "A.Peterson", "J.Kelley", "J.Conner")) %>%
  filter(ryoe <= 25) %>%
  filter(ryoe > -7.5) %>%
  filter(week <= 17) %>%
  ggplot(aes(x = rusher_player_name, y = ryoe, fill = Label)) + 
  geom_quasirandom(pch = 21, size = 3) + 
  scale_fill_brewer(palette = "Spectral") +
  theme_classic(15) +  
  geom_hline(yintercept = 0, color = "black", alpha=1.0) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Rusher",
    y = "RYOE",
    title = "The NFL's Worst Running Backs in 2020",
    subtitle = "Based off of a Rushing Yards over Expected (RYOE) model trained on @nflfastR data",
    caption = "By Tej Seth | @mfbanalytics"
  ) +
  theme(
    legend.position = "None",
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) 
ggsave("ryoe-bee-5.png", height = 10, width = 13, dpi = "retina")

