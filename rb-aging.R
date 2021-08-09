rosters <- read.csv("~/Downloads/rosters.csv")
library(lme4)
library(merTools)
library(ggthemes)

nfl_rbs <- unique(ryoe_projs$player_id)

ncaa_carries <- ncaa_ryoe_projs %>%
  filter(player_id %in% nfl_rbs) %>%
  dplyr::select(player, player_id, season, ryoe, league)

nfl_carries <- ryoe_projs %>%
  filter(season >= 2014) %>%
  dplyr::select(player, player_id, season, ryoe, league)

nfl_full_carries <- ryoe_projs %>%
  dplyr::select(player, player_id, season, offense, exp_yards, box_players, ryoe, league) %>%
  janitor::clean_names()

carries <- rbind(ncaa_carries, nfl_carries)

carries$rush_num <- ave(carries$ryoe, carries$player_id, FUN = seq_along)

carries$csum <- ave(carries$ryoe, carries$player_id, FUN=cumsum)

group <- carries %>%
  group_by(rush_num) %>%
  summarize(count = n(),
            avg_ryoe = mean(ryoe),
            avg_csum = mean(csum)) %>%
  filter(count >= 10)

group %>%
  ggplot(aes(x = rush_num, y = avg_ryoe)) +
  geom_point(aes(size = count, fill = -rush_num), color = "black", shape = 21, alpha = 0.2) +
  scale_fill_viridis_c() +
  geom_smooth(size = 3, color = "black") +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Career Rush Number",
       y = "Average Rushing Yards Over Expected",
       title = "Running Backs Don't Have a Severe Drop-off Over 1,000 Carries",
       subtitle = "2014-2020, college carries included")

draft <- espnscrapeR::get_sharpe_data(dataset = "draft_picks")

draft_select <- draft %>%
  filter(position == "RB") %>%
  dplyr::select(pfr_name, season, round) %>%
  rename(draft_season = season)

n_occur <- data.frame(table(draft_select$pfr_name))
n_occur[n_occur$Freq > 1,]

draft_select %<>%
  mutate(remove = case_when(
    pfr_name == "Adrian Peterson" & round == 6 ~ TRUE,
    pfr_name == "Ben Tate" & round == 11 ~ TRUE,
    pfr_name == "Ricky Williams" & round == 9 ~ TRUE,
    pfr_name == "Reggie Brown" & round == 4 ~ TRUE,
    pfr_name == "Mike Jones" & round == 12 ~ TRUE,
    pfr_name == "Larry Jones" & round == 10 ~ TRUE,
    pfr_name == "Keith Jones" & round == 6 ~ TRUE,
    pfr_name == "James Stewart" & round == 5 ~ TRUE,
    pfr_name == "James Jones" & round == 3 ~ TRUE,
    TRUE ~ FALSE
   )) %>%
  filter(remove != TRUE)

nfl_full_carries <- nfl_full_carries %>%
  left_join(draft_select, by = c("player" = "pfr_name"))

nfl_full_carries %<>%
  mutate(years_in_nfl = season - draft_season + 1)

nfl_full_carries %<>%
  filter(years_in_nfl > 0 & years_in_nfl < 15)

rushing_lmer <- lmer(ryoe ~ as.factor(years_in_nfl) + (1|player), data = nfl_full_carries)

summary(rushing_lmer)

VarCorr(rushing_lmer) %>% 
  as_tibble() %>% 
  mutate(icc = vcov / sum(vcov)) %>% 
  dplyr::select(grp, icc)

rushing_effects <- REsim(rushing_lmer)

rushing_effects %<>%
  dplyr::select(groupID, mean, median, sd) %>%
  rename(player = groupID) %>%
  mutate(mean = round(mean, 3),
         median = round(median, 3),
         sd = round(sd, 3),
         lci = mean - sd,
         uci = mean + sd) %>%
  arrange(-mean)

rb_colors <- nfl_full_carries %>%
  group_by(player, player_id, offense) %>%
  summarize(plays = n()) %>%
  arrange(-plays) %>%
  group_by(player, player_id) %>%
  top_n(1) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

rushing_effects %<>%
  left_join(rb_colors, by = c("player"))

rushing_effects %<>%
  filter(plays >= 800)

rushing_effects %>%
  mutate(team = fct_reorder(player, mean)) %>%
  ggplot(aes(x= team, y=mean)) + 
  geom_pointrange(aes(ymin=(lci),
                      ymax=(uci), color = team_color), size = 1.2, fatten = 3)+
  scale_color_identity() +
  coord_flip()+
  theme_reach() +
  geom_image(aes(x = team, y = -0.41, image = team_logo_espn), asp = 16/9, size = 0.02) +
  geom_hline(yintercept = mean(rushing_effects$mean), linetype = "dashed") +
  labs(y = "Age-Adjusted RYOE",
       x = "",
       title = "Rushing Yards Over Expected After Adjusting for Age",
       subtitle = "Minimum of 800 rushes to qualify") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(panel.grid.major.y = element_line(size = 0.1))
ggsave('1-dropoff.png', width = 15, height = 10, dpi = 'retina')

years <- nfl_full_carries %>%
  filter(!is.na(years_in_nfl)) %>%
  group_by(player, years_in_nfl, season) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe)) %>%
  filter(rushes >= 100)

years <- years %>%
  mutate(`Season Type` = case_when(
    avg_ryoe < -0.75 ~ "Bad Season",
    avg_ryoe >= -0.75 & avg_ryoe < 0 ~ "Below-Average Season",
    avg_ryoe >= 0 & avg_ryoe < 0.75 ~ "Above-Average Season",
    avg_ryoe >= 0.75 ~ "Elite Season"
  ))

years$`Season Type` <- factor(years$`Season Type`, 
                              levels = c("Bad Season", "Below-Average Season", "Above-Average Season", "Elite Season"))

years %>%
  ggplot(aes(x = as.factor(years_in_nfl), y = avg_ryoe, fill = `Season Type`)) + 
  geom_quasirandom(pch = 21, size = 4.5) + 
  scale_fill_viridis_d() +
  theme_reach() +
  geom_hline(yintercept = 0, color = "black", alpha=1.0) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggrepel::geom_text_repel(aes(label = player), max.overlaps = 4, size = 4, box.padding = 0.3) +
  labs(
    x = "Years in NFL",
    y = "Average RYOE",
    title = "It Takes Until Year 7 in the NFL for Elite Running Backs to Become Rare",
    subtitle = "RYOE = Rushing Yards Over Expected, minimum of 100 rushes to qualify") +
  theme(panel.grid.major.x = element_line(size = 0.2),
        legend.position = "bottom",
        legend.text = element_text(size = 18))
ggsave('2-dropoff.png', width = 15, height = 10, dpi = "retina")

years_no_players <- nfl_full_carries %>%
  filter(!is.na(years_in_nfl)) %>%
  group_by(years_in_nfl) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe)) %>%
  mutate(nfl_logo = "https://www.freepnglogos.com/uploads/nfl-logo-png/logo-nfl-6.png",
         adj_count = prettyNum(rushes,big.mark=",",scientific=FALSE)) %>%
  filter(rushes >= 1000)

years_no_players %>%
  ggplot(aes(x = as.factor(years_in_nfl), y = avg_ryoe)) +
  geom_bar(aes(fill = avg_ryoe), stat = "identity") +
  geom_text(aes(y = ifelse(avg_ryoe > 0, avg_ryoe + 0.01, avg_ryoe - 0.01), label = adj_count), size = 4) +
  theme_reach() +
  scale_fill_viridis_c() +
  theme(panel.grid.major.x = element_line(size = 0.2)) +
  labs(x = "Years in NFL",
       y = "Average RYOE",
       title = "Running Backs Tend to Have Negative RYOE After 5-7 Years in the NFL",
       subtitle = "RYOE = Rushing Yards Over Expected, text is amount of rushes") 
ggsave('3-dropoff.png', width= 15, height = 10, dpi = "retina")

nfl_full_carries %<>%
  mutate(`Draft Day` = case_when(
    round == 1 ~ "Day 1",
    round == 2 ~ "Day 2",
    round == 3 ~ "Day 2",
    round > 3 ~ "Day 3"
  ))

round_year <- nfl_full_carries %>%
  filter(years_in_nfl <= 12) %>%
  filter(!is.na(years_in_nfl)) %>%
  group_by(`Draft Day`, years_in_nfl) %>%
  summarize(rushes = n(),
            exp_yards = mean(exp_yards),
            avg_box = mean(box_players, na.rm = T),
            avg_ryoe = mean(ryoe))

round_year %>%
  ggplot(aes(x = years_in_nfl, y = avg_ryoe, color = `Draft Day`)) +
  geom_point(aes(size = rushes), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  scale_color_viridis_d() +
  theme_reach() +
  geom_hline(yintercept = 0) +
  labs(x = "Years in NFL",
       y = "Average RYOE",
       title = "How Draft Position Factors Into Running Back Aging",
       subtitle = "2006-2020, bubble size is amount of rushes") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  guides(size = FALSE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  annotate("text", x = 11, y = 0.13, label = "Survivorship \n Bias", size = 5) +
  annotate("curve", x = 11, y = 0.10, xend = 11.8, yend =-0.02, angle = 50,  curvature = 0.3, arrow = arrow(length = unit(2, "mm")))
ggsave('9-dropoff.png', width= 15, height = 10, dpi = "retina")

round_year %>%
  ggplot(aes(x = years_in_nfl, y = avg_box, color = `Draft Day`)) +
  geom_point(aes(size = rushes), alpha = 0.6) +
  geom_smooth(se = FALSE, size = 2) +
  scale_color_viridis_d() +
  theme_reach() +
  labs(x = "Years in NFL",
       y = "Average Box Defenders Faced",
       title = "How Draft Position Factors Into Average Box Defenders Faced",
       subtitle = "2006-2020, bubble size is amount of rushes") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18)) +
  guides(size = FALSE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) 
ggsave('10-dropoff.png', width= 15, height = 10, dpi = "retina")

nfl_draft_06 <- nfl_full_carries %>%
  filter(draft_season >= 2006)

nfl_draft_06$rush_num <- ave(nfl_draft_06$ryoe, nfl_draft_06$player_id, FUN = seq_along)

nfl_draft_06$csum <- ave(nfl_draft_06$ryoe, nfl_draft_06$player_id, FUN=cumsum)

nfl_draft_06 %<>%
  group_by(player_id) %>%
  mutate(total_rushes = n()) %>%
  ungroup()

peterson <- nfl_draft_06 %>% filter(player_id == "3623")
charles <- nfl_draft_06 %>% filter(player_id == "4387")
richardson <- nfl_draft_06 %>% filter(player_id == "7009")
gore <- nfl_draft_06 %>% filter(player_id == "7009")
lynch <- nfl_draft_06 %>% filter(player_id == "3628")
mccoy <- nfl_draft_06 %>% filter(player_id == "4976")

nfl_draft_06 %>%
  filter(!player_id %in% c("3623", "4387", "7009", "3628", "4976")) %>%
  filter(total_rushes >= 400) %>%
  ggplot(aes(x = rush_num, y = csum, group = player_id)) +
  geom_smooth(color = "gray", se = FALSE, size = 1.3) +
  geom_smooth(data = peterson, color = "#4F2683", size = 2.4) +
  geom_smooth(data = charles, color = "#E31837", size = 2.4) +
  geom_smooth(data = richardson, color = "#311D00", size = 2.4) +
  geom_smooth(data = lynch, color = "#69BE28", size = 2.4) +
  geom_smooth(data = mccoy, color = "#00338D", size = 2.4) +
  scale_color_identity() +
  geom_hline(yintercept = 0) +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  labs(x = "Rush Number",
       y = "Cumulative RYOE",
       title = "The Heavy Workload Running Backs Plateau Around 2,000 Carries",
       subtitle = 'RYOE = Rushing Yards Over Expected, all running backs drafted since 2006 included') +
  annotate("text", x = 1550, y = 1420, label = "Jamaal \n Charles", size = 5) +
  annotate("text", x = 3500, y = 1300, label = "Adrian \n Peterson", size = 5) +
  annotate("text", x = 2400, y = 750, label = "LeSean \n McCoy", size = 5) +
  annotate("text", x = 2400, y = 350, label = "Marshawn \n Lynch", size = 5) +
  annotate("text", x = 800, y = -550, label = "Trent \n Richardson", size = 5)
ggsave('4-dropoff.png', width = 15, height = 10, dpi = "retina")

draft_06_stats <- nfl_draft_06 %>%
  group_by(rush_num) %>%
  summarize(players = n(),
            avg_ryoe = mean(ryoe),
            avg_csum = mean(csum)) %>%
  filter(players >= 5)

ryoe_stats <- nfl_full_carries %>%
  group_by(player, player_id) %>%
  mutate(career_ryoe = mean(ryoe)) %>%
  group_by(player, player_id, offense, season, career_ryoe) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe),
            total_ryoe = sum(ryoe)) %>%
  arrange(season) %>%
  group_by(player, player_id) %>%
  mutate(next_rushes = lead(rushes),
         next_avg_ryoe = lead(avg_ryoe),
         next_total_ryoe = lead(total_ryoe),
         change_from_career = avg_ryoe - career_ryoe) %>%
  filter(rushes >= 100)

ryoe_stats %>%
  ggplot(aes(x = rushes, y = change_from_career)) +
  geom_point(aes(color = change_from_career, size = next_rushes), alpha = 0.6) +
  scale_color_viridis_c() +
  geom_smooth(se = FALSE, color = "black", size = 2) +
  theme_reach() +
  labs(x = "Rushes in Year N",
       y = "Difference in RYOE of N + 1 from Career",
       title = "Running Backs Don't Do Worse After a Heavy Workload the Year Before",
       subtitle = "2006-2020, bubble size is amount of rushes in year N + 1") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
ggsave('5-dropoff.png', width = 15, height = 10, dpi = "retina")

ryoe_stats_06 <- nfl_draft_06 %>%
  group_by(player, player_id) %>%
  mutate(career_ryoe = mean(ryoe)) %>%
  group_by(player, player_id, season, career_ryoe) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe),
            total_ryoe = sum(ryoe)) %>%
  arrange(season) %>%
  group_by(player, player_id) %>%
  mutate(next_rushes = lead(rushes),
         next_avg_ryoe = lead(avg_ryoe),
         next_total_ryoe = lead(total_ryoe),
         change_from_career = avg_ryoe - career_ryoe) %>%
  filter(rushes >= 100)

carries_into_season <- ryoe_stats_06 %>%
  group_by(player, player_id) %>%
  do({rowwise(.) %>% 
      mutate(carries_into_season = sum(.$rushes[.$season < season]))})

carries_into_season %>%
  ggplot(aes(x = carries_into_season, y = avg_ryoe)) +
  geom_point(aes(color = avg_ryoe, size = rushes), alpha = 0.6) +
  scale_color_viridis_c() +
  geom_smooth(se = FALSE, color = "black", size = 2, span = 0.6) +
  theme_reach() +
  labs(x = "Carries Into Season",
       y = "RYOE in Season",
       title = "Running Backs Steadily Decline After About 1,500 Rushes",
       subtitle = "2006-2020, bubble size is amount of rushes") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
ggsave('6-dropoff.png', width = 15, height = 10, dpi = "retina")

college_ryoe_stats <- ncaa_ryoe_projs %>%
  group_by(player, player_id) %>%
  summarize(college_rushes = n(),
            college_ryoe = mean(ryoe))

nfl_ryoe_stats <- ryoe_projs %>%
  group_by(player, player_id) %>%
  summarize(nfl_rushes = n(),
            nfl_total_ryoe = sum(ryoe),
            nfl_avg_ryoe = mean(ryoe)) %>%
  filter(nfl_rushes >= 150) %>%
  left_join(college_ryoe_stats, by = c("player", "player_id")) %>%
  filter(!is.na(college_rushes))

nfl_ryoe_stats %>%
  filter(!player %in% c("Lamar Jackson", "Kyler Murray", "Josh Allen")) %>%
  ggplot(aes(x = college_rushes, y = nfl_avg_ryoe)) +
  geom_point(aes(size = nfl_rushes, color = college_rushes)) +
  geom_smooth(se = FALSE, color = "black", size = 2, method = "lm") +
  theme_reach() +
  ggrepel::geom_text_repel(aes(label = player), box.padding = 0.35, size = 4.5, max.overlaps = 3) +
  scale_color_viridis_c() +
  labs(x = "Rushes in College",
       y = "NFL RYOE",
       title = "Having a Large Workload in College Doesn't Make a Running Back Worse in the NFL",
       subtitle = "2014-2020, bubble size is amount of rushes in the NFL") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
ggsave('7-dropoff.png', width = 15, height = 10, dpi = "retina")

nfl_full_carries %<>%
  mutate(rookie_contract = ifelse(years_in_nfl <= 4, "Rookie Contract", "Second Contract"))

rookie_stats <- nfl_full_carries %>%
  filter(rookie_contract == "Rookie Contract") %>%
  group_by(player, player_id) %>%
  summarize(rookie_rushes = n(),
            rookie_ryoe = mean(ryoe))

second_stats <- nfl_full_carries %>%
  filter(rookie_contract != "Rookie Contract") %>%
  group_by(player, player_id) %>%
  summarize(second_rushes = n(),
            second_ryoe = mean(ryoe),
            second_total_ryoe = sum(ryoe))

all_stats <- rookie_stats %>%
  filter(rookie_rushes >= 100) %>%
  left_join(second_stats, by = c("player", "player_id")) %>%
  filter(!is.na(second_rushes)) %>%
  left_join(rb_colors, by = c("player", "player_id"))

all_stats %>%
  ggplot(aes(x = rookie_rushes, y = second_total_ryoe)) +
  geom_point(aes(size = second_rushes, fill = team_color, color = team_color2), shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_smooth(size = 2, se = FALSE, color = "black") +
  ggrepel::geom_text_repel(aes(label = player), size = 5, box.padding = 0.35, max.overlaps = 3) +
  theme_reach() +
  labs(x = "Rushes on Rookie Contract",
       y = "RYOE for Rest of Career",
       title = "Running Backs Who Get Worked on Their Rookie Contract Don't Burn Out",
       subtitle = "RYOE = Rushing Yards Over Expected, 2006-2020") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
ggsave('8-dropoff.png', width = 15, height = 10, dpi = "retina")

breakdown_cans <- nfl_full_carries %>%
  group_by(player, player_id) %>%
  summarize(total_rushes = n(),
            total_years = max(years_in_nfl),
            last_season = max(season),
            avg_ryoe = round(mean(ryoe), 2)) %>%
  filter(total_rushes >= 200) %>%
  filter(last_season == 2020) %>%
  arrange(-total_years)

breakdown_rbs <- c("Le'Veon Bell", "Ezekiel Elliott", "Derrick Henry", "Latavius Murray", "Devonta Freeman", "Carlos Hyde")

breakdown_gt <- breakdown_cans %>%
  filter(player %in% breakdown_rbs)

write.csv(breakdown_gt, "breakdown_gt.csv")

breakdown_gt <- read.csv("~/RYOE/breakdown_gt.csv")

breakdown_gt %<>%
  arrange(-total_rushes)

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

dropoff_8 <- breakdown_gt %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(team_wordmark)),
    fn = function(x){
      web_image(
        url = x,
        height = px(40)
      )
    }
  ) %>% 
  cols_label(
    player = "Player",
    team_wordmark = "",
    total_years = "Total Years",
    total_rushes = "Total Rushes",
    avg_ryoe = "RYOE Per Rush") %>%
  data_color(
    columns = vars(total_rushes),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  tab_header(
    title = md("**Potential Running Back Regression Candidates, 2021**"),
    subtitle = md("Based on total carries and years spent in the NFL")
  ) %>%
  gt_theme_538(table.width = px(800))
gtsave(dropoff_8, "8-dropoff.png")
