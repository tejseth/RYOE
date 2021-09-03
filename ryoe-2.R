library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)
library(bayesboot)
library(rvest)
library(tidymodels)
library(vip)   
library(ggthemes)
library(caret)
library(viridis)
library(tidymodels)
library(SHAPforxgboost)
library(DiagrammeR)
library(caret)
library(dummies)
library(gganimate)
library(gifski)
library(ggcorrplot)
library(zoo)
library(ggbeeswarm)
options(scipen = 9999)
rushing_data <- read.csv("~/Downloads/rushing_data.csv")
epa_data <- pull_s3("epa_rd/nfl_ep_epa.csv", bucket = "ml")
run_blocking <- pull_s3(paste0("analytics/projections/by_facet/", 'nfl', "/%i/run_blocking.csv.gz"), season_start = 2006, season_end = 2020)

epa_select <- epa_data %>%
  dplyr::select(game_id, play_id, EPA)

rushing_data <- rushing_data %>%
  left_join(epa_select, by = c("game_id", "play_id"))

blocks_sums <- run_blocking %>%
  filter(!is.na(run_blocking_grade)) %>%
  group_by(game_id, play_id) %>%
  summarize(total_blocks = n(),
            pos_blocks = sum(run_blocking_grade > 0),
            neg_blocks = sum(run_blocking_grade < 0))

colSums(is.na(rushing_data))

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size=17),
      axis.title.y = element_text(size=17),
      axis.text = element_text(size = 15)
    )
}

rushing_data <- rushing_data %>%
  filter(!is.na(yards))

rushing_data <- rushing_data %>%
  mutate(score_diff = off_score_before - def_score_before)

rushing_data <- rushing_data %>%
  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>%
  ungroup()

rushing_data <- rushing_data %>%
  mutate(defense = case_when(
    defense == "SD" ~ "LAC",
    defense == "BLT" ~ "BAL",
    defense == "OAK" ~ "LV",
    defense == "HST" ~ "HOU",
    defense == "SL" ~ "LA",
    defense == "CLV" ~ "CLE", 
    defense == "ARZ" ~ "ARI",
    TRUE ~ defense
  )) %>%
  ungroup()

rushing_data <- rushing_data %>%
  filter(quarter < 5)

rushing_data <- rushing_data %>%
  filter(run_position != "QBK")

rushing_data <- rushing_data %>%
  mutate(seconds_left_in_half = case_when(
    quarter == 1 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 2 ~ as.integer(seconds_left_in_quarter),
    quarter == 3 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 4 ~ as.integer(seconds_left_in_quarter),
  ))

rushing_data <- rushing_data %>%
  mutate(half = case_when(
    quarter <= 2 ~ "First Half",
    quarter > 2 ~ "Second Half"
  ))

rushing_data %>% 
  filter(season > 2010) %>%
  filter(!is.na(box_players)) %>%
  filter(box_players < 9) %>%
  filter(box_players > 4) %>%
  filter(quarter < 5) %>%
  ggplot(aes(x = seconds_left_in_half, y = yards, group = as.factor(box_players), color = as.factor(box_players))) +
  geom_smooth(se = FALSE, size = 2) +
  theme_reach() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Seconds Left in Half",
       y = "Yards Gained",
       title = "How Rushing Yards Changes by Time and Defenders in the Box",
       subtitle = "Every designed rush from 2010 to 2020",
       color = "Box Players") +
  scale_x_reverse(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme(legend.position = "bottom") +
  facet_wrap(~half)

colSums(is.na(rushing_data))

def_ypc <- rushing_data %>%
  dplyr::group_by(defense, season) %>%
  dplyr::summarize(def_ypc = mean(yards))

rushing_data_join <- rushing_data %>%
  left_join(def_ypc, by = c("defense", "season")) %>%
  left_join(blocks_sums, by = c("game_id", "play_id"))

rushing_data_join <- rushing_data_join %>%
  dplyr::filter(!is.na(box_players)) %>%
  dplyr::select(offense, defense, player, player_id, season, game_id, play_id, week, concept_1, distance, down, 
         quarter, seconds_left_in_half, yards_to_go, yards, box_players, def_ypc, score_diff,
         total_blocks, pos_blocks, neg_blocks, EPA) %>%
  filter(!is.na(total_blocks))

colSums(is.na(rushing_data_join))

rushing_model_data <- rushing_data_join %>%
  select(distance, down, seconds_left_in_half, yards_to_go, 
         yards, box_players, def_ypc, score_diff, total_blocks, pos_blocks, neg_blocks) %>%
  dplyr::rename(label = yards) %>%
  dplyr::select(label, everything())

smp_size <- floor(0.50 * nrow(rushing_model_data))
set.seed(2011) #go lions
ind <- sample(seq_len(nrow(rushing_model_data)), size = smp_size)
train <- as.matrix(rushing_model_data[ind, ])
test <- as.matrix(rushing_model_data[-ind, ])

dim(train)
colnames(train)

ryoe_model <-
  xgboost(
    data = train[, 2:11],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )   

vip(ryoe_model)

xgb.plot.tree(model = ryoe_model, trees = 1)

pred_xgb <- predict(ryoe_model, test[, 2:11])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

hyper_grid <- expand.grid(max_depth = seq(3, 5, 1),
                          eta = seq(.2, .3, .01))
xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:11],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}

#ideal hyperparamters
hyper_grid[which.min(xgb_test_rmse), ]

rushing_model <-
  xgboost(
    data = train[, 2:11],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3, #ideal max depth
    eta = 0.22 #ideal eta
  )   

vip(rushing_model)

xgb.plot.tree(model = rushing_model, trees = 1)

pred_xgb <- predict(rushing_model, test[, 2:11])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

rushing_preds <- as.data.frame(
  matrix(predict(rushing_model, as.matrix(rushing_model_data %>% select(-label))))
) %>%
  dplyr::rename(exp_yards = V1)

ryoe_projs <- cbind(rushing_data_join, rushing_preds)

x = mean(ryoe_projs$yards) - mean(ryoe_projs$exp_yards)
x

ryoe_projs <- ryoe_projs %>%
  mutate(ryoe = yards - exp_yards + x)

ryoe_github <- ryoe_projs %>%
  dplyr::select(game_id, play_id, player, season, offense, week, exp_yards, yards, ryoe, EPA)

write.csv(ryoe_github, 'ryoe_github.csv')

rusher_seasons <- ryoe_projs %>%
  dplyr::group_by(player, player_id, season, offense) %>%
  dplyr::summarize(rushes = n(),
                   exp_yards = mean(exp_yards),
                   actual_yards = mean(yards),
                   avg_ryoe = mean(ryoe)) %>%
  dplyr::filter(rushes >= 100) %>%
  dplyr::arrange(season) %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(next_rushes = lead(rushes),
         next_avg_ryoe = lead(avg_ryoe)) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

summary(lm(next_avg_ryoe ~ avg_ryoe, data=rusher_seasons, weights = rushes))$r.squared #0.04

rushers_20 <- rusher_seasons %>%
  filter(season == 2020)

rushers_19 <- rusher_seasons %>%
  filter(season == 2019) 

rushers_all_time <- ryoe_projs %>%
  dplyr::group_by(player, player_id) %>%
  dplyr::summarize(rushes = n(),
            nfl_actual_ypc = mean(yards),
            nfl_ryoe = mean(ryoe),
            nfl_los_rate = sum(ryoe > 0) / rushes,
            nfl_explosive_rate = sum(ryoe > 10) / rushes) %>%
  dplyr::filter(rushes >= 300) %>%
  dplyr::arrange(-nfl_ryoe)

next_gen_ryoe <- read.csv("~/Downloads/next_gen_ryoe.csv")

rushers_20 <- rushers_20 %>%
  left_join(next_gen_ryoe, by = c("player" = "rusher")) %>%
  filter(!is.na(next_gen_ryoe))

rushers_20 %>%
  filter(player != "Lamar Jackson") %>%
  ggplot(aes(x = exp_yards, y = actual_yards)) +
  geom_point(aes(fill = team_color, color = team_color2, size = rushes), shape = 21, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = player)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  scale_x_continuous(breaks = pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  labs(x = "Expected Rushing Yards",
       y = "Actual Rushing Yards",
       title = "Actual and Expected Rushing Yards, 2020",
       subtitle = "Minimum of 100 designed rushes, bubble size is amount of rushes")
ggsave('dobbins.png', width = 15, height = 10, dpi = "retina")

rushers_19 %>%
  ggplot(aes(x = avg_ryoe, y = next_avg_ryoe)) +
  geom_point(aes(fill = team_color, color = team_color2), shape = 21, size = 4) +
  ggrepel::geom_text_repel(aes(label = player)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  geom_hline(yintercept = mean(rushers_19$next_avg_ryoe, na.rm = T), linetype = "dashed") +
  geom_vline(xintercept = mean(rushers_19$avg_ryoe), linetype = "dashed") +
  scale_x_continuous(limits = c(-1, 1.7), breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  labs(x = "<span style= 'color:purple'>2019</span> RYOE/Attempt",
       y = "<span style= 'color:blue'>2020</span> RYOE/Attempt",
       title = "How Each Rusher's RYOE/Attempt Compared in <span style= 'color:purple'>2019</span> vs. <span style= 'color:blue'>2020</span>",
       subtitle = "Minimum of 80 designed rushes in both <span style= 'color:purple'>2019</span> and <span style= 'color:blue'>2020</span>",
       caption = "By Tej Seth | @mfbanalytics | @PFF") +
  annotate("text", x = -0.8, y = 1.3, label = "Was better \n in 2020", color = "blue", size = 5) +
  annotate("text", x = 1.3, y = -0.7, label = "Was better \n in 2019", color = "purple", size = 5) +
  theme(plot.title = element_markdown(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5),
        axis.title.x = element_markdown(size=14),
        axis.title.y = element_markdown(size=14))
ggsave('rusher-19-20.png', width = 14, height = 10, dpi = "retina")

rates <- ryoe_projs %>%
  dplyr::filter(season == 2020) %>%
  dplyr::group_by(player, offense) %>%
  dplyr::summarize(los_rate = sum(ryoe < 0) / n(),
            explosive_rate = sum(ryoe > 10) / n(),
            rushes = n()) %>% 
  dplyr::filter(rushes >= 100) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

rates %>%
  ggplot(aes(x = los_rate, y = explosive_rate)) +
  geom_point(aes(fill = team_color, color = team_color2, size = rushes), shape = 21, alpha = 0.85) +
  geom_hline(yintercept = mean(rates$explosive_rate), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(rates$los_rate), linetype = "dashed", alpha = 0.5) +
  scale_size(name = "Designed Rushes") +
  ggrepel::geom_text_repel(aes(label = player), box.padding = 0.25) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  scale_x_reverse(breaks = pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = pretty_breaks(n = 7)) +
  labs(x = "Bad Run Rate (RYOE < 0)",
       y = "Explosive Run Rate (RYOE > 10)",
       title = "Rushers That are Explosive and Best at Avoiding Bad Runs, 2020",
       subtitle = "RYOE = Rushing Yards Over Expected, minimum of 100 rushes",
       caption = "By Tej Seth | @mfbanalytics | @PFF")
ggsave('kamara.png', width = 14, height = 10, dpi = "retina")

ryoe_projs <- ryoe_projs %>%
  dplyr::mutate(half = case_when(
    quarter <= 2 ~ "First Half",
    quarter > 2 ~ "Second Half"
  ))
  
ryoe_projs %>% 
  filter(quarter < 5) %>%
  filter(!is.na(down)) %>%
  filter(down > 0) %>%
  ggplot(aes(x = seconds_left_in_half, y = exp_yards, group = as.factor(box_players), color = as.factor(box_players))) +
  geom_smooth(se = FALSE, size = 2) +
  theme_reach() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Seconds Left in Half",
       y = "Expected Rushing yards",
       title = "How NFL Expected Rushing Yards Changes by Time and Down",
       subtitle = "Using PFF data from 2006-2020",
       color = "Down") +
  scale_x_reverse(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme(legend.position = "bottom") +
  facet_wrap(~half)

run_concepts <- c("Inside Zone", "Outside Zone", "Power", "Man", "Pull Lead", "Draw", "Counter", "Trap")

nfl_concept_stats <- ryoe_projs %>%
  filter(ryoe < 30) %>%
  filter(concept_1 %in% run_concepts) %>%
  filter(season > 2011) %>%
  group_by(concept_1) %>%
  summarize(nfl_count = n(),
            mean_ryoe = mean(ryoe),
            median_ryoe = median(ryoe)) %>%
  mutate(nfl_logo = "https://brandslogos.com/wp-content/uploads/images/large/nfl-logo.png")

ryoe_projs %>% 
  filter(season > 2011) %>%
  filter(ryoe < 15) %>%
  filter(ryoe > -5) %>%
  filter(concept_1 %in% run_concepts) %>%
  ggplot(aes(x = concept_1, y = ryoe)) +
  geom_jitter(alpha = 0.02, width = 0.2) +
  geom_boxplot(aes(fill = concept_1)) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme_reach() +
  labs(x = "Run Type",
       y = "RYOE",
       title = "How Run Type Affects Rushing Yards Over Expected",
       subtitle = "2012-2020, eight most common run types used")

ncaa_concept_stats <- ncaa_ryoe_projs %>%
  filter(ryoe < 30) %>%
  filter(concept_1 %in% run_concepts) %>%
  filter(season > 2011) %>%
  group_by(concept_1) %>%
  summarize(ncaa_count = n(),
            ncaa_mean_ryoe = mean(ryoe) + 0.31,
            ncaa_median_ryoe = median(ryoe)) %>%
  mutate(ncaa_logo = "https://cdn.freebiesupply.com/logos/large/2x/ncaa-4-logo-png-transparent.png")

concept_stats <- nfl_concept_stats %>%
  left_join(ncaa_concept_stats, by = c("concept_1")) %>%
  arrange(desc(nfl_count)) %>%
  mutate(rank = row_number())

concept_stats %>%
  mutate(concept_1 = fct_reorder(concept_1, nfl_count)) %>%
  ggplot() +
  geom_link(
    mapping = aes(x = mean_ryoe, y = concept_1, xend = ncaa_mean_ryoe, yend = concept_1, size = 2, color = "lightblue")
  ) +
  theme_reach() +
  scale_colour_identity() +
  geom_image(aes(x = mean_ryoe, y = concept_1, image = "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1200px-National_Football_League_logo.svg.png"), size = 0.05, asp = 16/9) +
  geom_image(aes(x = ncaa_mean_ryoe, y = concept_1, image = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/NCAA_logo.svg/1200px-NCAA_logo.svg.png"), size = 0.05, asp = 16/9) +
  labs(
    x = "Average RYOE",
    y = "Run Concept",
    title = "The Average Rushing Yards Over Expected of Each Run Concept, NCAA vs. NFL",
    subtitle = "NFL logo for RYOE/attempt in NFL and NCAA logo for RYOE/attempt in college, ordered in terms of usage"
  ) +
  theme(
    axis.ticks.y=element_blank(),
    axis.text.y = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('rushing-2.png', width = 14, height = 10, dpi = "retina")

ryoe_projs <- ryoe_projs %>%
  mutate(league = "NFL")

ncaa_ryoe_projs <- ncaa_ryoe_projs %>%
  mutate(league = "NCAA")

p1 <- ryoe_projs %>%
  select(ryoe, league)

p2 <- ncaa_ryoe_projs %>%
  select(ryoe, league)

p3 <- rbind(p1, p2)

p3 %>%
  filter(ryoe < 30) %>%
  filter(ryoe > -10) %>%
  ggplot() +
  geom_violin(aes(x = league, y = ryoe, fill = league)) +
  theme_reach()

ryoe_projs$rush_num <- ave(ryoe_projs$ryoe, ryoe_projs$player, FUN = seq_along)

ryoe_projs$csum <- ave(ryoe_projs$ryoe, ryoe_projs$player, FUN=cumsum)

rush_num_stats <- ryoe_projs %>%
  group_by(rush_num) %>%
  summarize(count = n(),
            avg_ryoe = mean(ryoe))

rush_num_stats %>%
  filter(count >= 5) %>%
  filter(avg_ryoe < 2.5) %>%
  filter(avg_ryoe > -2.5) %>%
  ggplot(aes(x = rush_num, y = avg_ryoe)) +
  geom_point(aes(size = count), alpha = 0.1, color = "purple") + 
  geom_smooth(se = FALSE, size = 3) +
  theme_bw() +
  labs(x = "Rush Number",
       y = "Average Rushing Yards Over Expected") +
  theme(legend.position = "none", 
        axis.title = element_text(size = 14))
ggsave('whitepaper-2.png', width = 15, height = 10, dpi = "retina")

ryoe_projs %>%
  mutate(Player = player) %>%
  filter(Player %in% c("Leonard Fournette", "Christian McCaffrey", "Dalvin Cook", 
                       "Joe Mixon", "Alvin Kamara")) %>%
  ggplot(aes(x = rush_num, y = csum)) +
  geom_smooth(aes(color = Player), se = FALSE, size = 2) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(fill = "Player",
       x = "Rush Number",
       y = "Cumulative Rushing Yards Over Expected") +
  theme(axis.title = element_text(size = 14),
        legend.text=element_text(size=12))
ggsave('whitepaper-3.png', width = 15, height = 10, dpi = "retina")

ind_rushing_data <- rushing_data %>%
  filter(!is.na(intended_run_position)) %>%
  filter(!is.na(run_position)) %>%
  mutate(hit_design = ifelse(intended_run_position == run_position, 1, 0))

gap_stats <- ind_rushing_data %>%
  filter(season == 2020) %>%
  group_by(player) %>%
  summarize(rushes = n(),
            gap_rate = mean(hit_design)) %>%
  filter(rushes >= 100)

nfl_ryoe_2020 <- ryoe_projs %>%
  filter(season == 2020) %>%
  group_by(player) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe)) %>%
  filter(rushes >= 100) %>%
  filter(player != "Lamar Jackson") %>%
  filter(player != "Cam Newton")
write.csv(nfl_ryoe_2020, "nfl_ryoe_2020.csv")

rb_yac_stats_2020 <- read.csv("~/CPOE/rb_yac_stats_2020.csv")

nfl_ryoe_2020 <- nfl_ryoe_2020 %>%
  left_join(rb_yac_stats_2020, by = c("player" = "receiver"))

nfl_ryoe_2020 %<>%
  mutate(usage = rushes + receptions)

nfl_ryoe_2020 %>%
  ggplot(aes(x = avg_ryoe, y = avg_yac)) +
  geom_point(aes(fill = ifelse(player == "Nick Chubb", "darkorange", "gray"), size = usage), color = "black", shape = 21) +
  ggrepel::geom_text_repel(aes(label = player), size = 4.5, box.padding = 0.35) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_hline(yintercept = mean(nfl_ryoe_2020$avg_yac, na.rm = T), linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = mean(nfl_ryoe_2020$avg_ryoe, na.rm = T), linetype = "dashed", alpha = 0.6) +
  labs(x = "Rushing Yards Over Expected",
       y = "Yards After Catch (YAC) Over Expected",
       title = "Rushing Yards Over Expected and YAC Over Expected, 2020",
       subtitle = "Minimum of 100 rushes to qualify, size of bubble is rushes + receptions",
       caption = "By Tej Seth | @tejfbanalytics | PFF") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))
ggsave('chubb.png', width = 15, height = 10, dpi = "retina")  


  





