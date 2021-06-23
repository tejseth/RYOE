ncaa_rushing_data <- read_csv("~/Downloads/ncaa_rushing_data.csv")

ncaa_rushing_data <- ncaa_rushing_data %>%
  filter(!is.na(yards))

ncaa_rushing_data <- ncaa_rushing_data %>%
  mutate(seconds_left_in_half = case_when(
  quarter == 1 ~ seconds_left_in_quarter + 900,
  quarter == 2 ~ seconds_left_in_quarter,
  quarter == 3 ~ seconds_left_in_quarter + 900,
  quarter == 4 ~ seconds_left_in_quarter,
  quarter >= 5 ~ 0
))

ncaa_rushing_data <- ncaa_rushing_data %>%
  filter(quarter < 5)

ncaa_rushing_data <- ncaa_rushing_data %>%
  mutate(score_diff = off_score_before - def_score_before)

ncaa_rushing_data <- ncaa_rushing_data %>%
  mutate(half = case_when(
    quarter == 1 ~ "First Half",
    quarter == 2 ~ "First Half",
    quarter == 3 ~ "Second Half",
    quarter == 4 ~ "Second Half",
  ))

nrow(ncaa_rushing_data)

ncaa_rushing_data <- ncaa_rushing_data %>%
  filter(run_position != "QBK")

nrow(ncaa_rushing_data)

ncaa_rushing_data %>% 
  filter(!is.na(box_players)) %>%
  filter(box_players < 9) %>%
  filter(box_players > 4) %>%
  filter(distance <= 15) %>%
  ggplot(aes(x = distance, y = yards, group = as.factor(box_players), color = as.factor(box_players))) +
  geom_smooth(se = FALSE, size = 2) +
  theme_reach() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Distance to Sticks",
       y = "Yards Gained",
       title = "How NCAA Rushing Yards Changes by Distance to Sticks and Defenders in the Box",
       subtitle = "Using PFF data from 2015-2020, QB Kneeldowns excluded",
       color = "Box Players") +
  scale_x_reverse(breaks = pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme(legend.position = "bottom") +
  facet_wrap(~half)

ncaa_def_ypc <- ncaa_rushing_data %>%
  dplyr::group_by(defense, season) %>%
  dplyr::summarize(snaps = n(),
            def_ypc = mean(yards)) %>%
  filter(snaps >= 100)

ncaa_rushing_data_join <- ncaa_rushing_data %>%
  left_join(ncaa_def_ypc, by = c("defense", "season"))

ncaa_rushing_data_join$def_ypc[is.na(ncaa_rushing_data_join$def_ypc)] <- mean(ncaa_rushing_data_join$yards)

colSums(is.na(ncaa_rushing_data))

ncaa_rushing_data_join <- ncaa_rushing_data_join %>%
  filter(!is.na(box_players)) %>%
  select(offense, defense, player, player_id, season, game_id, distance, down, concept_1,
         quarter, seconds_left_in_half, yards_to_go, yards, box_players, def_ypc, score_diff)

colSums(is.na(ncaa_rushing_data_join))

ncaa_rushing_model_data <- ncaa_rushing_data_join %>%
  select(distance, down, seconds_left_in_half, yards_to_go, 
         yards, box_players, def_ypc, score_diff) %>%
  dplyr::rename(label = yards) %>%
  select(label, everything())

ncaa_smp_size <- floor(0.70 * nrow(ncaa_rushing_model_data))
set.seed(2016) #go blue
ncaa_ind <- sample(seq_len(nrow(ncaa_rushing_model_data)), size = ncaa_smp_size)
ncaa_train <- as.matrix(ncaa_rushing_model_data[ncaa_ind, ])
ncaa_test <- as.matrix(ncaa_rushing_model_data[-ncaa_ind, ])

dim(ncaa_train)

ncaa_ryoe_model <-
  xgboost(
    data = ncaa_train[, 2:8],
    label = ncaa_train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )   

ncaa_imp <- xgb.importance(colnames(ncaa_train), model = ncaa_ryoe_model)
xgb.plot.importance(ncaa_imp)

pred_xgb <- predict(ncaa_ryoe_model, ncaa_test[, 2:8])

yhat <- pred_xgb
y <- ncaa_test[, 1]
postResample(yhat, y)

ncaa_hyper_grid <- expand.grid(max_depth = seq(3, 5, 1),
                          eta = seq(.2, .28, .01))
xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(ncaa_hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = ncaa_train[, 2:7],
    label = ncaa_train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = ncaa_hyper_grid$max_depth[j],
    eta = ncaa_hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}

#ideal hyperparamters
ncaa_hyper_grid[which.min(xgb_test_rmse), ]

ncaa_rushing_model <-
  xgboost(
    data = ncaa_train[, 2:8],
    label = ncaa_train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 4, #ideal max depth
    eta = 0.24 #ideal eta
  )  

ncaa_imp <- xgb.importance(colnames(ncaa_train), model = ncaa_rushing_model)
xgb.plot.importance(ncaa_imp)

pred_xgb <- predict(ncaa_ryoe_model, ncaa_test[, 2:8])

yhat <- pred_xgb
y <- ncaa_test[, 1]
postResample(yhat, y)

ncaa_rushing_preds <- as.data.frame(
  matrix(predict(ncaa_rushing_model, as.matrix(ncaa_rushing_model_data %>% select(-label))))
) %>%
  dplyr::rename(exp_yards = V1)

ncaa_ryoe_projs <- cbind(ncaa_rushing_data_join, ncaa_rushing_preds)

f = mean(ncaa_ryoe_projs$yards) - mean(ncaa_ryoe_projs$exp_yards)

ncaa_ryoe_projs <- ncaa_ryoe_projs %>%
  mutate(ryoe = yards - exp_yards + f)

ncaa_rushing_season_stats <- ncaa_ryoe_projs %>%
  dplyr::group_by(season, player, offense) %>%
  dplyr::summarize(rushes = n(),
            avg_ryoe = mean(ryoe),
            los_rate = sum(ryoe > 0) / n(),
            explosive_rate = sum(ryoe > 10) / n()) %>%
  dplyr::filter(rushes >= 100) %>%
  dplyr::arrange(-avg_ryoe) %>%
  dplyr::arrange(season) %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(next_rushes = lead(rushes),
         next_avg_ryoe = lead(avg_ryoe),
         next_los_rate = lead(los_rate),
         next_explosive_rate = lead(explosive_rate))

summary(lm(next_rushes ~ rushes, data=ncaa_rushing_season_stats))$r.squared #0.14
summary(lm(next_avg_ryoe ~ avg_ryoe, data=ncaa_rushing_season_stats))$r.squared #0.13
summary(lm(next_los_rate ~ los_rate, data=ncaa_rushing_season_stats))$r.squared #0.13
summary(lm(next_explosive_rate ~ explosive_rate, data=ncaa_rushing_season_stats))$r.squared #0.11

ncaa_rushing_season_stats %>%
  ggplot(aes(x = avg_ryoe, y = next_avg_ryoe)) +
  geom_point(shape = 21, fill = "orange", color = "black", alpha = 0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  theme_reach() +
  labs(x = "RYOE/att in Year N",
       y = "RYOE/att in Year N + 1",
       title = "Stability of College Rushing Yards Over Expected",
        subtitle = "2014-2020, minimum of 100 rushes in both years") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  annotate("text", x = -1.7, y = 3.25, label = "R = 0.36", size = 5)

ncaa_rushing_stats_20 <- ncaa_rushing_season_stats %>%
  filter(season == 2020)

ncaa_rushing_career <- ncaa_ryoe_projs %>%
  dplyr::group_by(player, offense) %>%
  dplyr::summarize(ncaa_rushes = n(),
            ncaa_actual_ypc = mean(yards),
            ncaa_avg_ryoe = mean(ryoe),
            ncaa_sum_ryoe = sum(ryoe),
            ncaa_los_rate = sum(ryoe > 0) / ncaa_rushes,
            ncaa_explosive_rate = sum(ryoe > 10) / ncaa_rushes) %>%
  dplyr::filter(ncaa_rushes >= 200) %>%
  dplyr::arrange(-ncaa_avg_ryoe)

nfl_ncaa_rushing <- rushers_all_time %>%
  inner_join(ncaa_rushing_career, by = c("player"))

summary(lm(nfl_ryoe ~ ncaa_actual_ypc, data=nfl_ncaa_rushing))$r.squared #0.18
summary(lm(nfl_ryoe ~ ncaa_avg_ryoe, data=nfl_ncaa_rushing))$r.squared #0.21
summary(lm(nfl_ryoe ~ ncaa_sum_ryoe, data=nfl_ncaa_rushing))$r.squared #0.22
summary(lm(nfl_ryoe ~ ncaa_explosive_rate, data=nfl_ncaa_rushing))$r.squared #0.18
summary(lm(nfl_ryoe ~ ncaa_los_rate, data=nfl_ncaa_rushing))$r.squared #0.10
summary(lm(nfl_ryoe ~ ncaa_rushes, data=nfl_ncaa_rushing))$r.squared #0.03
summary(lm(nfl_explosive_rate ~ ncaa_explosive_rate, data=nfl_ncaa_rushing))$r.squared #0.32
summary(lm(nfl_los_rate ~ ncaa_los_rate, data=nfl_ncaa_rushing))$r.squared #0.10

#Maybe there's survivorship bias

nfl_teams <- ryoe_projs %>%
  dplyr::group_by(player, player_id, offense) %>%
  dplyr::summarize(count = n()) %>%
  top_n(1) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

nfl_ncaa_rushing <- nfl_ncaa_rushing %>%
  left_join(nfl_teams, by = c("player", "player_id"))

nfl_ncaa_rushing %>%
  ggplot(aes(x = ncaa_avg_ryoe, y = nfl_ryoe)) +
  geom_point(aes(fill = ifelse(player == "Lamar Jackson", "purple", "gray")), shape = 21, color = "black", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  scale_color_identity(aesthetics = c("color", "fill")) +
  theme_reach() +
  ggrepel::geom_text_repel(aes(label = player), box.padding = 0.30) +
  labs(x = "NCAA RYOE/Attempt",
       y = "NFL RYOE/Attempt",
       title = "How NCAA RYOE/Attempt Compares to NFL RYOE/Attempt",
       subtitle = "RYOE = Rushing Yards Over Expected, minimum of 200 rush attempts in both leagues") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  annotate("text", x = 0.2, y = 0.9, label = "R^2 = 0.22", size = 5)

nfl_ncaa_rushing %>%
  filter(player != "Lamar Jackson") %>%
  ggplot(aes(x = ncaa_explosive_rate, y = nfl_explosive_rate)) +
  geom_point(aes(fill = team_color, color = team_color2), shape = 21, size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  scale_color_identity(aesthetics = c("color", "fill")) +
  theme_reach() +
  ggrepel::geom_text_repel(aes(label = player), box.padding = 0.30) +
  labs(x = "NCAA Explosive Rate (RYOE > 10)",
       y = "NFL Explosive Rate (RYOE > 10)",
       title = "How NCAA Explosive Run Rate Compares to NFL Explosive Run Rate",
       subtitle = "RYOE = Rushing Yards Over Expected, minimum of 200 rush attempts in both leagues") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5))  +
  annotate("text", x = 0.05, y = 0.075, label = "R^2 = 0.35", size = 5)

nfl_ncaa_long <- nfl_ncaa_rushing %>%
  select(player, nfl_ryoe, ncaa_actual_ypc, ncaa_avg_ryoe, ncaa_los_rate, ncaa_explosive_rate)

nfl_ncaa_long_2 <- nfl_ncaa_long %>%
  pivot_longer(cols = c(ncaa_actual_ypc, ncaa_avg_ryoe, ncaa_los_rate, ncaa_explosive_rate), names_to = "type", values_to = "value")

nfl_ncaa_long_2 <- nfl_ncaa_long_2 %>%
  mutate(type2 = case_when(
    type == "ncaa_actual_ypc" ~ "Yards Per Carry",
    type == "ncaa_avg_ryoe" ~ "Average RYOE",
    type == "ncaa_los_rate" ~ "Positive RYOE Rate",
    type == "ncaa_explosive_rate" ~ "Explosive RYOE Rate"
  ))

dat_text <- data.frame(
  label = c("R^2 = 0.16", "R^2 = 0.20", "R^2 = 0.08", "R^2 = 0.22"),
  type2   = c("Yards Per Carry", "Average RYOE", "Positive RYOE Rate", "Explosive RYOE Rate")
)

p <- nfl_ncaa_long_2 %>%
  ggplot(aes(x = value, y = nfl_ryoe)) +
  geom_point(aes(fill = type), alpha = 0.85, shape = 21, color = "black", size = 4) +
  geom_smooth(se = FALSE, color = "gray", method = "lm") +
  theme_reach() +
  scale_fill_brewer(palette = "Accent") +
  facet_wrap(~type2, scales = "free_x") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  labs(x = "Value",
       y = "RYOE/Attempt in the NFL",
       title = "Explosiveness Does a Better Job of Predicting NFL Performance than Yards Per Carry") +
  theme(strip.text.x = element_text(size = 14))

p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label),
  hjust   = -6,
  vjust   = -1.5
)
  
draft_picks <- espnscrapeR::get_sharpe_data(dataset = "draft_picks")

draft_select <- draft_picks %>%
  select(round, pick, pfr_name, position) %>%
  filter(position %in% c("QB", "RB"))

ncaa_rushing_career_rbs <- ncaa_rushing_career %>%
  left_join(draft_select, by = c("player" = "pfr_name")) %>%
  filter(position == "RB") %>%
  filter(!is.na(pick)) %>% 
  filter(round <= 7)

ncaa_draft_long <- ncaa_rushing_career_rbs %>%
  pivot_longer(cols = c(ncaa_avg_ryoe, ncaa_sum_ryoe, ncaa_los_rate, ncaa_explosive_rate), names_to = "type", values_to = "value")

ncaa_draft_long <- ncaa_draft_long %>%
  mutate(type2 = case_when(
    type == "ncaa_avg_ryoe" ~ "Average RYOE",
    type == "ncaa_explosive_rate" ~ "Explosive RYOE Rate",
    type == "ncaa_los_rate" ~ "Positive RYOE Rate",
    type == "ncaa_sum_ryoe" ~ "Total RYOE"
  ))

ncaa_draft_long %>%
  ggplot(aes(x = value, y = pick, color = type2)) +
  geom_point(aes(fill = type), alpha = 0.85, shape = 21, color = "black", size = 4) +
  geom_smooth(se = FALSE, color = "gray", method = "lm") +
  theme_reach() +
  scale_fill_brewer(palette = "Accent") +
  facet_wrap(~type2, scales = "free_x") +
  scale_y_reverse(breaks = pretty_breaks(n = 10)) +
  labs(x = "Value",
       y = "Draft Pick",
       title = "How Each RYOE Variable Affects Draft Position",
       subtitle = "Total RYOE has the biggest effect on draft position while explosive rate has the smallest of the 4 metrics") +
  theme(strip.text.x = element_text(size = 14))

summary(lm(pick ~ rushes, data=ncaa_rushing_career_rbs))$r.squared #0.05
summary(lm(pick ~ avg_ryoe,  data=ncaa_rushing_career_rbs))$r.squared #0.10
summary(lm(pick ~ sum_ryoe,  data=ncaa_rushing_career_rbs))$r.squared #0.19
summary(lm(pick ~ los_rate,  data=ncaa_rushing_career_rbs))$r.squared #0.08
summary(lm(pick ~ explosive_rate,  data=ncaa_rushing_career_rbs))$r.squared #0.09

draft_pick_lm <- lm(pick ~ sum_ryoe + los_rate + explosive_rate, data = ncaa_rushing_career_rbs)
summary(draft_pick_lm)

library(car)
vif(draft_pick_lm)

ncaa_rushing_career_rbs %>% 
  filter(round < 8) %>%
  ggplot(aes(x = round, y = ncaa_sum_ryoe, fill = ncaa_sum_ryoe)) + 
  geom_quasirandom(pch = 21, size = 5.5, width = 0.2) + 
  ggrepel::geom_text_repel(aes(label = player), size = 2, box.padding = 0.35) +
  scale_fill_viridis_c("",  guide = FALSE) + 
  theme_reach() +
  labs(
    title = "Total College Rushing Yards Over Expected is the Best Indicator of Draft Position",
    subtitle = "Running backs drafted from 205 to 2020, R^2 = 0.19",
    x = "Round Drafted",
    y = "Total College RYOE"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 

teams <- read_csv(url("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/teams/teams_colors_logos.csv"))

ncaa_rates <- ncaa_ryoe_projs %>%
  filter(season == 2020) %>%
  group_by(player, offense) %>%
  summarize(los_rate = sum(ryoe < 0) / n(),
            explosive_rate = sum(ryoe > 10) / n(),
            rushes = n()) %>% 
  filter(rushes >= 100) 

ncaa_ryoe_projs <- ncaa_ryoe_projs %>%
  mutate(half = case_when(
    quarter <= 2 ~ "First Half",
    quarter > 2 ~ "Second Half"
  ))

ncaa_ryoe_projs %>% 
  filter(!is.na(box_players)) %>%
  filter(box_players < 9) %>%
  filter(box_players > 4) %>%
  ggplot(aes(x = seconds_left_in_half, y = exp_yards, group = as.factor(box_players), color = as.factor(box_players))) +
  geom_smooth(se = FALSE, size = 2) +
  theme_reach() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Seconds Left in Half",
       y = "Expected Rushing yards",
       title = "How Expected Rushing Yards Changes by Time and Defenders in the Box",
       subtitle = "Using PFF data from 2015-2020",
       color = "Box Players") +
  scale_x_reverse(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme(legend.position = "bottom") +
  facet_wrap(~half)

xpart_1 <- ncaa_rushing_career %>%
  ungroup() %>%
  select(ncaa_avg_ryoe) %>%
  rename(avg_ryoe = ncaa_avg_ryoe) %>%
  mutate(type = "NCAA")

part_2 <- rushers_all_time %>%
  ungroup() %>%
  select(avg_ryoe) %>%
  mutate(type = "NFL")

part_3 <- rbind(part_1, part_2)

part_3 %>%
  group_by(type) %>%
  summarize(basic_rate = sum(avg_ryoe > -0.5 & avg_ryoe < 0.5) / n())

part_3 %>%
  filter(avg_ryoe < 2.5) %>%
  ggplot(aes(x = avg_ryoe)) +
  geom_density(aes(fill = type)) +
  scale_fill_brewer(palette = "Accent") +
  theme_reach() +
  geom_vline(xintercept = -0.5, linetype = "dashed", alpha = 0.8) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.8) +
  facet_grid(rows = vars(type)) +
  labs(y = "Rusher Density %",
       x = "Each Rusher's RYOE/Attempt",
       title = "Running Backs Matter More in College",
       subtitle = "83% of NFL running backs fall between -0.5 and 0.5 RYOE/attempt however it's only 55% in college") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  theme(strip.text.y = element_text(size = 14, colour = "black", face = "bold", angle = 360))
ggsave('rushing-1.png', width = 14, height = 10, dpi = "retina")






