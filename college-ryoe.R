ncaa_rushing_data <- read_csv("~/Downloads/ncaa_rushing_data.csv")

ncaa_rushing_data <- ncaa_rushing_data %>%
  filter(!is.na(yards))

ncaa_rushing_data <- ncaa_rushing_data %>%
  mutate(seconds_left_in_half = case_when(
  quarter == 1 ~ seconds_left_in_quarter + 900,
  quarter == 2 ~ seconds_left_in_quarter,
  quarter == 3 ~ seconds_left_in_quarter + 900,
  quarter == 4 ~ seconds_left_in_quarter,
  quarter == 5 ~ 0
))

ncaa_rushing_data <- ncaa_rushing_data %>%
  mutate(score_diff = off_score_before - def_score_before)

ncaa_rushing_data %>% 
  filter(!is.na(box_players)) %>%
  filter(box_players < 9) %>%
  filter(box_players > 4) %>%
  ggplot(aes(x = yards_to_go, y = yards, group = as.factor(box_players), color = as.factor(box_players))) +
  geom_smooth(se = FALSE, size = 2) +
  theme_reach() +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Yards From Endzone",
       y = "Yards Gained",
       title = "How NCAA Rushing Yards Changes by Yardline and Defenders in the Box",
       subtitle = "Using PFF data from 2015-2020",
       color = "Box Players") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  theme(legend.position = "bottom")

ncaa_def_ypc <- ncaa_rushing_data %>%
  group_by(defense, season) %>%
  summarize(snaps = n(),
            def_ypc = mean(yards)) %>%
  filter(snaps >= 100)

ncaa_rushing_data_join <- ncaa_rushing_data %>%
  left_join(ncaa_def_ypc, by = c("defense", "season"))

ncaa_rushing_data_join$def_ypc[is.na(ncaa_rushing_data_join$def_ypc)] <- 4.918677

colSums(is.na(ncaa_rushing_data))

ncaa_rushing_data_join <- ncaa_rushing_data_join %>%
  filter(!is.na(box_players)) %>%
  select(offense, defense, player, player_id, season, game_id, distance, down, 
         quarter, seconds_left_in_half, yards_to_go, yards, box_players, def_ypc, score_diff)

colSums(is.na(ncaa_rushing_data_join))

ncaa_rushing_model_data <- ncaa_rushing_data_join %>%
  select(distance, down, seconds_left_in_half, yards_to_go, 
         yards, box_players, def_ypc, score_diff) %>%
  rename(label = yards) %>%
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

ncaa_hyper_grid <- expand.grid(max_depth = seq(3, 6, 1),
                          eta = seq(.2, .35, .01))
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
    eta = 0.21 #ideal eta
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
  group_by(season, player, offense) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe)) %>%
  filter(rushes >= 100) %>%
  arrange(-avg_ryoe) %>%
  arrange(season) %>%
  group_by(player) %>%
  mutate(next_avg_ryoe = lead(avg_ryoe))

summary(lm(next_avg_ryoe ~ avg_ryoe, data=ncaa_rushing_season_stats))$r.squared #0.13

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
  group_by(player, offense) %>%
  summarize(rushes = n(),
            avg_ryoe = mean(ryoe),
            sum_ryoe = sum(ryoe),
            los_rate = sum(ryoe < 0) / rushes,
            explosive_rate = sum(ryoe > 10) / rushes) %>%
  filter(rushes >= 200) %>%
  arrange(-avg_ryoe)

nfl_ncaa_rushing <- rushers_all_time %>%
  inner_join(ncaa_rushing_career, by = c("player"))

nfl_ncaa_rushing <- nfl_ncaa_rushing %>%
  rename(nfl_rushes = rushes.x,
         nfl_ryoe = avg_ryoe.x,
         ncaa_rushes = rushes.y,
         ncaa_ryoe = avg_ryoe.y)

summary(lm(nfl_ryoe ~ ncaa_ryoe, data=nfl_ncaa_rushing))$r.squared #0.226
summary(lm(nfl_ryoe ~ sum_ryoe, data=nfl_ncaa_rushing))$r.squared #0.244
summary(lm(nfl_ryoe ~ explosive_rate, data=nfl_ncaa_rushing))$r.squared #0.193
summary(lm(nfl_ryoe ~ los_rate, data=nfl_ncaa_rushing))$r.squared #0.120
summary(lm(nfl_ryoe ~ ncaa_rushes, data=nfl_ncaa_rushing))$r.squared #0.038

#Maybe there's survivorship bias

nfl_ncaa_rushing %>%
  ggplot(aes(x = ncaa_ryoe, y = nfl_ryoe)) +
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
  annotate("text", x = -0.3, y = 0.9, label = "R^2 = 0.22", size = 5)

nfl_ncaa_rushing %>%
  ggplot(aes(x = sum_ryoe, y = nfl_ryoe)) +
  geom_point(fill = "gray", shape = 21, color = "black", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  scale_color_identity(aesthetics = c("color", "fill")) +
  theme_reach() +
  ggrepel::geom_text_repel(aes(label = player), box.padding = 0.30) +
  labs(x = "NCAA Total RYOE",
       y = "NFL RYOE/Attempt",
       title = "How NCAA RYOE/Attempt Compares to NFL RYOE/Attempt",
       subtitle = "RYOE = Rushing Yards Over Expected, minimum of 200 rush attempts in both leagues") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  annotate("text", x = -120, y = 0.9, label = "R^2 = 0.24", size = 5)
  
draft_picks <- espnscrapeR::get_sharpe_data(dataset = "draft_picks")

draft_select <- draft_picks %>%
  select(round, pick, pfr_name, position) %>%
  filter(position %in% c("QB", "RB"))

ncaa_rushing_career_rbs <- ncaa_rushing_career %>%
  left_join(draft_select, by = c("player" = "pfr_name")) %>%
  filter(position == "RB") %>%
  filter(!is.na(pick))

summary(lm(pick ~ rushes, data=ncaa_rushing_career_rbs))$r.squared #0.05
summary(lm(pick ~ avg_ryoe,  data=ncaa_rushing_career_rbs))$r.squared #0.10
summary(lm(pick ~ sum_ryoe,  data=ncaa_rushing_career_rbs))$r.squared #0.18
summary(lm(pick ~ los_rate,  data=ncaa_rushing_career_rbs))$r.squared #0.08
summary(lm(pick ~ explosive_rate,  data=ncaa_rushing_career_rbs))$r.squared #0.09

draft_pick_lm <- lm(pick ~ sum_ryoe + los_rate + explosive_rate, data = ncaa_rushing_career_rbs)
summary(draft_pick_lm)

library(car)
vif(draft_pick_lm)



teams <- read_csv(url("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/teams/teams_colors_logos.csv"))

ncaa_rates <- ncaa_ryoe_projs %>%
  filter(season == 2020) %>%
  group_by(player, offense) %>%
  summarize(los_rate = sum(ryoe < 0) / n(),
            explosive_rate = sum(ryoe > 10) / n(),
            rushes = n()) %>% 
  filter(rushes >= 100) 


