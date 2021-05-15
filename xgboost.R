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

source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")

seasons <- 2010:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

rush_attempts <- pbp_rp %>%
  filter(rush_attempt == 1, qb_scramble == 0, qb_dropback == 0)

rush_attempts %>%
  group_by(season, defteam) %>%
  summarize(def_ypc = mean(yards_gained),
            count = n()) %>%
  filter(count >= 100) %>%
  select(-count) -> def_ypc

rush_attempts <- rush_attempts %>%
  left_join(def_ypc, by = c("season", "defteam"))

rush_attempts2 <- rush_attempts %>%
  mutate(yards_rushed = case_when(yards_gained > 20 ~ 20L,
                                  yards_gained < -5 ~ -5L,
                                TRUE ~ as.integer(yards_gained)),
         label = yards_rushed + 5L)

rush_attempts3 <- rush_attempts2 %>%
  mutate(run_left_end = if_else((run_gap == "end" & run_location == "left"), 1, 0),
         run_left_guard = if_else((run_gap == "guard" & run_location == "left"), 1, 0),
         run_left_tackle = if_else((run_gap == "tackle" & run_location == "left"), 1, 0),
         run_right_end = if_else((run_gap == "end" & run_location == "right"), 1, 0),
         run_right_guard = if_else((run_gap == "guard" & run_location == "right"), 1, 0),
         run_right_tackle = if_else((run_gap == "tackle" & run_location == "right"), 1, 0),
         run_middle = if_else((run_location == "middle"), 1, 0))

rush_attempts4 <- rush_attempts3 %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
         no_score_prob, ep, wp, def_ypc, label) %>%
  filter(!is.na(label)) %>%
  filter(!is.na(down))

smp_size <- floor(0.80 * nrow(rush_attempts4))
set.seed(123)
ind <- sample(seq_len(nrow(rush_attempts4)), size = smp_size)
ind_train <- rush_attempts4[ind, ]
ind_test <- rush_attempts4[-ind, ]

full_train <- xgboost::xgb.DMatrix(as.matrix(ind_train %>% select(-label)), label = as.integer(ind_train$label))

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 26,
    eta = .012,
    gamma = 1,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 8,
    min_child_weight = 21
  )

ryoe_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

imp <- xgb.importance(colnames(ind_train), model = ryoe_model)
xgb.plot.importance(imp)

rushes_2020 <- rush_attempts3 %>%
  filter(season == 2020) %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
         no_score_prob, ep, wp, def_ypc, xpass) %>%
  mutate(index = 1:n())

seasons <- 1999:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(old_game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

rush_attempts <- pbp_rp %>%
  filter(rush_attempt == 1, qb_scramble == 0, qb_dropback == 0)

rush_attempts %>%
  group_by(season, defteam) %>%
  summarize(def_ypc = mean(yards_gained),
            count = n()) %>%
  filter(count >= 100) %>%
  select(-count) -> def_ypc

rush_attempts <- rush_attempts %>%
  left_join(def_ypc, by = c("season", "defteam"))

rush_attempts2 <- rush_attempts %>%
  mutate(yards_rushed = case_when(yards_gained > 20 ~ 20L,
                                  yards_gained < -5 ~ -5L,
                                  TRUE ~ as.integer(yards_gained)),
         label = yards_rushed + 5L)

rush_attempts3 <- rush_attempts2 %>%
  mutate(run_left_end = if_else((run_gap == "end" & run_location == "left"), 1, 0),
         run_left_guard = if_else((run_gap == "guard" & run_location == "left"), 1, 0),
         run_left_tackle = if_else((run_gap == "tackle" & run_location == "left"), 1, 0),
         run_right_end = if_else((run_gap == "end" & run_location == "right"), 1, 0),
         run_right_guard = if_else((run_gap == "guard" & run_location == "right"), 1, 0),
         run_right_tackle = if_else((run_gap == "tackle" & run_location == "right"), 1, 0),
         run_middle = if_else((run_location == "middle"), 1, 0))

rush_attempts4 <- rush_attempts3 %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
         no_score_prob, ep, wp, def_ypc, label) %>%
  filter(!is.na(label)) %>%
  filter(!is.na(down))

smp_size <- floor(0.80 * nrow(rush_attempts4))
set.seed(123)
ind <- sample(seq_len(nrow(rush_attempts4)), size = smp_size)
ind_train <- rush_attempts4[ind, ]
ind_test <- rush_attempts4[-ind, ]

full_train <- xgboost::xgb.DMatrix(as.matrix(ind_train %>% select(-label)), label = as.integer(ind_train$label))

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 26,
    eta = .012,
    gamma = 1,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 8,
    min_child_weight = 21
  )

ryoe_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

rushes_all <- rush_attempts3 %>%
  select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
         no_score_prob, ep, wp, def_ypc) %>%
  mutate(index = 1:n())

ryoe_all <- stats::predict(ryoe_model,
                           as.matrix(rushes_all %>%
                                       select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
                                              game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
                                              no_score_prob, ep, wp, def_ypc))) %>%
  tibble::as_tibble() %>%
  dplyr::rename(prob = "value") %>%
  dplyr::bind_cols(purrr::map_dfr(seq_along(rushes_all$index), function(x) {
    tibble::tibble("xyds_rushed" = -5:20,
                   "down" = rushes_all$down[[x]],
                   "yardline_100" = rushes_all$yardline_100[[x]],
                   "quarter_seconds_remaining" = rushes_all$quarter_seconds_remaining[[x]],
                   "half_seconds_remaining" = rushes_all$half_seconds_remaining[[x]],
                   "game_seconds_remaining" = rushes_all$game_seconds_remaining[[x]],
                   "qtr" = rushes_all$qtr[[x]],
                   "goal_to_go" = rushes_all$goal_to_go[[x]],
                   "ydstogo" = rushes_all$ydstogo[[x]],
                   "shotgun" = rushes_all$shotgun[[x]],
                   "no_huddle" = rushes_all$no_huddle[[x]],
                   "no_score_prob" = rushes_all$no_score_prob[[x]],
                   "ep" = rushes_all$ep[[x]],
                   "wp" = rushes_all$wp[[x]],
                   "index" = rushes_all$index[[x]]) 
  })) %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(max_loss = dplyr::if_else(.data$yardline_100 < 95, -5L, as.integer(.data$yardline_100 - 99L)),
                max_gain = dplyr::if_else(.data$yardline_100 > 20, 20L, as.integer(.data$yardline_100)),
                cum_prob = cumsum(.data$prob),
                prob = dplyr::case_when(.data$xyds_rushed == .data$max_loss ~ .data$prob,
                                        .data$xyds_rushed == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
                                        TRUE ~ .data$prob),
                yardline_100 = .data$yardline_100 - .data$xyds_rushed) %>%
  dplyr::filter(.data$xyds_rushed >= .data$max_loss, .data$xyds_rushed <= .data$max_gain) %>%
  dplyr::select(-.data$cum_prob) %>%
  dplyr::summarise(x_rush_yards = sum(.data$prob * .data$xyds_rushed)) %>%
  ungroup() 

rushes_all_2 <- rushes_all %>%
  inner_join(ryoe_all)

pbp_all <- pbp_rp %>%
  inner_join(rushes_all_2) %>%
  select(season, posteam, defteam, rusher_player_name, yards_gained, x_rush_yards, epa, week, qtr, down, ydstogo, play_id, yardline_100, xpass) %>%
  filter(week <= 17) %>%
  mutate(ryoe = yards_gained - x_rush_yards + 1.2)

#pbp_all %>% group_by(season, posteam, defteam, week, play_id) %>% summarize(count = n()) %>% arrange(desc(count))

pbp_all_un <- pbp_all[!duplicated(pbp_all[,c('season', 'posteam', 'defteam', 'play_id')]),]

pbp_all_un <- pbp_all_un %>%
  mutate(rusher = rusher_player_name,
         rusher_player_name = ifelse(rusher_player_name == "W.Gallman Jr.", "W.Gallman", rusher)) %>%
  select(-rusher)

pbp_all_un <- pbp_all_un %>%
  mutate(rusher = rusher_player_name,
         rusher_player_name = ifelse(rusher_player_name == "M.Ingram II", "M.Ingram", rusher)) %>%
  select(-rusher)

pbp_all_un <- pbp_all_un %>%
  filter(season <= 2020)

pbp_all_un_1 <- pbp_all_un %>%
  filter(season <= 2010)

pbp_all_un_2 <- pbp_all_un %>%
  filter(season > 2010)

write.csv(pbp_all_un_1, "pbp_all_un_1.csv")
write.csv(pbp_all_un_2, "pbp_all_un_2.csv")


    




    