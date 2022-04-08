library(tidyverse)
plays <- read.csv("model_data.csv") %>%
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(time)) %>%
    filter(!is.na(game_coach))
rf_preds <- readRDS("rf_preds.RDS")
attempt <- plays %>% 
    mutate(pred = rf_preds) %>% 
    group_by(timeouts_possessions, game_coach) %>%
    summarize(expected = round(mean(pred), digits = 3),
              actual = round(mean(possession_score), digits = 3), 
              diff = round(actual - expected, digits = 3), 
              n = n()) %>%
    arrange(desc(diff))
names(attempt) <- c("Team", "Coach", "Expected Points", "Actual Points", "Difference", "Number of Plays")
