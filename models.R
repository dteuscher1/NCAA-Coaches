plays <- read.csv("model_data.csv") %>%
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(time)) %>%
    filter(!is.na(game_coach))

library(DataExplorer)
library(glmnet)
plot_missing(plays)
library(tidyverse)
glimpse(plays)

lm_model <- lm(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = plays)
library(ranger)
rf <- ranger(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = plays)

obs <- sample(nrow(plays) *.3, 1:nrow(plays), replace = FALSE)
train <- plays[-obs,]
test <- plays[obs, ]
lm_train <-  lm(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train)
model <- ranger(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train)
rf_preds <- predict(model, test)
lm_preds <- predict(lm_train, test)
rmse <- (rf_preds$predictions - test$possession_score)^2 %>% sqrt() %>% mean()
rmse2 <- (lm_preds - test$possession_score)^2 %>% sqrt() %>% mean()
attempt <- plays %>% 
    mutate(pred = rf$predictions) %>% 
    group_by(game_coach) %>%
    summarize(expected = mean(pred),
              actual = mean(possession_score), 
              diff = actual - expected, 
              n = n()) %>%
    inner_join(coach_info %>% dplyr::select(Coach, team_market), by = c('game_coach' = 'Coach'))
coach_info <- read.csv("coaches_data.csv")

attempt %>% arrange(desc(diff)) %>% filter(n > 100)
attempt %>% arrange(diff) %>% filter(n > 100)

plot(attempt$expected, attempt$diff)

