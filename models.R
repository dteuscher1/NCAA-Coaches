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

obs <- sample(1:nrow(plays), nrow(plays) *.3, replace = FALSE)
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
    group_by(timeouts_possessions, game_coach) %>%
    summarize(expected = mean(pred),
              actual = mean(possession_score), 
              diff = actual - expected, 
              n = n()) #%>%
    #inner_join(coach_info %>% dplyr::select(Coach, team_market), by = c('game_coach' = 'Coach'))
coach_info <- read.csv("coaches_data.csv")

attempt %>% arrange(desc(diff)) %>% filter(n > 100)
attempt %>% arrange(diff) %>% filter(n > 100)

plot(attempt$expected, attempt$diff)

library(modelr)
plays_grid <- plays %>% 
    data_grid(possession_x_location = seq(min(possession_x_location), 450, length = 40),
              possession_y_location = seq(min(possession_y_location), max(possession_y_location), length = 40),
              score_diff = seq(-25, 25, by = 5), 
              time = seq(max(plays$time), min(plays$time), length = 15), 
              team_basket = "left")
library(broom)
grid_hat <- predict(rf, data = plays_grid)

ggplot(plays_grid, aes(x = possession_x_location, y = possession_y_location)) + 
    geom_tile(aes(fill = grid_hat$predictions), alpha = 0.7) + 
    scale_fill_gradient("Expected Points Per Possession", breaks = c(.5, 1, 1.5, 2)) +
    theme_minimal()

right <- plays %>% filter(team_basket == "left")
plot(right$possession_x_location, right$possession_y_location)

grid_hat$predictions