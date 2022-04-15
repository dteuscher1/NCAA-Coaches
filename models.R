## Author: David Teuscher
## Last Edited: 04.12.22
## This script provides the code for the Shiny app presenting the results
## of this analysis
##############################################################################

# Load packages
library(DataExplorer)
library(tidyverse)

plays <- read.csv("model_data_v2.csv") 

# %>%
#     dplyr::select(-X, -game_number, -possession_ids, -half) %>%
#     mutate(team_basket = factor(team_basket)) %>%
#     filter(!is.na(possession_x_location)) %>%
#     filter(!is.na(time)) %>%
#     filter(!is.na(game_coach))

plot_missing(plays)

library(tidymodels)
plot_missing(plays)

set.seed(222)
# Put 3/4 of the data into the training set 
data_split <- initial_split(plays2, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


plays_rec <- recipe(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train_data)

cores <- parallel::detectCores()

rf_mod <- rand_forest(trees = 1000) %>%
    set_engine("ranger", num.threads = cores) %>%
    set_mode("regression")

plays_wflow <- workflow() %>% 
    add_model(rf_mod) %>% 
    add_recipe(plays_rec)

plays_fit <- plays_wflow %>% 
    fit(data = train_data)

folds <- vfold_cv(train_data, v = 10)
folds

plays_fit_rs <- plays_wflow %>% 
    fit_resamples(folds)

collect_metrics(plays_fit_rs)

plays_testing_pred <- predict(plays_fit, plays2) %>% 
    bind_cols(plays2 %>% select(possession_score))

plays_testing_pred %>% 
    rmse(possession_score, .pred)

plays2 <- plays %>% 
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(season)) %>%
    filter(!is.na(game_coach)) %>%
    filter(!is.na(time)) %>% 
    filter((team_basket == "right" & possession_x_location > 550) | (team_basket == "left" & possession_x_location < 450)) %>%
    filter(possession_score <= 3)

plot_missing(plays2)

lm_model <- lm(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = plays)
library(ranger)
rf <- ranger(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = plays)

obs <- sample(1:nrow(plays2), nrow(plays2) *.3, replace = FALSE)
train <- plays2[-obs,]
test <- plays2[obs, ]
lm_train <-  lm(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train)
model <- ranger(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train)
rf_preds <- predict(model, test)
lm_preds <- predict(lm_train, test)
rmse <- (rf_preds$predictions - test$possession_score)^2 %>% sqrt() %>% mean()
rmse2 <- (lm_preds - test$possession_score)^2 %>% sqrt() %>% mean()
attempt <- plays2 %>% 
    mutate(pred = plays_testing_pred$.pred) %>% 
    group_by(timeouts_possessions) %>%
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

right <- plays2 %>% filter(team_basket == "right")
plot(right$possession_x_location, right$possession_y_location)

grid_hat$predictions