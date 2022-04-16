## Author: David Teuscher
## Last Edited: 04.12.22
## This script provides the code for the Shiny app presenting the results
## of this analysis
##############################################################################

# Load packages
library(DataExplorer)
library(tidyverse)
library(tidymodels)

plays <- read.csv("model_data_v2.csv") 


plot_missing(plays)



plays <- plays %>%
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(time)) %>%
    filter(!is.na(game_coach)) %>%
    filter(!is.na(season))  %>% 
    filter((team_basket == "right" & possession_x_location > 550) | (team_basket == "left" & possession_x_location < 450)) %>%
    filter(possession_score <= 3)

plot_missing(plays)
set.seed(222)
plays_small <- plays %>% sample_n(15000)
# Put 3/4 of the data into the training set 
data_split <- initial_split(plays_small, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


plays_rec <- recipe(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train_data) %>%
    step_dummy(team_basket)

cores <- parallel::detectCores() - 1

tune_spec <- rand_forest(mtry = tune(),
                         trees = 1000,
                         min_n = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger", num.threads = cores)

xgb_model <- boost_tree(trees = 1000,
                        tree_depth = tune(),
                        min_n = tune(), 
                        loss_reduction = tune(),                     ## first three: model complexity
                        sample_size = tune(), 
                        mtry = tune(),         ## randomness
                        learn_rate = tune()) %>%
    set_mode("regression") %>%
    set_engine("xgboost")
xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_data),
    learn_rate(),
    size = 20
)

xgb_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(xgb_model)

plays_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(tune_spec)

folds <- vfold_cv(train_data, v = 5)

plays_tune <- plays_wflow %>% 
    tune_grid(resamples = folds,
         grid = 25) 

doParallel::registerDoParallel()

xgb_tune <- xgb_wflow %>%
    tune_grid(resamples = folds, 
              grid = xgb_grid)

plays_tune %>% collect_metrics(metric = "rmse")

xgb_tune %>% collect_metrics(metric = "rmse")

plays_tune %>% select_best(metric = "rmse")

xgb_tune %>% select_best(metric = "rmse")

data_split <- initial_split(plays, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

plays_model <- rand_forest(mtry = 2,
                           trees = 1000,
                           min_n = 30) %>%
    set_mode("regression") %>%
    set_engine("ranger", num.threads = cores)

xgb_model <- boost_tree(trees = 1000,
                        tree_depth = 8,
                        min_n = 6, 
                        loss_reduction = .518,                     ## first three: model complexity
                        sample_size = .9, 
                        mtry = 3,         ## randomness
                        learn_rate = .00821) %>%
    set_mode("regression") %>%
    set_engine("xgboost")

plays_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(plays_model)

xgb_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(xgb_model)

plays_fit_rs <- plays_wflow %>% 
    fit_resamples(folds)

xgb_fit_rs <- xgb_wflow %>%
    fit_resamples(folds)

collect_metrics(plays_fit_rs)
collect_metrics(xgb_fit_rs)

plays_fit <- plays_wflow %>%
    fit(train_data)

xgb_fit <- xgb_wflow %>%
    fit(train_data)

plays_testing_pred <- predict(plays_fit, test_data) %>% 
    bind_cols(test_data %>% select(possession_score)) %>%
    bind_cols(predict(xgb_fit, test_data)) %>%
    rename(RF = .pred...1,
           XGB = .pred...3)

plays_testing_pred %>% 
    rmse(possession_score, RF)

plays_testing_pred %>% 
    rmse(possession_score, XGB)

plays_fit_all <- plays_wflow %>%
    fit(plays)
xgb_fit_all <- xgb_wflow %>%
    fit(plays)

plays_testing_pred <- predict(plays_fit_all, test_data) %>% 
    bind_cols(test_data %>% select(possession_score)) %>%
    bind_cols(predict(xgb_fit_all, test_data)) %>%
    rename(RF = .pred...1,
           XGB = .pred...3)

plays_testing_pred %>% 
    rmse(possession_score, RF)

plays_testing_pred %>% 
    rmse(possession_score, XGB)

plays2 <- plays %>% 
    #dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(season)) %>%
    filter(!is.na(game_coach)) %>%
    filter(!is.na(time)) %>% 
    filter((team_basket == "right" & possession_x_location > 550) | (team_basket == "left" & possession_x_location < 450)) %>%
    filter(possession_score <= 3)

plot_missing(plays2)

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