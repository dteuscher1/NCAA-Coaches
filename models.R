## Author: David Teuscher
## Last Edited: 04.12.22
## This script provides the code for the Shiny app presenting the results
## of this analysis
##############################################################################

# Load packages
library(DataExplorer)
library(tidyverse)
library(tidymodels)

# Load in data
plays <- read.csv("model_data_v2.csv") 

# Check missing values
plot_missing(plays)

# Remove missing value and columns not needed 
plays <- plays %>%
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(time)) %>%
    filter(!is.na(game_coach)) %>%
    filter(!is.na(season))  %>% 
    filter((team_basket == "right" & possession_x_location > 550) | (team_basket == "left" & possession_x_location < 450)) %>%
    filter(possession_score <= 3)

# Check missing values again
plot_missing(plays)

# Set seed
set.seed(222)
# Take small sample of plays to train the model
plays_small <- plays %>% sample_n(15000)

# Put 3/4 of the data into the training set 
data_split <- initial_split(plays_small, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create a recipe for the model
plays_rec <- recipe(possession_score ~ possession_x_location + possession_y_location + score_diff + time + team_basket, data = train_data) %>%
    step_dummy(team_basket)

# Determine the number of cores
cores <- parallel::detectCores() - 1

# Set random forest model to be tuned
tune_spec <- rand_forest(mtry = tune(),
                         trees = 1000,
                         min_n = tune()) %>%
    set_mode("regression") %>%
    set_engine("ranger", num.threads = cores)

# Set xgboost model to be tuned
xgb_model <- boost_tree(trees = 1000,
                        tree_depth = tune(),
                        min_n = tune(), 
                        loss_reduction = tune(),                     ## first three: model complexity
                        sample_size = tune(), 
                        mtry = tune(),         ## randomness
                        learn_rate = tune()) %>%
    set_mode("regression") %>%
    set_engine("xgboost")

# Set xgboost grid
xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_data),
    learn_rate(),
    size = 20
)

# Set xgboost workflow
xgb_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(xgb_model)

# Set random forest workflow
plays_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(tune_spec)

# Set cross validation folds
folds <- vfold_cv(train_data, v = 5)

# Tune the random forest
plays_tune <- plays_wflow %>% 
    tune_grid(resamples = folds,
         grid = 25) 

# Use parallel to tune xgboost
doParallel::registerDoParallel()

# Tune xgboost model
xgb_tune <- xgb_wflow %>%
    tune_grid(resamples = folds, 
              grid = xgb_grid)

# Collect RMSE for random forest
plays_tune %>% collect_metrics(metric = "rmse")

# Collect RMSE for xgboost
xgb_tune %>% collect_metrics(metric = "rmse")

# Get best tuning parameters for random forest
plays_tune %>% select_best(metric = "rmse")

# Get best tuning parameters for xgboost
xgb_tune %>% select_best(metric = "rmse")

# Split data with all data
data_split <- initial_split(plays, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create random forest with tuned parameters
plays_model <- rand_forest(mtry = 2,
                           trees = 1000,
                           min_n = 30) %>%
    set_mode("regression") %>%
    set_engine("ranger", num.threads = cores)

# Create xgboost with tuned parameters
xgb_model <- boost_tree(trees = 1000,
                        tree_depth = 8,
                        min_n = 6, 
                        loss_reduction = .518,                     ## first three: model complexity
                        sample_size = .9, 
                        mtry = 3,         ## randomness
                        learn_rate = .00821) %>%
    set_mode("regression") %>%
    set_engine("xgboost")

# Workflow for random forest
plays_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(plays_model)

# Workflow for xgboost
xgb_wflow <- workflow() %>%
    add_recipe(plays_rec) %>%
    add_model(xgb_model)

# Cross validation for random forest
plays_fit_rs <- plays_wflow %>% 
    fit_resamples(folds)

# Cross validation for xgboost
xgb_fit_rs <- xgb_wflow %>%
    fit_resamples(folds)

# Get metrics for random forest and xgboost
collect_metrics(plays_fit_rs)
collect_metrics(xgb_fit_rs)

# Fit random forest on train data
plays_fit <- plays_wflow %>%
    fit(train_data)

# Fit xgboost on train data
xgb_fit <- xgb_wflow %>%
    fit(train_data)

# Get predictions for both modesl on the test data
plays_testing_pred <- predict(plays_fit, test_data) %>% 
    bind_cols(test_data %>% select(possession_score)) %>%
    bind_cols(predict(xgb_fit, test_data)) %>%
    rename(RF = .pred...1,
           XGB = .pred...3)

# Get RMSE for random forest
plays_testing_pred %>% 
    rmse(possession_score, RF)

# Get RMSE for xgboost
plays_testing_pred %>% 
    rmse(possession_score, XGB)

# Fit random forest and xgboost on all data
plays_fit_all <- plays_wflow %>%
    fit(plays)
xgb_fit_all <- xgb_wflow %>%
    fit(plays)

# Get fitted values for all observations
plays_testing_pred <- predict(plays_fit_all, plays) %>% 
    bind_cols(plays %>% select(possession_score)) %>%
    bind_cols(predict(xgb_fit_all, plays)) %>%
    rename(RF = .pred...1,
           XGB = .pred...3)

# RMSE for random forest
plays_testing_pred %>% 
    rmse(possession_score, RF)

# RMSE for xgboost
plays_testing_pred %>% 
    rmse(possession_score, XGB)

# Create data for Shiny app
plays <- plays %>%
    bind_cols(plays_testing_pred %>% dplyr::select(RF, XGB))

write.csv(plays, "shiny_data.csv")
