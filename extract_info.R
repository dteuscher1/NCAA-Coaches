games <- readRDS("pbp8.RDS")

# 5007, 6493, 8504
library(listviewer)
library(progress)
jsonedit(games[[1]])
library(lubridate)
library(tidyverse)
test <- data.frame()
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                       total = length(games))
for(game in 1:length(games)){
    if(length(games[[game]]) == 1){
        next
    }
    if(games[[game]]$status %in% c("postponed", "cancelled")){
        next
    }
    if(length(games[[game]]$periods[[1]]$events) < 1){
        next
    }
    if(game %in% c(5007, 6493, 8504)){
        next
    }
    for(half in 1:length(games[[game]]$periods)){
        events <- games[[game]]$periods[[half]]$events
        event_types <- c()
        timeouts <- c()
        timeouts_possessions <- c()
        possession_ids <- c()
        home_team <- games[[game]]$home$market
        away_team <- games[[game]]$away$market
        home_basket <- c()
        away_basket <- c()
        if(events[[1]]$attribution$market == home_team){
            home_basket <- events[[1]]$attribution$team_basket
        } else {
            away_basket <- events[[1]]$attribution$team_basket
        }
        if(is.null(home_basket)){
            if(away_basket == "right"){
                home_basket <- "left"
            }
            else {
                home_basket <- "right"
            }
        }else {
            if(home_basket == "right"){
                away_basket <- "left"
            } 
            else {
                away_basket <- "right"
            }
        }
        for(i in 1:length(events)){
            event_types <- c(event_types, events[[i]]$event_type)
            if(events[[i]]$event_type %in% c("teamtimeout", "tvtimeout", "officialtimeout")){
                if(!is.null(events[[i]]$possession$market)){
                    timeouts <- c(timeouts, i)
                    team_possession <- events[[i]]$possession$market
                    timeouts_possessions <- c(timeouts_possessions, team_possession)
                    possession_ids <- c(possession_ids, events[[i]]$possession$id)
                }
                else{
                    next
                }
            }
        }
        
        if(length(timeouts) > 0){
            possession_score <- numeric()
            possession_x_location <- numeric()
            possession_y_location <- numeric()
            score_diff <- numeric()
            team_basket <- c()
            time <- c()
            for(i in 1:length(timeouts)){
                event_counter <- timeouts[i]
                possession <- timeouts_possessions[i]
                score_diff[i] <- events[[event_counter]]$home_points - events[[event_counter]]$away_points
                while(possession == timeouts_possessions[i]){
                    event <- events[[event_counter]]$event_type
                    if(is.null(events[[event_counter]]$possession$market)){
                        possession <- possession
                    } else{
                        possession <- events[[event_counter]]$possession$market
                    }
                    home_score <- events[[event_counter]]$home_points - events[[timeouts[i]]]$home_points
                    away_score <- events[[event_counter]]$away_points - events[[timeouts[i]]]$away_points
                    points <- max(home_score, away_score)
                    event_counter <- event_counter + 1
                    if(event_counter > length(events)){
                        break
                    }
                }
                if(is.null(events[[event_counter - 1]]$location$coord_x)){
                    possession_x_location[i] <- NA
                    possession_y_location[i] <- NA
                } 
                else{
                    possession_x_location[i] <- events[[event_counter - 1]]$location$coord_x
                    possession_y_location[i] <- events[[event_counter - 1]]$location$coord_y   
                }
                possession_score[i] <- points
                if(timeouts_possessions[i] == home_team){
                    team_basket[i] <- home_basket
                } else {
                    team_basket[i] <- away_basket
                }
                if(half == 1){
                    time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock) + minutes(20))
                }
                time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock))
            }
            game_date <- rep(games[[game]]$scheduled, length(timeouts_possessions))
            test.half <- data.frame(possession_score, possession_x_location, possession_y_location, 
                                    score_diff, time, timeouts_possessions, possession_ids, game_date,
                                    team_basket, half = rep(half, length(timeouts_possessions)), 
                                    game_number = rep(game, length(timeouts_possessions)))
            test <- test %>% bind_rows(test.half)
        }
        else{
            next
        }
    }
    pb$tick()
}    


write.csv(test, "ato.csv")
