games <- readRDS("pbp_data.RDS")
games[[1]]
library(listviewer)
jsonedit(games[[135]])
events <-games[[1]]$periods[[1]]$events
length(events)
library(lubridate)
library(tidyverse)
test <- data.frame()
for(game in 289:length(games)){
    if(length(games[[game]]) == 1){
        next
    }
    for(half in 1:length(games[[game]]$periods)){
        events <- games[[game]]$periods[[half]]$events
        event_types <- c()
        timeouts <- c()
        timeouts_possessions <- c()
        if (length(events) == 0){
            next
        }
        
        for(i in 1:length(events)){
            event_types <- c(event_types, events[[i]]$event_type)
            if(events[[i]]$event_type %in% c("teamtimeout", "tvtimeout")){
                timeouts <- c(timeouts, i)
                team_possession <- events[[i]]$possession$market
                if(is.null(team_possession)){
                    counter <- i-1
                    while(is.null(team_possession)){
                        team_possession <- events[[counter]]$possession$market
                        counter <- counter - 1
                    }
                }
                timeouts_possessions <- c(timeouts_possessions, team_possession)
            }
        }
        
        possession_score <- numeric()
        possession_x_location <- numeric()
        possession_y_location <- numeric()
        score_diff <- numeric()
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
            if(half == 1){
                time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock) + minutes(20))
            }
            time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock))
        }
        test.half <- data.frame(possession_score, possession_x_location, possession_y_location, score_diff, time, timeouts_possessions)
        test <- test %>% bind_rows(test.half)
    }
}    

