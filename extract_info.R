games <- readRDS("pbp_data.RDS")
games[[1]]
library(listviewer)
jsonedit(games[[1]])
events <-games[[1]]$periods[[1]]$events
length(events)
event_types <- c()
timeouts <- c()
timeouts_possessions <- c()
for(i in 1:length(events)){
    event_types <- c(event_types, events[[i]]$event_type)
    if(events[[i]]$event_type %in% c("teamtimeout", "tvtimeout")){
        timeouts <- c(timeouts, i)
        timeouts_possessions <- c(timeouts_possessions, events[[i]]$possession$market)
    }
}

library(lubridate)
unique(event_types)
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
        possession <- events[[event_counter]]$possession$market
        home_score <- events[[event_counter]]$home_points - events[[timeouts[i]]]$home_points
        away_score <- events[[event_counter]]$away_points - events[[timeouts[i]]]$away_points
        points <- max(home_score, away_score)
        event_counter <- event_counter + 1
    }
    possession_x_location[i] <- events[[event_counter - 1]]$location$coord_x
    possession_y_location[i] <- events[[event_counter - 1]]$location$coord_y
    possession_score[i] <- points
    if(half == 1){
        time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock) + minutes(20))
    }
}
test <- data.frame(possession_score, possession_x_location, possession_y_location, score_diff, time, timeouts_possessions)
