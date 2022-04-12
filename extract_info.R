## Author: David Teuscher
## Last Edited: 04.12.22
## This script takes the play by play data and extracts the information for
## plays that occur immediately after a timeout and saves the data to 
## a .csv file
##############################################################################

# Load packages
library(listviewer)
library(progress)
library(lubridate)
library(tidyverse)

# Read in the list containing play-by-play data for all games
games <- readRDS("pbp8.RDS")

# 5007, 6493, 8504

# Create data frame to hold the after timeout plays
test <- data.frame()

# Initialize progress bar
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                       total = length(games))
# Loop through each game
for(game in 1:length(games)){
    # If there are no plays for the game, go to the next iteration
    if(length(games[[game]]) == 1){
        next
    }
    # If the game was postponed or cancelled, go to the next iteration
    if(games[[game]]$status %in% c("postponed", "cancelled")){
        next
    }
    # If the plays were not recorded, go to the next event
    if(length(games[[game]]$periods[[1]]$events) < 1){
        next
    }
    # Skip these three games that had some errors in the play by play that
    # causes errors to occur
    if(game %in% c(5007, 6493, 8504)){
        next
    }
    # Loop through each period in the dataset (First, Second, any overtimes)
    for(half in 1:length(games[[game]]$periods)){
        # Extract the play
        events <- games[[game]]$periods[[half]]$events
        # Create a empty vector for the event type and timeouts and who has possession
        # when a timeout occurs
        event_types <- c()
        timeouts <- c()
        timeouts_possessions <- c()
        possession_ids <- c()
        # Extract home and away team for the game and initialize vectors for the basket for each team
        home_team <- games[[game]]$home$market
        away_team <- games[[game]]$away$market
        home_basket <- c()
        away_basket <- c()
        # Use the first possession of the game to determine which basket each team is playing on
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
        # Loop through each play
        for(i in 1:length(events)){
            event_types <- c(event_types, events[[i]]$event_type)
            # If the plays is a timeout, extract information about the plays
            if(events[[i]]$event_type %in% c("teamtimeout", "tvtimeout", "officialtimeout")){
                # As long as the possession is not null, extract when the timeout occurred, 
                # and the team that had possession
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
        
        # If there is more than 0 timeouts, extract information about the next play
        if(length(timeouts) > 0){
            # Initialize vectors to hold the score, location, difference and time
            possession_score <- numeric()
            possession_x_location <- numeric()
            possession_y_location <- numeric()
            score_diff <- numeric()
            team_basket <- c()
            time <- c()
            # Loop through each of the timeouts
            for(i in 1:length(timeouts)){
                # Get the index for the timeout
                event_counter <- timeouts[i]
                # Get the team with possession at the timeout
                possession <- timeouts_possessions[i]
                # Determine the difference in score
                score_diff[i] <- events[[event_counter]]$home_points - events[[event_counter]]$away_points
                # While the possession doesn't change, loop through the plays in the play by play 
                while_counter <- 1
                while(possession == timeouts_possessions[i]){
                    event <- events[[event_counter]]$event_type
                    # If possession isn't recorded, keep the possession on the same team
                    if(is.null(events[[event_counter]]$possession$market)){
                        possession <- possession
                    } else{
                        possession <- events[[event_counter]]$possession$market
                    }
                    # Calcluate the change in home and away score and take the max
                    home_score <- events[[event_counter]]$home_points - events[[timeouts[i]]]$home_points
                    away_score <- events[[event_counter]]$away_points - events[[timeouts[i]]]$away_points
                    points <- max(home_score, away_score)
                    # Increase the counter
                    event_counter <- event_counter + 1
                    # If the counter is greater than the number of plays, end the loop
                    if(event_counter > length(events)){
                        break
                    }
                    while_counter <- while_counter + 1
                    if(while_counter > 15){
                        break
                    }
                }
                if(while_counter > 15){
                    possession_score[i] <- NA
                    possession_x_location[i] <- NA
                    possession_y_location[i] <- NA
                    time[i] <- NA
                    team_basket[i] <- NA
                    next
                }
                # If the location isn't recorded, give an NA or extract the location
                if(is.null(events[[event_counter - 1]]$location$coord_x)){
                    possession_x_location[i] <- NA
                    possession_y_location[i] <- NA
                } 
                else{
                    possession_x_location[i] <- events[[event_counter - 1]]$location$coord_x
                    possession_y_location[i] <- events[[event_counter - 1]]$location$coord_y   
                }
                # Extract the total possession points and the team baskets
                possession_score[i] <- points
                if(timeouts_possessions[i] == home_team){
                    team_basket[i] <- home_basket
                } else {
                    team_basket[i] <- away_basket
                }
                # Extract the time left in the game
                if(half == 1){
                    time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock) + minutes(20))
                } else{
                    time[i] <- as.numeric(ms(events[[event_counter - 1]]$clock))
                }
            }
            # Get the date for the game
            game_date <- rep(games[[game]]$scheduled, length(timeouts_possessions))
            # Create a data frame with all of the game information and bind to full data frame
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
    # Increase progress bar
    pb$tick()
}    

# Write out the data to a .csv file
write.csv(test, "ato_v2.csv")
