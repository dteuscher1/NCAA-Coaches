## Author: David Teuscher
## Last Edited: 04.12.22
## This script takes the coach and play by play data and get the coach
## for a team during the game. It also adds in a season variable
##############################################################################

# Load packages
library(tidyverse)

# Load plays dataset and coaches data
plays <- read.csv("ato_v2.csv")
coaches <- read.csv("coaches_data.csv")

# Change the date to a date type and then create a season variable
plays <- plays %>% 
    mutate(game_date = as.Date(game_date), 
           timeouts_possessions = str_replace(timeouts_possessions, "BYU", "Brigham Young"), 
           season = ifelse(between(game_date, as.Date("2013-11-08"), as.Date("2014-04-07")), 2013,
                           ifelse(between(game_date, as.Date("2014-11-14"), as.Date("2015-04-06")), 2014,
                                  ifelse(between(game_date, as.Date("2015-11-13"), as.Date("2016-04-04")), 2015,
                                         ifelse(between(game_date, as.Date("2016-11-11"), as.Date("2017-04-03")), 2016,
                                                ifelse(between(game_date, as.Date("2017-11-10"), as.Date("2018-04-02")), 2017,
                                                       ifelse(between(game_date, as.Date("2018-11-06"), as.Date("2019-04-08")), 2018,
                                                              ifelse(between(game_date, as.Date("2019-11-05"), as.Date("2020-04-06")), 2019,
                                                                     ifelse(between(game_date, as.Date("2020-11-25"), as.Date("2021-04-05")), 2020,
                                                                            ifelse(between(game_date, as.Date("2021-11-09"), as.Date("2022-04-04")), 2021, NA))))))))))

# Extract the coach of a team for each game
game_coach <- c()
for(i in 1:nrow(plays)){
    coach <- coaches[which(coaches$team_market %in% plays$timeouts_possessions[i]), ]
    for(j in 1:nrow(coach)){
        if(plays$game_date[i] > coach$Start_Date[j] & plays$game_date[i] < coach$End_Date[j]){
            game_coach[i] <- coach$Coach[j]
        }
    }
}

# Bind the team coach to the dataset
model_data <- plays %>% 
    mutate(game_coach = game_coach)

# Write the data to a .csv file
write.csv(model_data, "model_data_v2.csv", row.names = FALSE)
