plays <- read.csv("ato.csv")
coaches <- read.csv("coaches_data.csv")
library(tidyverse)
glimpse(plays)
plays <- plays %>% 
    mutate(game_date = as.Date(game_date), 
           timeouts_possessions = str_replace(timeouts_possessions, "BYU", "Brigham Young"))

game_coach <- c()
for(i in 1:nrow(plays)){
    coach <- coaches[which(coaches$team_market %in% plays$timeouts_possessions[i]), ]
    for(j in 1:nrow(coach)){
        if(plays$game_date[i] > coach$Start_Date[j] & plays$game_date[i] < coach$End_Date[j]){
            game_coach[i] <- coach$Coach[j]
        }
    }
}
unique(plays$timeouts_possessions) %>% sort()


model_data <- plays %>% 
    mutate(game_coach = game_coach)
write.csv(model_data, "model_data.csv", row.names = FALSE)
