## Author: David Teuscher
## Last Edited: 04.12.22
## This script takes the coach information for all the selected teams
## and extracts the coaches who coached from 2013-2022. The start and end dates
## for the coaches are included as well. 
##############################################################################

# Load packages
library(tidyverse)

# Read in coaches data and team information
coaches <- readRDS("coaches_data.RDS")
team_info <- read.csv("team_ids.csv")

# Create empty data frame to store coach information
coach_df <- data.frame()
# Loop through the list for each coach
for(coach in 1:length(coaches)){
    # This coach had two stints at the school with only one in the time period
    # so I changed the start and end dates
    if(coach == 85){
        # Separate the date column into start and end
        one_frame <- coaches[[coach]] %>% separate(Tenure, c("Start", "End"), sep = "-")
        one_frame$Start[2] <- 2015
        one_frame$End[2] <- 2021
        # Add in the team name, market, id, and conference
        one_frame <- one_frame %>% 
            mutate(End = as.numeric(ifelse(End == "Pres", 2022, End)),
                   team_name = team_info$name[coach],
                   team_market = team_info$market[coach],
                   team_id = team_info$id[coach],
                   conference = team_info$conf_alias[coach]) %>% 
            dplyr::select(Coach, Start, End, team_name, team_market, team_id, conference)
        # This coach also had two tenures at this school
    } else if(coach == 66){
        one_frame <- coaches[[coach]] %>% separate(Tenure, c("Start", "End"), sep = "-")
        one_frame$Start[1] <- 2018
        one_frame$End[1] <- "Pres"
        one_frame$Start[2] <- 2011
        one_frame$End[2] <- 2018
        one_frame <- one_frame %>% 
            mutate(End = as.numeric(ifelse(End == "Pres", 2022, End)),
                   team_name = team_info$name[coach],
                   team_market = team_info$market[coach],
                   team_id = team_info$id[coach],
                   conference = team_info$conf_alias[coach]) %>% 
            dplyr::select(Coach, Start, End, team_name, team_market, team_id, conference)
    } else {
        # Split the date column into two columns, start and end
        one_frame <- coaches[[coach]] %>% separate(Tenure, c("Start", "End"), sep = "-") %>%
            # Change end date to 2022 instead of Present
            mutate(End = as.numeric(ifelse(End == "Pres", 2022, End)),
                   team_name = team_info$name[coach],
                   team_market = team_info$market[coach],
                   team_id = team_info$id[coach],
                   conference = team_info$conf_alias[coach]) %>% 
            dplyr::select(Coach, Start, End, team_name, team_market, team_id, conference)
    }
    # Create an empty vector
    rows <- c()
    # Loop through each row in the data frame for the school's coaches
    for(i in 1:nrow(one_frame)){
        # If the start date is after 2013, get the index
        if(as.numeric(one_frame$Start[i]) >= 2013){
            rows <- c(rows, i)
        }
        # If the first row start date is before 2013, take that row and break the loop
        if(i == 1){
            if(as.numeric(one_frame$Start[i]) < 2013){
                rows <- c(rows, i)
                break
            }
        }
        # If the next coach start data is before 2013 and the current (iteration wise) coach start date is before 2013,
        # extract the coach and break the loop
        if(i != 1){
            if(as.numeric(one_frame$Start[i - 1]) > 2013 & as.numeric(one_frame$Start[i]) <= 2013){
                rows <- c(rows, i)
                break
            }
        }
        
    }
    # Bind the rows of coaches into a combined data frame
    coach_df <- coach_df %>% bind_rows(one_frame[rows, ])
}

# Read in file with season start and end dates
seasons <- read.csv("season_dates.csv")


clean_coach_df <- coach_df %>%
    # Specify end date as the same year as start for interim coaches
    mutate(End = ifelse(is.na(End), Start, End),
           # Add '20' in front of the end year so they are all 4 digit years
           End = ifelse(nchar(End) == 2, paste0(20, End), End),
           # Change start date to a numeric variable
           Start = as.numeric(Start), 
           # Change end date to a numeric variable
           End = as.numeric(End), 
           End_Merge = End - 1) %>%
    # Remove rows where there was no coach name
    filter(Coach != "vacant") %>% 
    # Join on the start date
    inner_join(seasons %>% dplyr::select(Season, Start_Date), by = c('Start' = 'Season')) %>%
    # Join on th end date
    inner_join(seasons %>% dplyr::select(Season, End_Date), by = c('End_Merge' = 'Season')) %>%
    # Drop column used to merge end date
    dplyr::select(-End_Merge) %>%
    # Remove extra punctuation/characters for the coaches name
    mutate(Coach = str_replace(Coach, "\\*\\*", ""), 
           Coach = str_trim(str_replace(Coach, "\\([A-Za-z.]+\\)", "")),
           # Change how BYU is coded
           team_market = str_replace(team_market, "BYU", "Brigham Young")) %>%
    # Remove two interim coaches who had no games
    filter(Coach != "Tony Benford") %>%
    filter(Coach != "Bob Cantu")
    
# Read in data set of interim coaches and coaches fired mid-season
interim <- read.csv("interim_coaches.csv")

# Loop through each coach in the interim and fired mid-season coaches and
# update their start and end dates to be correct 
for(i in 1:nrow(interim)){
    counter <- 1
    while((clean_coach_df$Coach[counter] != interim$Coach[i]) | (clean_coach_df$team_market[counter] != interim$Team[i])){
        counter <- counter + 1
    }
    clean_coach_df$Start_Date[counter] <- interim$Actual_Start[i]
    clean_coach_df$End_Date[counter] <- interim$Actual_End[i]
    
}

# Write the clean coach data to a .csv file
write.csv(clean_coach_df, "coaches_data.csv")
