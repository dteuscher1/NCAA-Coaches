coaches <- readRDS("coaches_data.RDS")
library(tidyverse)
team_info <- read.csv("team_ids.csv")
one_frame <- coaches[[2]] %>% separate(Tenure, c("Start", "End"), sep = "-") 
coach_df <- data.frame()
for(coach in 1:length(coaches)){
    if(coach == 85){
        one_frame <- coaches[[coach]] %>% separate(Tenure, c("Start", "End"), sep = "-")
        one_frame$Start[2] <- 2015
        one_frame$End[2] <- 2021
        one_frame <- one_frame %>% 
            mutate(End = as.numeric(ifelse(End == "Pres", 2022, End)),
                   team_name = team_info$name[coach],
                   team_market = team_info$market[coach],
                   team_id = team_info$id[coach],
                   conference = team_info$conf_alias[coach]) %>% 
            dplyr::select(Coach, Start, End, team_name, team_market, team_id, conference)
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
        one_frame <- coaches[[coach]] %>% separate(Tenure, c("Start", "End"), sep = "-") %>%
            mutate(End = as.numeric(ifelse(End == "Pres", 2022, End)),
                   team_name = team_info$name[coach],
                   team_market = team_info$market[coach],
                   team_id = team_info$id[coach],
                   conference = team_info$conf_alias[coach]) %>% 
            dplyr::select(Coach, Start, End, team_name, team_market, team_id, conference)
    }    
    rows <- c()
    for(i in 1:nrow(one_frame)){
        if(as.numeric(one_frame$Start[i]) >= 2013){
            rows <- c(rows, i)
        }
        if(i == 1){
            if(as.numeric(one_frame$Start[i]) < 2013){
                rows <- c(rows, i)
                break
            }
        }
        if(i != 1){
            if(as.numeric(one_frame$Start[i - 1]) > 2013 & as.numeric(one_frame$Start[i]) <= 2013){
                rows <- c(rows, i)
                break
            }
        }
        
    }
    coach_df <- coach_df %>% bind_rows(one_frame[rows, ])
}

seasons <- read.csv("season_dates.csv")

clean_coach_df <- coach_df %>%
    mutate(End = ifelse(is.na(End), Start, End),
           End = ifelse(nchar(End) == 2, paste0(20, End), End),
           Start = as.numeric(Start), 
           End = as.numeric(End), 
           End_Merge = End - 1) %>%
    filter(Coach != "vacant") %>% 
    inner_join(seasons %>% dplyr::select(Season, Start_Date), by = c('Start' = 'Season')) %>%
    inner_join(seasons %>% dplyr::select(Season, End_Date), by = c('End_Merge' = 'Season')) %>%
    dplyr::select(-End_Merge) %>%
    mutate(Coach = str_replace(Coach, "\\*\\*", ""), 
           Coach = str_trim(str_replace(Coach, "\\([A-Za-z.]+\\)", "")),
           team_market = str_replace(team_market, "BYU", "Brigham Young")) %>%
    filter(Coach != "Tony Benford") %>%
    filter(Coach != "Bob Cantu")
    
View(clean_coach_df)
# remove benford
interim <- read.csv("interim_coaches.csv")

for(i in 1:nrow(interim)){
    counter <- 1
    while((clean_coach_df$Coach[counter] != interim$Coach[i]) | (clean_coach_df$team_market[counter] != interim$Team[i])){
        counter <- counter + 1
    }
    clean_coach_df$Start_Date[counter] <- interim$Actual_Start[i]
    clean_coach_df$End_Date[counter] <- interim$Actual_End[i]
    
}

write.csv(clean_coach_df, "coaches_data.csv")
