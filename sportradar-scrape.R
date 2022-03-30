## Author: David Teuscher
## Last Edited: 03.30.22
## This script scrapes team information, schedule data, and play by play data
## from Sportradar from 2013-2022 for NCAAMB games. It also scrapes coach
## information from the Coaches Database
##############################################################################

# Load needed modules
library(rjson)
library(tidyverse)
library(listviewer)
library(xml2)
library(rvest)
library(glue)
library(progress)

# Url/API to scrape: https://www.coachesdatabase.com/college-basketball-programs/
# https://developer.sportradar.com/docs/read/basketball/NCAA_Mens_Basketball_v7#rankings-by-week

# API Key
trial_key <- insert_key_here

# Scrape Sport Radar for team information and their ids
url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/league/hierarchy.json?api_key={trial_key}")

# Extract json information
league <- url %>%
    rjson::fromJSON(file = .)

# Use the listviewer package to look at the hierarchy of the
# returned list in an interactive viewer and determine where the 
# conference of interest are located in the list
jsonedit(league)

# Conferences:
# Pac-12: 1; Big 12: 4; ACC: 17; SEC: 23; Big 10: 25; WCC 30; Big East: 31;

# Create empty data frame to store team information
team_info <- data.frame()
# List elements for the conferences I want to examine
conf_id <- c(1, 4, 17, 23, 25, 30, 31)
# Loop through each conference and pull information about the teams
for(i in 1:length(conf_id)){
    # Extract the conference and get the conference name and alias
    conf <- league$divisions[[8]]$conferences[[conf_id[i]]]
    conf_name <- conf$name
    conf_alias <- conf$alias
    # Loop through each team in the conference and get the id,
    # name, market, and the team alias and append to the data frame.
    for(j in 1:length(conf$teams)){
        info <- conf$teams[[j]] %>%
            data.frame() %>%
            dplyr::select(id, name, market, alias) %>%
            mutate(conference = conf_name,
                   conf_alias = conf_alias)
        team_info <- team_info %>% bind_rows(info)
    }
}

# Write out the team information to a .csv file
write.csv(team_info, "team_ids.csv")

# Scrape information about the coaches for each team from this site
coaches <- "https://www.coachesdatabase.com/college-basketball-programs/"

# Extract the href for all of the links on the page
coach <-  read_html(coaches) %>%
    html_nodes('ul') %>%
    html_nodes('a') %>%
    html_attr('href')

# Combine the name and market to get the full team name and remove any
# punctuation or (fl) (for Miami Hurricanes) and rename entry for NC State
team_name <- tolower(paste(team_info$market, team_info$name)) %>%
    str_remove_all("[\\.']") %>%
    str_remove_all("&") %>%
    str_remove_all(" \\(fl\\)") %>%
    str_replace("north carolina state", "nc state") %>%
    str_replace_all(" ", "-")

# Extract the href for each team by detecting the team name in the href string
team_href <- sapply(team_name, FUN = function(x) coach[str_detect(coach, x)], 
                    simplify = FALSE, USE.NAMES = FALSE) %>% unlist()

# Create an empty list to store coach information
team_coach_df <- list()

# Open progress bar to see scraping progress
pb <- txtProgressBar(min = 0, max = length(team_name), style = 3)

# Loop through the teams and scrape the table with information about all of the
# coaches for a team.
for(i in 1:length(team_href)){
    # Extract the table with the coaches for a team over their history
    coach_page <- read_html(team_href[i]) %>%
        html_table(header = TRUE) %>%
        data.frame()
    team_coach_df[[i]] <- coach_page
    # Print out the current team being scraped and increase progress bar
    cat("\n Retrieving data for", team_name[i], '\n')
    setTxtProgressBar(pb, i)
}
close(pb)

# Save the list of the coach data frames to a .RDS file
saveRDS(team_coach_df, "coaches_data.RDS")

# Get schedule for the years
library(glue)
years <- 2013:2021
# Types of games
# CT: Conference tournament
# REG: Regular season
# PST: Postseason
season <- c("CT", "REG", "PST")
# Loop through all the years
for(i in years){
    # Loop through each of the type of games
    for(j in season){
        # Add in sleep time for scraping
        Sys.sleep(1)
        # Create string of the url
        url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/games/{i}/{j}/schedule.json?api_key={trial_key}")
        # Pull the info into a list
        schedule <- url %>%
            rjson::fromJSON(file = .)
        # Check to make sure there are games
        if(length(schedule$games) > 0){
            # Start a progress bar
            pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                                   total = length(schedule$games))
            # Initialize the progress bar
            pb$tick(0)
            # Loop through each game in the schedule
            for(k in 1:length(schedule$games)){
                # Skip over any cancelled games
                if(schedule$games[[k]]$status == "cancelled"){
                    next
                }
                # Extract the game id, home team id, away team id, and date
                game_id <- schedule$games[[k]]$id
                home_id <- schedule$games[[k]]$home$id
                away_id <- schedule$games[[k]]$away$id
                date <- schedule$games[[k]]$scheduled
                # Combine the variables into a single data frame and bind to the master data frame
                game <- data.frame(game_id = game_id, home_id = home_id,
                                   away_id = away_id, date = date, game_type = j)
                game_info <- game_info %>% bind_rows(game)
                # Move the progress bar forward
                pb$tick()
                Sys.sleep(1/100)
            }
            # Move to the next one if there are no games
        }else{
            next
        }
    }
}
# Write the game information to a csv file
write.csv(game_info, "games.csv")

# Read in the game info 
game_info <- read.csv("games.csv")

# Combine the schedule information with the team information
game_team_info <- game_info %>% left_join(team_info, by = c('home_id' = 'id')) %>%
    left_join(team_info, by = c('away_id' = 'id'), suffix = c(".home", ".away")) %>%
    filter(!is.na(name.home) & !is.na(name.away))

# Extract the game ids for all games
pbp_game_id <- game_team_info$game_id

# Read in data containing information about API keys
keys_info <- readxl::read_excel("../API-keys.xlsx")
# Create empty list to store lists with play by play data
key_test <- list()
# Initialize a progress par
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                       total = length(pbp_game_id))
# Loop through the different API keys
for(j in 1:10){
    # Specify the key and counter
    trial_key <- keys_info$Trial_Key[j]
    counter <- counter + 1
    # Pull 1000 games with each key
    for(i in 1:1000){
        counter2 <- counter * 1000
        # Check to make sure there are still games to pull
        if(counter2 <= length(pbp_game_id)){
            # Get the game ID
            game_id <- pbp_game_id[counter2]
            # Try to pull play by play data for the game; return NA if warnings or errors occur
            game_data <- tryCatch(
                expr = {
                    url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/games/{game_id}/pbp.json?api_key={trial_key}")
                    data <- url %>%
                        rjson::fromJSON(file = .)},
                error = function(cond){NA},
                warning = function(cond){NA})
            # Put play by play data into the list
            key_test[[counter2]] <- game_data
            pb$tick()
            Sys.sleep(1/10)
        } else{
            next
            pb$tick()
        }
        
    }
}    

# Save the pbp data to a RDS file
saveRDS(key_test, "pbp_last.RDS")

