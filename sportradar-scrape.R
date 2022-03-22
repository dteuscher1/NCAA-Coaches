library(rjson)
library(tidyverse)
library(listviewer)
library(xml2)
library(rvest)

# Url/API to scrape: https://www.coachesdatabase.com/college-basketball-programs/
# https://developer.sportradar.com/docs/read/basketball/NCAA_Mens_Basketball_v7#rankings-by-week

# API Key
trial_key <- "57yjy9e3bg2psxjqhs7qznef"

# Scrape Sport Radar for team information and their ids
url <- "https://api.sportradar.us/ncaamb/trial/v7/en/league/hierarchy.json?api_key=57yjy9e3bg2psxjqhs7qznef"

# Extract json information
league <- url %>%
    rjson::fromJSON(file = .)

# Use the listviewer package to look at the hierarchy of the
# returned list in an interactive viewer
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
team_href <- sapply(team_name, FUN = function(x) coach[str_detect(coach, x)], simplify = FALSE, USE.NAMES = FALSE) %>% unlist()

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

saveRDS(team_coach_df, "coaches_data.RDS")
head(team_coach_df)
team_coaches <- data.frame()
counter <- 0
for(i in 1:length(team_coach_df)){
    frame <- team_coach_df[[i]] %>% separate(Tenure, into = c("Start", "End"), sep = "-") %>%
        mutate(Team = team_info$market[i],
               TeamID = team_info$id[i])
    team_coach_df[[i]] <- frame
    #team_coaches <- team_coaches %>% bind_rows(frame)
    #counter <- counter + 1
}
team_coach_df[[12]]
# Get schedule for the years
library(glue)
years <- 2019:2021
season <- c("CT", "REG", "PST")
#game_info <- c()
for(i in years){
    for(j in season){
        Sys.sleep(1)
        url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/games/{i}/{j}/schedule.json?api_key={trial_key}")
        schedule <- url %>%
            rjson::fromJSON(file = .)
        if(length(schedule$games) > 0){
            pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                                   total = length(schedule$games))
            pb$tick(0)
            for(k in 1:length(schedule$games)){
                if(schedule$games[[k]]$status == "cancelled"){
                    next
                }
                game_id <- schedule$games[[k]]$id
                home_id <- schedule$games[[k]]$home$id
                away_id <- schedule$games[[k]]$away$id
                date <- schedule$games[[k]]$scheduled
                game <- data.frame(game_id = game_id, home_id = home_id,
                                   away_id = away_id, date = date, game_type = j)
                game_info <- game_info %>% bind_rows(game)
                pb$tick()
                Sys.sleep(1/100)
            }
        }else{
            next
        }
    }
}

write.csv(game_info, "games.csv")

game_info <- read.csv("games.csv")


attempt <- game_info %>% left_join(team_info, by = c('home_id' = 'id')) %>%
    left_join(team_info, by = c('away_id' = 'id'), suffix = c(".home", ".away")) %>%
    filter(!is.na(name.home) & !is.na(name.away))
head(attempt)
byu <- attempt %>% filter(alias.home == "BYU" | alias.away == "BYU")
unique(attempt$game_id)
team_info %>% filter(market %in% c("Louisville", "Michigan"))

pbp_game_id <- attempt$game_id
library(glue)
url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/games/{test_game_id}/pbp.json?api_key={trial_key}")
single_game <- url %>%
    rjson::fromJSON(file = .)

jsonedit(single_game)


library(progress)
keys_info <- readxl::read_excel("../API-keys.xlsx")
trial_key <- keys_info$Trial_Key[17]
key_test <- list()
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                       total = length(8992:9462))
trial_key <- "9xtb9s2ecm9prt44zryhk99u"
for(i in 8992:9462){
    game_id <- pbp_game_id[i]
    game_data <- tryCatch(
        expr = {
            url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/games/{game_id}/pbp.json?api_key={trial_key}")
            data <- url %>%
                rjson::fromJSON(file = .)},
        error = function(cond){NA},
        warning = function(cond){NA})
    key_test[[i-8991]] <- game_data
    pb$tick()
    Sys.sleep(1/10)
}
saveRDS(key_test, "pbp_last.RDS")
# Loop through each single game
# Loop through each half (and overtime for necessary games)
# For each play, extract play number, possession id, team with possession, clock, location_x, location_y,
# event_type, home points, away points, team_basket
library(progress)
keys_info <- readxl::read_excel("../API-keys.xlsx")
key_test <- list()
counter <- 0
counter2 <- 0
pbp_game_id <- attempt$game_id
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                       total = nrow(keys_info)*1000)
for(i in 1:nrow(keys_info)){
    counter <- counter + 1
    trial_key <- keys_info$Trial_Key[i]
    for(j in 1:1000){
        counter2 <- counter2 + 1
        if(counter2 < length(pbp_game_id)){
            game_id <- pbp_game_id[counter2]
            game_data <- tryCatch(
                expr = {
                    url <- glue("https://api.sportradar.us/ncaamb/trial/v7/en/games/{game_id}/pbp.json?api_key={trial_key}")
                    data <- url %>%
                        rjson::fromJSON(file = .)},
                error = function(cond){NA},
                warning = function(cond){NA})
            if(length(game_data) > 1){
                key_test[[counter2]] <- game_data
            }
            pb$tick()
            Sys.sleep(1)
        }
        else {
            next
        }
    }
}

saveRDS(key_test, "pbp_data.RDS")
