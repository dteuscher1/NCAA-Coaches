## Author: David Teuscher
## Last Edited: 04.12.22
## This script manipulates the data as needed to prepare the data for the Shiny app
##############################################################################

# Read in the play data
plays <- read.csv("model_data.csv") %>%
    # Remove columns that are not needed
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    # Make the team basket into a factor
    mutate(team_basket = factor(team_basket)) %>%
    # Remove NA location, time, or coach observations
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(time)) %>%
    filter(!is.na(game_coach))

# Read in team info and model predictions
team_info <- read.csv("team_ids.csv")
rf_preds <- readRDS("rf_preds.RDS")

# Group the plays by the team and calculate the actual and expected points
# per possession and the number of plays out of a timeout
attempt <- plays %>% 
    mutate(pred = rf_preds) %>% 
    group_by(timeouts_possessions) %>%
    summarize(expected = round(mean(pred), digits = 3),
              actual = round(mean(possession_score), digits = 3), 
              diff = round(actual - expected, digits = 3), 
              n = n()) %>%
    arrange(desc(diff)) %>% 
    mutate(timeouts_possessions = ifelse(timeouts_possessions == "Brigham Young", "BYU", timeouts_possessions))

# Join the data with the team data and change some teams and conferences to match the 
# strings in the cfbplotR package
attempt2 <- attempt %>% inner_join(team_info, by = c("timeouts_possessions" = "market")) %>%
    mutate(timeouts_possessions = ifelse(timeouts_possessions == "North Carolina State", "NC State", timeouts_possessions),
           timeouts_possessions = ifelse(timeouts_possessions == "Miami (FL)", "Miami", timeouts_possessions)) %>%
    dplyr::select(-X, -id, -alias, -conf_alias) %>% 
    mutate(Conference_logo = ifelse(conference == "West Coast", "NCAA", conference),
           Conference_logo = ifelse(Conference_logo == "Big East", "NCAA", Conference_logo),
           Conference_logo = ifelse(Conference_logo == "Southeastern", "SEC", Conference_logo),
           Conference_logo = ifelse(Conference_logo == "Atlantic Coast", "ACC", Conference_logo),
           Conference_logo = ifelse(Conference_logo == "Pacific 12", "Pac-12", Conference_logo),
           Team_logo = timeouts_possessions) %>%
    dplyr::select(timeouts_possessions, name, Team_logo, expected, actual, diff, n, conference, Conference_logo)

# Rename the columns of the data frame
names(attempt2) <- c("Team", "Team Name", "Logo", "Expected Points", "Actual Points", "Difference", "Number of Plays", "Conference", "Conference Logo")

    
