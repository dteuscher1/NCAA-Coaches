library(tidyverse)
plays <- read.csv("model_data.csv") %>%
    dplyr::select(-X, -game_number, -possession_ids, -half) %>%
    mutate(team_basket = factor(team_basket)) %>%
    filter(!is.na(possession_x_location)) %>%
    filter(!is.na(time)) %>%
    filter(!is.na(game_coach))
team_info <- read.csv("team_ids.csv")
rf_preds <- readRDS("rf_preds.RDS")
attempt <- plays %>% 
    mutate(pred = rf_preds) %>% 
    group_by(timeouts_possessions) %>%
    summarize(expected = round(mean(pred), digits = 3),
              actual = round(mean(possession_score), digits = 3), 
              diff = round(actual - expected, digits = 3), 
              n = n()) %>%
    arrange(desc(diff)) %>% 
    mutate(timeouts_possessions = ifelse(timeouts_possessions == "Brigham Young", "BYU", timeouts_possessions))

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
names(attempt2) <- c("Team", "Team Name", "Logo", "Expected Points", "Actual Points", "Difference", "Number of Plays", "Conference", "Conference Logo")
# ggplot(attempt2, aes(x = expected, y = actual)) +
#     geom_median_lines(aes(v_var = expected, h_var = actual)) +
#     geom_cfb_logos(aes(team = timeouts_possessions), width = 0.075) +
#     labs(y = "Actual Points per Possession",x = "Expected Points Per Possession") +
#     theme_bw()

# library(gt)
# attempt2 %>% gt() %>% gt_fmt_cfb_logo(columns = c(`Conference Logo`, `Logo`)) %>%
#     gt_merge_stack_team_color(Team,`Team Name`,Team)
    
