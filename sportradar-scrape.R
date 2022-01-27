library(rjson)

trial_key <- "57yjy9e3bg2psxjqhs7qznef"

sport_radar_url <- "https://api.sportradar.us/ncaamb/trial/v7/en/games/2022/1/15/schedule.json?api_key=57yjy9e3bg2psxjqhs7qznef"

ncaa_m <- sport_radar_url %>%
    rjson::fromJSON(file = .)
test_obs <- ncaa_m$games[[1]] %>% data.frame()

# glue package

test_obs

url <- "https://api.sportradar.us/ncaamb/trial/v7/en/league/hierarchy.json?api_key=57yjy9e3bg2psxjqhs7qznef"
league <- url %>%
    rjson::fromJSON(file = .)

library(listviewer)
jsonedit(league)

# Conferences: Pac-12: 1; Big 12: 4; ACC: 17; SEC: 23; Big 10: 25; WCC 30; Big East: 31;
team_info <- data.frame()
conf_id <- c(1, 4, 17, 23, 25, 30, 31)
for(i in 1:length(conferences)){
    conf <- league$divisions[[8]]$conferences[[conf_id[i]]]
    conf_name <- conf$name
    conf_alias <- conf$alias
    for(j in 1:length(conf$teams)){
        info <- conf$teams[[j]] %>% 
            data.frame() %>%
            dplyr::select(id, name, market, alias) %>%
            mutate(conference = conf_name, 
                   conf_alias = conf_alias)
        team_info <- team_info %>% bind_rows(info)
    }
}
