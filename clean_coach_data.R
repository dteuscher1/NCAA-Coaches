coaches <- readRDS("coaches_data.RDS")
library(tidyverse)
one_frame <- coaches[[2]] %>% separate(Tenure, c("Start", "End"), sep = "-") 
coach_df <- data.frame()
for(coach in 1:length(coaches)){
    one_frame <- coaches[[coach]] %>% separate(Tenure, c("Start", "End"), sep = "-") %>%
        mutate(End = as.numeric(ifelse(End == "Pres", 2022, End))) %>% 
        dplyr::select(Coach, Start, End)
    rows <- c()
    for(i in 1:nrow(one_frame)){
        if(as.numeric(one_frame$Start[i]) > 2013){
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
    print(coach)
}

