coaches <- readRDS("coaches_data.RDS")
library(tidyverse)
one_frame <- coaches[[2]] %>% separate(Tenure, c("Start", "End"), sep = "-")
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
