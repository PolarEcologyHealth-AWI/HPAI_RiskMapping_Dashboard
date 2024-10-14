library(tidyverse)

load("~/Downloads/WOAH_PI_Rdata")

Is <- PI %>% dplyr::select(c("Dead","Species" ,"is_wild","Date","Longitude","Latitude","sero_sub_genotype_eng")) %>%
  filter(complete.cases(.)) %>%
  mutate(H5 = grepl("H5", sero_sub_genotype_eng),
         markerCol = ifelse(is_wild,"chocolate","blue"))

save(Is, file = "data/HPAIoutbreak.rda")
