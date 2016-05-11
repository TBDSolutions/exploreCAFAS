
#### Aggregate data to allow presentation in open app

library(dplyr)
library(tidyr)
library(car)

# Define dataset without PHI to use with Shiny Apps 

  # Make an ID key
    id <- unique(sub_cafas$id)
    cafas_key <- data.frame(id)
    cafas_key$fake_id <- sample(x = 100000001:999999999, 
                              size = length(cafas_key$id), 
                              replace = FALSE)
    cafas_key$id <- as.character(cafas_key$id)
    cafas_key$fake_id <- as.character(cafas_key$fake_id)
    rm(id)

  # Make PHI-free dataset
    scrub_cafas <-
      sub_cafas %>%
      mutate(id = as.character(id)) %>%
      left_join(cafas_key, by = "id") %>%
      select(-id, -assess_age, -gender)
    
    write.csv(scrub_cafas,"data/scrub_cafas.csv", row.names = F)
