
#### Aggregate data to allow presentation in open app

library(dplyr)
library(tidyr)
library(car)

# Define dataset without PHI to use with Shiny Apps 
# Assumes that the episode grouper has already been run

  # Make an ID key
    id <- unique(grp_fas$id)
    fas_key <- data.frame(id)
    fas_key$fake_id <- sample(x = 100000001:999999999, 
                              size = length(fas_key$id), 
                              replace = FALSE)
    fas_key$id <- as.character(fas_key$id)
    fas_key$fake_id <- as.character(fas_key$fake_id)
    rm(id)

  # Make an episode key for original episode groups
    unique_episode_id <- unique(grp_fas$unique_episode_id)
    fas_episode_key <- data.frame(unique_episode_id)
    fas_episode_key$fake_episode_id <- sample(x = 100000001:999999999, 
                                              size = length(fas_episode_key$unique_episode_id), 
                                              replace = FALSE)
    fas_episode_key$unique_episode_id <- as.character(fas_episode_key$unique_episode_id)
    fas_episode_key$fake_episode_id <- as.character(fas_episode_key$fake_episode_id)
    rm(unique_episode_id)  
    
  # Make an episode key for revised episode groups
    rev_episode_id <- unique(grp_fas$rev_episode_id)
    rev_fas_episode_key <- data.frame(rev_episode_id)
    rev_fas_episode_key$rev_fake_episode_id <- sample(x = 100000001:999999999, 
                                                      size = length(rev_fas_episode_key$rev_episode_id), 
                                                      replace = FALSE)
    rev_fas_episode_key$rev_episode_id <- as.character(rev_fas_episode_key$rev_episode_id)
    rev_fas_episode_key$rev_fake_episode_id <- as.character(rev_fas_episode_key$rev_fake_episode_id)
    rm(rev_episode_id)  
    
  # Make PHI-free dataset
    scrub_fas <-
      grp_fas %>%
      mutate(id = as.character(id)) %>%
      left_join(fas_key, by = "id") %>%
      left_join(fas_episode_key, by = "unique_episode_id") %>%
      left_join(rev_fas_episode_key, by = "rev_episode_id") %>%   
      select(-id,-unique_episode_id,-assessmentID,-rev_episode_id,
             -assess_age,-gender,-age_range) %>%
      mutate(fake_id = as.factor(fake_id),
             fake_episode_id = as.factor(fake_episode_id),
             rev_fake_episode_id = as.factor(rev_fake_episode_id)) %>%
      select(fake_id,client_status,cmh,service_area,program_name,version,
             # Episode vars
             fake_episode_id,episode_num,episode_start,episode_end,assess_ord,
             episode_elapsed,episode_length,
             # Revised episode vars
             rev_fake_episode_id,rev_episode_num,rev_episode_start,rev_episode_end,rev_assess_ord,
             rev_episode_elapsed,rev_episode_length,
             # Assessment vars
             assess_type,assess_period,assess_date,assessor,assess_status,most_recent,
             # Personal Characteristics
             living_situation,
             # Assessment
             tier:n_crit,
             # Everything else from local vars
             everything())
    
write.csv(scrub_fas,"data/scrub_fas.csv", row.names = F)
