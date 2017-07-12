
## Episode Grouper for CAFAS®

## Rationale: ##################################################################
#
# The current dataset includes an episode number,  which is generated when an 
# employee activates a case in the FAS system, while initial assessment may 
# occur later for multiple reasons (e.g. no-show, rescheduling, etc.). It is 
# therefore more accurate to use initial assessment date as start, since the 
# assessment serves as the first step of service provision.
#
# An additional inconsistency occurs regarding episodes during which children 
# transition, or 'age out' of the PECFAS® tool and workers begin to use the 
# CAFAS® tool instead.  When this occurs, some workers complete an 'initial 
# CAFAS®', rather than continuing same episode, and thereby create a new episode 
# identifier. There is also inconsistent practice around combining episodes that 
# occur within 90 days.
#
# In order to consistently define treatment episodes for measuring outcomes and 
# other features of clinical treatment, there needs to be a consistent definition 
# of a treatment episode. 
#
# This logic assumes that the dataframe passed into this function: 
# (a) includes all historical PECFAS and CAFAS data for each child, 
# (b) is formatted/processed in the same way as output from 'readCAFAS.R' script
#
################################################################################


# Create consistent episode groupings that span PECFAS and CAFAS 
# Caveat: Comparisons between baseline and update score using mixed episodes are
# less likely to show improvement, since PECFAS has a max score of 210, while 
# CAFAS has a max score of 240.

library(dplyr); library(magrittr); library(tidyr); library(car)
library(lubridate)

grp_fas <-
  sub_fas %>%
  # Calc values based on default episode groupings
  group_by(id,episode_num) %>%
  arrange(assess_date) %>%
  mutate(assess_ord = as.integer(row_number(assess_date)),
         episode_length = ifelse(is.na(episode_end) == TRUE,
                                 yes = round(as.numeric(difftime(today(),
                                                                 as.Date(episode_start),
                                                                 units = "days")),
                                             digits = 0),
                                 no = round(as.numeric(difftime(as.Date(episode_end),
                                                                as.Date(episode_start),
                                                                units = "days")),
                                            digits = 0))
         ) %>%
  # Use init CAFAS date as start date (as opposed to episode start) 
  # Group by id and cmh.  Using this definition, an episode never bridges multiple 
  # organizations, but may bridge service areas or programs within an org.
  # Records without these grouping variables will not be included.
  # Also, each org gets to determine the logic for assigning ids, which could 
  # lead to the same id being applied to different persons
  # Note: Returning person is merged with existing ID in system, but may not 
  # always be matched due to lookup process
  filter(is.na(id) == F, is.na(cmh) == F) %>%
  group_by(id,cmh) %>%
  # Sort by date of assessment to determine sequence
  arrange(id,cmh,assess_date) %>%
  mutate(i = 1, # Start at 1 ...
         # The first assessment for any ID in a given CMH is automatically 
         # marked as episode 1, then increments from there when conditions are met
         ord = cumsum(i),
         since = as.numeric(difftime(as.Date(assess_date),
                                     as.Date(lag(assess_date)))),
         # Tag an assessment as the 'last' in an episode if:
         # the next one is 'Initial...' AND >= 120 days elapse before next one
         # Note: By this point all 'initial' assessments which are followed by
         # a 'revised initial' have been removed. 
         # Anyone coming back >120 after previous assessment AND marked as 
         # 'Initial' or 'Revised Initial' would start a new episode.
         # 'Revised Initial' assessments are also needed to trigger episodes,
         # since there are instances when the initial is incompletely filled out 
         # by access staff are are therefore excluded from this data, due to
         # incompleteness.  In these cases, the revised initial would be used to 
         # trigger the beginning of a new episode.
         last = lead(assess_type) %in% c("Initial PECFAS","Initial CAFAS","Revised Initial") & lead(since) >= 120,
         # Exception: Do not start new episode if the first assessment in the 
         # episode has been cued by assess_type of 'Initial PECFAS' and then 
         # 'Initial CAFAS' occurs during the course of that episode. 
         # While previous methods of initializing a new episode have included 
         # starting a new episode with revision of the treatment plan due to 
         # significant clinical changes, this approach uses a ....
         last = ifelse(assess_type == 'Initial CAFAS' 
                       & lead(version) == 'PECFAS' 
                       & lead(since) >= 120,
                       yes = F, no = last),
         # Increment records based on tagged 'last' assessments
         rev_episode_num = sprintf("%03d", head(cumsum(c(F, last == T)), -1)),
         # Re-number starting at 1 and coerce to factor
         rev_episode_num = sprintf("%02d", as.numeric(rev_episode_num) + 1)
  ) %>%
  ungroup() %>%
  group_by(id,cmh,rev_episode_num) %>%
  # Add episode 'start' and 'end' dates using new episode groupings
  # Redefine end of episode as final assessment date
  mutate(rev_episode_start = min(assess_date),
         rev_episode_end = max(assess_date)
  ) %>%
  # End date of episode is not necessarily marked as 'Exit CAFAS', since this is 
  # inconsistently used in practice (e.g. in the case that someone discontinues 
  # AMA following the assessment). This logic calculates the episode end date
  # as the most recent assessment date. If 'client_status' == 'Active', and the 
  # episode is the most recent episode, then the episode end date is set to NA.
  # If Exit CAFAS is not marked and person not closed, then the end date of the 
  # episode will remain empty.  The episode will continue to be marked as active 
  # and calculations of the duration of the episode will continue to be calculated 
  # between the start date and the most recent system update (rather than an 
  # episode end date) which will lead to longer episodes.
  ungroup() %>%
  group_by(id,cmh) %>%
  mutate(episodes_per_kid = max(rev_episode_num),
         current_episode = rev_episode_num == episodes_per_kid & client_status == 'Active',
         current_episode = car::recode(current_episode,"TRUE = NA"),
         rev_episode_end = ifelse(is.na(current_episode) == T,current_episode,rev_episode_end),
         rev_episode_end = as.Date(rev_episode_end, origin = "1970-01-01")) %>%
  select(-last,-current_episode, -episodes_per_kid,-start_dt,-revised,-since) %>%
  ungroup() %>%
  group_by(id,cmh,rev_episode_num) %>%
  # Order assessment sequence by new episode groups
  mutate(rev_assess_ord = cumsum(i),
         rev_episode_id = paste0(id,"_",cmh,"_",rev_episode_num),
         rev_episode_length = ifelse(is.na(rev_episode_end) == TRUE,
                                     yes = round(as.numeric(difftime(today(),
                                                                     as.Date(rev_episode_start),
                                                                     units = "days")),
                                                 digits = 0),
                                     no = round(as.numeric(difftime(as.Date(rev_episode_end),
                                                                    as.Date(rev_episode_start),
                                                                    units = "days")),
                                                digits = 0)),
         rev_episode_since = round(as.numeric(difftime(assess_date,
                                                       lag(assess_date),
                                                       units = "days")),
                                   digits = 0),
         rev_episode_since = recode(rev_episode_since, "NA = 0"),
         rev_episode_elapsed = cumsum(rev_episode_since)) %>%
  select(-i,-ord) %>%
  # Arrange column order logically
  select(id,client_status,cmh,service_area,program_name,version,
         # Episode vars
         unique_episode_id,episode_num,episode_start,episode_end,assess_ord,
         episode_elapsed,episode_length,
         # Revised episode vars
         rev_episode_id,rev_episode_num,rev_episode_start,rev_episode_end,rev_assess_ord,
         rev_episode_elapsed,rev_episode_length,
         # Assessment vars
         assessmentID,assess_type,assess_period,assess_date,assessor,assess_status,most_recent,
         # Personal Characteristics
         gender,assess_age,age_range,living_situation,
         # Assessment
         tier:n_crit) %>%
  ungroup() %>% droplevels()
  
