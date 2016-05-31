## explore CAFAS global.R ##

# Load fun, data, libs, source files
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(magrittr)
library(tidyr)
library(xts)
library(car)
library(scales)
library(lubridate)
library(plotly)
library(DT)
library(ggplot2)
library(rcdimple)
library(dygraphs)
library(parsetR)
library(d3heatmap)
library(RColorBrewer)

# Load de-identified data
scrub_cafas <- read.csv("data/scrub_cafas.csv")

# Create dataframe for calculating measures

outcome_df <-
scrub_cafas %>%
  group_by(fake_episode_id,fake_id,cmh,episode_num) %>%
  mutate(init_CAFAS = assess_ord == min(assess_ord), # for included assessments
         last_CAFAS = assess_ord == max(assess_ord)) %>%
  filter(init_CAFAS == T | last_CAFAS == T) %>%
  mutate(cafas_total_diff = cafas_total - lag(cafas_total), #doesn't work if start of episode not in dataset
         meaning_improve = total_diff >= 20, # total_diff is calc by episode
         # Tag youth with improved Risk Factors between init and most recent
         child_mgmt_improve = child_mgmt == F & lag(child_mgmt) == T,
         behavioral_improve = behavioral == F & lag(behavioral) == T,
         psychosis_risk_improve = psychosis_risk == F & lag(psychosis_risk) == T,
         severe_sud_improve = severe_sud == F & lag(severe_sud) == T,
         suicide_risk_improve = suicide_risk == F & lag(suicide_risk) == T,
         suicide_ideation_improve = suicide_ideation == F & lag(suicide_ideation) == T,
         aggressive_improve = aggressive == F & lag(aggressive) == T,
         sexual_improve = sexual == F & lag(sexual) == T,
         fire_setting_improve = fire_setting == F & lag(fire_setting) == T,
         runaway_improve = runaway == F & lag(runaway) == T,
         risk_improve_num = child_mgmt_improve + behavioral_improve 
         + psychosis_risk_improve + severe_sud_improve 
         + suicide_risk_improve + suicide_ideation_improve
         + aggressive_improve + sexual_improve
         + fire_setting_improve + runaway_improve,
         change_risk_den = lag(child_mgmt) == T 
         | lag(behavioral) == T 
         | lag(psychosis_risk) == T 
         | lag(severe_sud) == T 
         | lag(suicide_risk) == T
         | lag(suicide_ideation) == T
         | lag(aggressive) == T
         | lag(sexual) == T
         | lag(fire_setting) == T
         | lag(runaway) == T, 
         change_risk_num = risk_improve_num >= 1,
         # Calc diff in subscales from init to most recent
         subscale_school_diff = subscale_school - lag(subscale_school),
         subscale_home_diff = subscale_home - lag(subscale_home),
         subscale_community_diff = subscale_community - lag(subscale_community),
         subscale_behavior_diff = subscale_behavior - lag(subscale_behavior),
         subscale_mood_diff = subscale_mood - lag(subscale_mood),
         subscale_selfharm_diff = subscale_selfharm - lag(subscale_selfharm),
         subscale_substance_diff = subscale_substance - lag(subscale_substance),
         subscale_thinking_diff = subscale_thinking - lag(subscale_thinking),
         # Tag youth with changes in any subscale
         change_subscale_log = subscale_school_diff < 0 
         | subscale_home_diff < 0
         | subscale_community_diff < 0
         | subscale_behavior_diff < 0
         | subscale_mood_diff < 0
         | subscale_selfharm_diff < 0
         | subscale_substance_diff < 0
         | subscale_thinking_diff < 0,
         # Define den and num for Severe Impairment measure
         severe_impair_den = lag(subscale_school) == 30
         | lag(subscale_home) == 30 
         | lag(subscale_community) == 30 
         | lag(subscale_behavior) == 30
         | lag(subscale_mood) == 30
         | lag(subscale_selfharm) == 30
         | lag(subscale_substance) == 30
         | lag(subscale_thinking) == 30,
         severe_impair_num = (lag(subscale_school) == 30 & subscale_school_diff < 0)
         | (lag(subscale_home) == 30 & subscale_home_diff < 0)
         | (lag(subscale_community) == 30 & subscale_community_diff < 0)
         | (lag(subscale_behavior) == 30 & subscale_behavior_diff < 0)
         | (lag(subscale_mood) == 30 & subscale_mood_diff < 0)
         | (lag(subscale_selfharm) == 30 & subscale_selfharm_diff < 0)
         | (lag(subscale_substance) == 30 & subscale_substance_diff < 0)
         | (lag(subscale_thinking) == 30 & subscale_thinking_diff < 0),
         # Define den and num for Pervasive Behavioral Impairment measure
         change_pbi_den_log = lag(subscale_school) %in% c(20:30) 
         & lag(subscale_home) %in% c(20:30)
         & lag(subscale_behavior) %in% c(20:30),
         change_pbi_num_log = lag(subscale_school) %in% c(20:30) 
         & lag(subscale_home) %in% c(20:30)
         & lag(subscale_behavior) %in% c(20:30)
         & subscale_school %in% c(0:19) 
         & subscale_home %in% c(0:19)
         & subscale_behavior %in% c(0:19)
  ) %>%
  ungroup() %>%
  select(fake_episode_id,fake_id,cmh,client_status,episode_num,assess_ord,
         episode_elapsed,episode_length,assess_type,assess_date,
         subscale_school_diff:change_subscale_log,
         severe_impair_den,severe_impair_num,
         change_pbi_den_log, change_pbi_num_log,
         cafas_total,total_diff,cafas_total_diff,meaning_improve,improvement,
         child_mgmt_improve:change_risk_num,change_impair,change_behavior,
         improve_one_more) %>%
  filter(is.na(cafas_total_diff) == F) # rm init assessments & episodes w/ 1 rating
