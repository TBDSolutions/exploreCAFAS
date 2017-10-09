## explore CAFAS global.R ##

# Load fun, data, libs, source files
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(lubridate)
library(forcats)
library(magrittr)
library(tidyr)
library(broom)
library(caret)
library(xts)
library(car)
library(scales)
library(plotly)
library(DT)
library(dygraphs)
library(parsetR)
library(d3heatmap)
library(RColorBrewer)
library(feather)

# Load de-identified data
scrub_fas <- read_feather("data/scrub_fas.feather")
scrub_fas$LOC <- as.factor(scrub_fas$LOC)

# Calculate most recent assessment date per fake_id
# to allow filtering of data based of clients whose most recent assessment
# is within a specific range

scrub_fas %<>%
  mutate(episode_start = ymd(episode_start),
         episode_end = ymd(episode_end),
         assess_date = ymd(assess_date),
         fake_id = as.factor(fake_id),
         fake_episode_id = as.factor(fake_episode_id)) %>%
  group_by(fake_id) %>%
  mutate(max_date = max(assess_date)) %>%
  ungroup() %>% droplevels()

# Recode CMHSP names
scrub_fas %<>%
  mutate(cmh = fct_recode(cmh,
                          GIHN = "GCCMH",
                          MCN = "MCCMH",
                          `The Right Door` = "ICCMH")
  )

# Add custom color palettes

soft_12 <- c("#c64457","#d7532a","#ae5d35","#d0ab2c","#a69743",
             "#7ac43e","#59a653","#45bc8d","#20d8fd","#725eb3",
             "#934fd0","#c04b91")

soft_20 <- c("#c94572","#71352d","#d14530","#cc8977","#cd7d34",
             "#d2b551","#706a2f","#c7da52","#c4c9a6","#72d752",
             "#559046","#3b5243","#6ed9a9","#76b2c4","#677bc4",
             "#7949d2","#592f7f","#452e4a","#cc52c0","#c596bd")
