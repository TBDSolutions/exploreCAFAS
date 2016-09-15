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
scrub_fas <- read.csv("data/scrub_fas.csv")

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
