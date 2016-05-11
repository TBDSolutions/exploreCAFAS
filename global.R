## explore CAFAS global.R ##

# Load fun, data, libs, source files
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(magrittr)
library(tidyr)
library(xts)
library(lubridate)
library(plotly)
library(DT)
library(ggplot2)
library(rcdimple)
library(dygraphs)
library(parsetR)
library(d3heatmap)

# Load de-identified data
scrub_cafas <- read.csv("data/scrub_cafas.csv")
