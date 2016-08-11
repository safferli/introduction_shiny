##############
# ANNO 6 EVENT SESSION DASHBOARD - global.R
##############
#rm(list = ls()); gc(); gc()
options(java.parameters = "-Xmx4096m")
options(java.parameters = "-XX:-UseConcMarkSweepGC")
options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(RODBC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(readr)
library(reshape2)

setwd("C:/github/introduction_gameanalytics/anno6_event_session_dashboard/")


##############
# CONNECT TO DATABASE
##############
#myconn <- odbcConnect("ANNO6_POSTLAUNCH", uid="Tableau_ANNO6", pwd="T@bl3au_ANNO6")
# myconn <- odbcConnect("DW_ANNO6_POSTLAUNCH")

##############
# VARIABLES
##############
levels <- expand.grid(difficulty = c("Easy","Normal","Hard","Classic"), corp_level = seq(2, 21))
event_lookup <- read.csv("source/anno6_event_sessions.csv") %>%
  select(-name, -quest_guid) %>% mutate(quest_name = substr(quest_name, 14, str_length(quest_name)))

# SERVER OR COMP
file.path <- "source/"

##############
# LOAD SOURCE FUNCTIONS
##############
source("source/charts.R")
