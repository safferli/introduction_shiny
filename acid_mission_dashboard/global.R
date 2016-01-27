options(bitmapType='cairo', scipen = 999)
suppressPackageStartupMessages(library(shiny))
library(markdown)
suppressPackageStartupMessages(library(bit64))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(htmlwidgets)
library(rpivotTable)
library(ggplot2)
library(grid)
# library(xlsx)

setwd("D:/github/game_analytics/acid_mission_dashboard/")
load("acid_mission_dashboard.Rda")
# setwd("/var/analytics/apps/acid_mission_dashboard/")
# load("/var/analytics/data/acid_mission_dashboard.Rda")

list_factors <- c("mission_status", "mission_type", "mission_location", "mission_objective", "mission_name", "assassin_class", "hireling_type", "hireling_class", "user_level", "date", "mission_failid")
list_colors <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e")

runApp(launch.browser = TRUE)


