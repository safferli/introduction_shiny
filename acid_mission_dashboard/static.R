rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx2048m")
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo', scipen = 999)
library(bit64)
# library(RODBC)
# library(XML)
# library(jsonlite)
library(data.table)
# library(doBy)
# library(plyr)
# library(readr)
library(dplyr)
library(xlsx)
# library(reshape2)
# library(zoo)
library(fasttime)
# library(compiler)
# library(ggplot2)
# library(grid)
# library(scales)
# library(RColorBrewer)
# source("D:/work/Git/myfun/r_functions.R")



# Define your workspace: "X:/xxx/"
workspace <- "D:/work/R/"
server <- "ACID"
# range <- c(20150306, gsub("-", "", Sys.Date()))

# # sql_missions
# INSERT OVERWRITE LOCAL DIRECTORY '/opt/kpiserv/export_data/BIABB-2146/missions'
# SELECT 
# user_id, 
# user_level, 
# substring_index(substring_index(`data`, '<assassinId>', -1), '</assassinId>', 1) as assassin_id,
# substring_index(substring_index(`data`, '<assassinClass>', -1), '</assassinClass>', 1) as assassin_class,
# substring_index(substring_index(`data`, '<hirelingId>', -1), '</hirelingId>', 1) as hireling_id,
# substring_index(substring_index(`data`, '<hirelingType>', -1), '</hirelingType>', 1) as hireling_type,
# substring_index(substring_index(`data`, '<hirelingClass>', -1), '</hirelingClass>', 1) as hireling_class,
# substring_index(substring_index(`data`, '<missionStatus>', -1), '</missionStatus>', 1) as mission_status,
# substring_index(substring_index(`data`, '<missionType>', -1), '</missionType>', 1) as mission_type,
# substring_index(substring_index(`data`, '<missionId>', -1), '</missionId>', 1) as mission_id,
# substring_index(substring_index(`data`, '<fullName>', -1), '</fullName>', 1) as mission_name,
# substring_index(substring_index(`data`, '<missionPlayTimeInMs>', -1), '</missionPlayTimeInMs>', 1) as mission_playtime,
# xpath_string(`data`, '//diesInMission') as mission_deaths,
# xpath_string(`data`, '//failReasonID') as mission_failid,
# xpath(`data`, '/statusMission/missionStats/hotSpotStats/count/text()') as mission_hotspots,
# time
# FROM acid_int_20.fct_actions_info
# WHERE name = 'StatusMission' AND date_dim >= 20150306

sql_missions <- fread("D:/data/hadoop/2146_missions.csv", sep = "|")
setnames(sql_missions, c("user_id", "user_level", "assassin_id", "assassin_class", "hireling_id", "hireling_type", "hireling_class", "mission_status", "mission_type", "mission_id", "mission_name", "mission_playtime", "mission_deaths", "mission_failid", "mission_hotspots", "time_string"))

# # sql_devices
# INSERT OVERWRITE LOCAL DIRECTORY '/opt/kpiserv/export_data/BIABB-2146/devices'
# SELECT user_id, browser, count(time) as n_sessions
# FROM acid_int_20.fct_sessions 
# WHERE type = 'Login' AND date_dim >= 20150306
# GROUP BY user_id, browser

sql_devices <- fread("D:/data/hadoop/2146_devices.csv", sep = "|")
setnames(sql_devices, c("user_id", "device_type", "n_sessions"))



# Preparing data 1
info_devices <- read.csv(file = paste0("D:/R/info/acid_devices.csv"), stringsAsFactors = FALSE)

info_location <- data.table(
  from = c("G_It_Mo_Da", "G_It_Mo_Ni", "G_It_FiPa_Da", "G_It_FiSaCr_Su", "G_It_FiPa_Ni", "G_It_RoCo_Af", "G_It_RoCo_Fo", "G_It_RoSaAn_Ov", "G_It_FiSaCr", "G_It_RoSaAn_St"),
  to = c("Monteriggioni-Day-D0", "Monteriggioni-Night-D1", "FirenzePalazzo-Day-D2", "FirenzeSantaCroce-Sunny-D3", "FirenzePalazzo-Night-D4", "RomaColosseum-Afternoon-D5", "RomaColosseum-Foggy-D7", "RomaSant'Angelo-Overcast-D8", "FirenzeSantaCroce-Stormy-D9", "RomaSant'Angelo-Stormy-D10")
) %>% select(from, to) %>% setnames(c("location", "mission_location"))

  
info_objective <- data.table(
  from = c("DeAr" ,"Lo" ,"Ga" ,"Ki" ,"CoKi" ,"KiTy" ,"IdKi" ,"FrCa" ,"Pa" ,"PaRa" ,"Es" ,"Ta" ,"TaKi"),
  to1 = c("DeliverArea", "Loot", "Gather", "Kill", "ContractKill", "KillType", "IdentifyKill", "FreeCaptives", "Parkour", "ParkourRace", "Escort", "Tail", "TailKill"),
  to2 = c("Courier", "Pillage", "Recovery", "Assassinate", "Killcontract", "Bountyhunt", "Trackdown", "Liberation", "Free-run", "Parkour", "Escort", "Tail", "Stalk")
) %>% select(from, to1) %>% setnames(c("objective", "mission_objective"))

df_devices <- sql_devices %>%
  group_by(user_id) %>%
  arrange(desc(n_sessions)) %>%
  summarize(device_type = first(device_type)) %>%
  merge(info_devices, by = "device_type", all.x = TRUE)

users_supported <- df_devices$user_id[df_devices$device_support == 1]



# Preparing data 2
# a <- sql_missions %>% sample_n(10000)
df_missions <- sql_missions %>%
  filter(mission_status != "Revive") %>%
  filter(user_id %in% users_supported) %>%
  mutate(id = 1:length(user_id)) %>%
  group_by(id) %>%
  mutate(user_id = gsub("^USER::0*(\\d+)::NOVA", "\\1", user_id),
         #assassin_slot = as.integer(gsub("^USER::(\\d+)::NOVA::A::(\\d+)", "\\2", assassin_id)),
         hireling_type = ifelse(nchar(hireling_type) > 100, as.character(NA), hireling_type),
         hireling_class = ifelse(nchar(hireling_class) > 100, as.character(NA), hireling_class),
         mission_status = ifelse(mission_status == "Win", "Won", ifelse(mission_status == "Loose", "Lost", mission_status)),
         location = gsub("^(G_It_Mo_Da|G_It_Mo_Ni|G_It_FiPa_Da|G_It_FiSaCr_Su|G_It_FiPa_Ni|G_It_RoCo_Af|G_It_RoCo_Fo|G_It_RoSaAn_Ov|G_It_FiSaCr|G_It_RoSaAn_St).*", "\\1", mission_name),
         objective = gsub("_", "", gsub(".*(_DeAr_|_Lo_|_Ga_|_Ki_|_CoKi_|_KiTy_|_IdKi_|_FrCa_|_Pa_|_PaRa_|_Es_|_Ta_|_TaKi_).*", "\\1", mission_name)),
         mission_failid = ifelse(mission_status == "Started", as.integer64(NA), mission_failid),
         mission_hscount = as.integer(ifelse(mission_status == "Started", NA, sum(as.integer(strsplit(mission_hotspots, "\\\002")[[1]])))),
         mission_playtime = as.integer(ifelse(mission_status == "Started", NA, round(mission_playtime/1000))),
         time = fastPOSIXct(time_string, tz = "UTC"),
         date = as.Date(time, tz = "UTC")) %>%
  ungroup() %>%
  merge(info_location, by = "location", all.x = TRUE) %>%
  merge(info_objective, by = "objective", all.x = TRUE) %>%
  mutate(mission_location = ifelse(mission_type == "Story", mission_name, mission_location),
         mission_objective = ifelse(mission_type == "Story", mission_name, mission_objective))

  
df_raw <- df_missions %>% 
  mutate(user_id = as.integer(user_id),
         assassin_class = factor(assassin_class, levels = sort(unique(assassin_class))),
         hireling_type = factor(hireling_type, levels = sort(unique(hireling_type))),
         hireling_class = factor(hireling_class, levels = sort(unique(hireling_class))),
         mission_status = factor(mission_status, levels = c("Started", "Won", "Lost")),
         mission_type = factor(mission_type, levels = sort(unique(mission_type))),
         mission_location = factor(mission_location, levels = info_location$mission_location),
         mission_objective = factor(mission_objective, levels = info_objective$mission_objective),
         mission_name = factor(mission_name, levels = sort(unique(mission_name))),
         mission_failid = as.character(mission_failid),
         mission_failid = factor(mission_failid, levels = sort(unique(mission_failid)))
         ) %>%
  select(user_id, user_level, assassin_class, hireling_type, hireling_class, 
         mission_status, mission_type, mission_location, mission_objective, mission_name, 
         mission_deaths, mission_failid, mission_hscount, mission_playtime, date, time)


setwd("d:/R/shiny/acid_mission_dashboard/")
save(df_raw, file="acid_mission_dashboard.Rda")



cutoff <- as.Date("2015-03-06")
cutoff <- as.Date("2015-04-09")
df_data <- df_raw %>% filter(date >= cutoff)






# Overall stats
summary_daily <- df_data %>% 
  filter(mission_status != "Revive") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(date, mission_type, mission_status) %>%
  summarize(N = n()) %>%
  spread(date, N) %>%
  arrange(mission_type, mission_status)

summary_perplayer <- df_data %>%
  filter(mission_status != "Revive") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(user_id, mission_type, mission_status) %>%
  summarize(N = n()) %>%
  group_by(mission_type, mission_status) %>%
  summarize(mean = mean(N),
            q10 = quantile(N, probs = 0.10), 
            q25 = quantile(N, probs = 0.25), 
            q50 = quantile(N, probs = 0.50), 
            q75 = quantile(N, probs = 0.75), 
            q90 = quantile(N, probs = 0.90), 
            max = max(N),
            total = sum(N)) %>%
  arrange(mission_type, mission_status)

summary_cl <- df_data %>%
  filter(mission_status != "Revive") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(assassin_class, user_level, mission_type, mission_status, user_id) %>%
  summarize(N = n()) %>%
  group_by(mission_type, mission_status, assassin_class, user_level) %>%
  summarize(mean = mean(N),
            q10 = quantile(N, probs = 0.10), 
            q25 = quantile(N, probs = 0.25), 
            q50 = quantile(N, probs = 0.50), 
            q75 = quantile(N, probs = 0.75), 
            q90 = quantile(N, probs = 0.90), 
            max = max(N),
            total = sum(N)) %>%
  arrange(mission_type, mission_status, assassin_class, user_level)

summary_clol <- df_data %>%
  filter(mission_status != "Revive") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective, user_id) %>%
  summarize(N = n()) %>%
  group_by(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective) %>%
  summarize(mean = mean(N),
            q10 = quantile(N, probs = 0.10), 
            q25 = quantile(N, probs = 0.25), 
            q50 = quantile(N, probs = 0.50), 
            q75 = quantile(N, probs = 0.75), 
            q90 = quantile(N, probs = 0.90), 
            max = max(N),
            total = sum(N)) %>%
  arrange(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective)

summary_clol_playtime <- df_data %>%
  filter(mission_status != "Revive") %>%
  filter(mission_status != "Started") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective) %>%
  summarize(mean = mean(mission_playtime),
            q10 = quantile(mission_playtime, probs = 0.10), 
            q25 = quantile(mission_playtime, probs = 0.25), 
            q50 = quantile(mission_playtime, probs = 0.50), 
            q75 = quantile(mission_playtime, probs = 0.75), 
            q90 = quantile(mission_playtime, probs = 0.90), 
            max = max(mission_playtime),
            total = sum(as.numeric(mission_playtime))) %>%
  arrange(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective)

summary_clol_deaths <- df_data %>%
  filter(mission_status != "Revive") %>%
  filter(mission_status != "Started") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective) %>%
  summarize(mean = mean(mission_deaths),
            q10 = quantile(mission_deaths, probs = 0.10), 
            q25 = quantile(mission_deaths, probs = 0.25), 
            q50 = quantile(mission_deaths, probs = 0.50), 
            q75 = quantile(mission_deaths, probs = 0.75), 
            q90 = quantile(mission_deaths, probs = 0.90), 
            max = max(mission_deaths),
            total = sum(as.numeric(mission_deaths))) %>%
  arrange(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective)

summary_clol_hotspots <- df_data %>%
  filter(mission_status != "Revive") %>%
  filter(mission_status != "Started") %>%
  mutate(mission_status = factor(mission_status, levels = c("Started", "Won", "Lost"))) %>%
  group_by(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective) %>%
  summarize(mean = mean(mission_hscount),
            q10 = quantile(mission_hscount, probs = 0.10), 
            q25 = quantile(mission_hscount, probs = 0.25), 
            q50 = quantile(mission_hscount, probs = 0.50), 
            q75 = quantile(mission_hscount, probs = 0.75), 
            q90 = quantile(mission_hscount, probs = 0.90), 
            max = max(mission_hscount),
            total = sum(as.numeric(mission_hscount))) %>%
  arrange(mission_type, mission_status, assassin_class, user_level, mission_location, mission_objective)




# Testing
# names(df_data)

summary_heatmap <- df_data %>% 
  filter(mission_status != "Revive") %>%
  filter(mission_type == "Random") %>%
  filter(!is.na(mission_location)) %>%
  group_by(mission_status, mission_location, mission_objective) %>%
  summarize(N = n()) %>% 
  ungroup() %>%
  dcast(mission_location + mission_objective ~ mission_status, value.var = "N") %>%
  mutate(Unfinished = Started - Won - Lost)
  
  

q <- ggplot(a) +
  aes(x = mission_location, y = mission_objective) + 
  geom_tile(aes(fill = N), colour = "white") +
  geom_text(aes(label = N), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(mission_status ~ .)
ggsave(file = paste0(workspace, "data/ACID/2146 - ACID mission heatmap (from ", 
                     gsub("-", "", as.character(cutoff)), 
                     " ).jpeg"), plot = q, width = 10, height = 15)










a <- df_raw %>% 
  filter(mission_type == "Random") %>%
  filter(!is.na(mission_location)) %>%
  group_by(mission_status, mission_location, mission_objective) %>%
  summarize(N = n()) %>% 
  ungroup() %>%
  spread(mission_status, N) %>%
  mutate(Unfinished = Started - Won - Lost) %>%
  gather(mission_status, N, -mission_location, -mission_objective) %>%
  mutate(N = ifelse(N < 0, 0, N)) %>%
  filter(!is.na(N) & N > 0)

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(a$N) 

q <- ggplot(a) +
  aes(x = mission_location, y = mission_objective) + 
  geom_tile(aes(fill = N)) +
  geom_text(aes(label = N), size = 3) +
  #   scale_fill_gradient(low = "white", high = "steelblue") +
  scale_fill_gradientn(colours=cols, limits=c(0, t), breaks=round(seq(0, t, by=t/8)/5000)*5000, guide=guide_colourbar()) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "", title = "Missions by status/location/objective") +
  facet_grid(. ~ mission_status)



b <- df_raw %>% 
  filter(mission_status != "Started") %>%
  filter(mission_type == "Random") %>%
  group_by(mission_location, mission_objective, assassin_class, mission_status) %>%
  summarize(N = n()) %>%
  spread(mission_status, N) %>%
  mutate(win_ratio = Won / (Won + Lost), N = Won + Lost, N = ifelse(is.na(N), 0, N)) %>%
  select(mission_location, mission_objective, assassin_class, N, win_ratio) %>%
  filter(N != 0)

q <- ggplot(b) +
  aes(x = mission_location, y = mission_objective) + 
  geom_tile(aes(fill = win_ratio)) +
  geom_text(aes(label = paste0(round(win_ratio, 2)*100, "%")), size = 3) +
  scale_fill_gradient2(low = "#d73027", mid = "#f7f7f7", high = "#4575b4", midpoint = 0.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "", title = "Win rate by assassin class and mission location/objective") +
  facet_grid(. ~ assassin_class)



c <- df_raw %>%
  filter(mission_status != "Started") %>%
  filter(mission_type == "Random") %>%
  filter(!is.na(hireling_type)) %>%
  group_by(mission_location, mission_objective, hireling_type, mission_status) %>%
  summarize(N = n()) %>%
  spread(mission_status, N) %>%
  mutate(win_ratio = Won / (Won + Lost), N = Won + Lost, N = ifelse(is.na(N), 0, N)) %>%
  select(mission_location, mission_objective, hireling_type, N, win_ratio)

q <- ggplot(c %>% filter(N >= 100)) +
  aes(x = mission_location, y = mission_objective) + 
  geom_tile(aes(fill = win_ratio)) +
  geom_text(aes(label = paste0(round(win_ratio, 2)*100, "%")), size = 3) +
  scale_fill_gradient2(low = "#d73027", mid = "#f7f7f7", high = "#4575b4", midpoint = 0.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "", title = "Win rate by hireling type and mission location/objective") +
  facet_grid(. ~ hireling_type)



# a <- df_raw %>% filter(mission_status != "Started") %>% group_by(mission_failid) %>% summarize(N = n()) %>% ungroup() %>% arrange(desc(N)) %>% filter(N >= mean(N))
# a <- df_raw %>% filter(mission_failid == "9218868437227407266") %>% select(mission_status, mission_type) %>% unique()
# a <- df_raw %>% filter(mission_status == "Started") %>% group_by(mission_failid) %>% summarize(N = n()) %>% ungroup() %>% arrange(desc(N)) 
# 
# b <- df_raw %>%
#   filter(mission_status != "Started") %>%
#   group_by(mission_status, mission_type, mission_location, mission_objective, assassin_class, hireling_type, user_level, mission_failid) %>%
#   summarize(N = n()) %>% ungroup() %>%
#   group_by(mission_status, mission_type, mission_location, mission_objective, assassin_class, hireling_type, user_level) %>%
#   mutate(ratio = N / sum(N)) %>% 
#   filter(mission_failid %in% a$mission_failid) %>%
#   filter(ratio > 0.9) %>%
#   filter(N > 100)





a <- df_raw %>%
  filter(mission_status != "Started") %>%
  filter(!is.na(hireling_type)) %>%
  select(user_level, assassin_class, hireling_type, 
         mission_status, mission_location, mission_objective,
         mission_deaths, mission_hscount, mission_playtime)

b <- dist(a, method = "manhattan")
