# CLEANING LOAD DATA

library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)


data_dir <- "C:/Users/gio/Documents/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/"
load_dir <- paste0(data_dir, "LOAD/")


# UBWC ----------------------------------------------------------
UBWC <- read_excel(paste0(flow_dir, "UBWC Tile Nitrate-N Load.xlsx"), 
                    sheet = "2005", col_types = c(rep("date", 1), rep("numeric", 4)))
for(i in 2006:2012){
  a <- read_excel(paste0(flow_dir, "UBWC Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 4)))
  UBWC <- bind_rows(UBWC, a)
}

UBWC %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:3) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "UBWC") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>%
  ungroup() -> ubwc_load




# TIDE_OLD ----------------------------------------------------------
TIDE_OLD <- read_excel(paste0(flow_dir, "TIDE_OLD Tile Nitrate-N Load.xlsx"), 
                   sheet = "2007", col_types = c(rep("date", 1), rep("numeric", 8)))
for(i in 2008:2011){
  a <- read_excel(paste0(flow_dir, "TIDE_OLD Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 8)))
  TIDE_OLD <- bind_rows(TIDE_OLD, a)
}

TIDE_OLD %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:5) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "TIDE_OLD") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>%
  ungroup() -> tide_old_load




# STORY ----------------------------------------------------------
STORY <- read_excel(paste0(flow_dir, "STORY Tile Nitrate-N Load.xlsx"), 
                       sheet = "2006", col_types = c(rep("date", 1), rep("numeric", 24)))
for(i in 2007:2009){
  a <- read_excel(paste0(flow_dir, "STORY Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 24)))
  STORY <- bind_rows(STORY, a)
}

STORY %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:13) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "STORY") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>%
  ungroup() %>%
  filter(id %in% c(2, 3, 5, 8, 9, 11)) -> story_load






# SERF_SD ----------------------------------------------------------
SERF_SD <- read_excel(paste0(flow_dir, "SERF_SD Tile Nitrate-N Load.xlsx"), 
                    sheet = "2015", col_types = c(rep("date", 1), rep("numeric", 8)))
for(i in 2016){
  a <- read_excel(paste0(flow_dir, "SERF_SD Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 8)))
  SERF_SD <- bind_rows(SERF_SD, a)
}

SERF_SD %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:3) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "SERF_SD") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>%
  ungroup() %>% 
  filter(!is.na(WAT20)) -> serf_sd_load







# SERF_IA ----------------------------------------------------------
SERF_IA <- read_excel(paste0(flow_dir, "SERF_IA Tile Nitrate-N Load.xlsx"), 
                      sheet = "2007", col_types = c(rep("date", 1), rep("numeric", 12)))
for(i in 2008:2016){
  a <- read_excel(paste0(flow_dir, "SERF_IA Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 12)))
  SERF_IA <- bind_rows(SERF_IA, a)
}

SERF_IA %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:7) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "SERF_IA") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>% 
  filter(!is.na(WAT20)) %>%
  ungroup() -> serf_ia_load






# MUDS2 ----------------------------------------------------------
MUDS2 <- read_excel(paste0(flow_dir, "MUDS2 Tile Nitrate-N Load.xlsx"), 
                      sheet = "2010", col_types = c(rep("date", 1), rep("numeric", 16)))
for(i in 2011:2013){
  a <- read_excel(paste0(flow_dir, "MUDS2 Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 16)))
  MUDS2 <- bind_rows(MUDS2, a)
}

MUDS2 %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:9) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MUDS2") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>% 
  filter(!is.na(WAT20)) %>%
  ungroup() -> muds2_load


# MUDS3_OLD ----------------------------------------------------------
MUDS3_OLD <- read_excel(paste0(flow_dir, "MUDS3_OLD Tile Nitrate-N Load.xlsx"), 
                    sheet = "2010", col_types = c(rep("date", 2), rep("numeric", 8)))
for(i in 2011:2013){
  a <- read_excel(paste0(flow_dir, "MUDS3_OLD Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 2), rep("numeric", 8)))
  MUDS3_OLD <- bind_rows(MUDS3_OLD, a)
}

MUDS3_OLD %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:5) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MUDS3_OLD") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>% 
  filter(!is.na(WAT20)) %>%
  ungroup() -> muds3_old_load







# MUDS4 ----------------------------------------------------------
MUDS4 <- read_excel(paste0(flow_dir, "MUDS4 Tile Nitrate-N Load.xlsx"), 
                        sheet = "2010", col_types = c(rep("date", 2), rep("numeric", 16)))
for(i in 2011:2013){
  a <- read_excel(paste0(flow_dir, "MUDS4 Tile Nitrate-N Load.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 2), rep("numeric", 16)))
  MUDS4 <- bind_rows(MUDS4, a)
}

MUDS4 %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT20")) %>%
  gather(key=id, value = WAT20, 2:9) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MUDS4") %>%
  filter(!is.na(WAT20)) %>%
  group_by(site, id, date) %>%
  summarise(WAT20 = sum(WAT20)) %>% 
  filter(!is.na(WAT20)) %>%
  ungroup() -> muds4_load






# ------------------------------------------------
bind_rows(lapply(setdiff(ls(pattern = "load"), "load_dir"), get)) -> TD_load


write.csv(TD_load, file = paste0(load_dir, "TD_load.csv"), row.names = FALSE)
write.csv(TD_load, file = paste0(data_dir, "LOAD.csv"), row.names = FALSE)

















