# CLEANING WQ DATA

library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)


data_dir <- "C:/Users/gio/Documents/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/"
wq_dir <- paste0(data_dir, "WQ/")



# TIDE_OLD ----------------------------------------------------------
TIDE_OLD <- read_excel(paste0(wq_dir, "TIDE_OLD WQ.xlsx"), 
                       sheet = "2007", col_types = c(rep("date", 1), rep("numeric", 12)))
for(i in 2008:2011){
  a <- read_excel(paste0(wq_dir, "TIDE_OLD WQ.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 12)))
  TIDE_OLD <- bind_rows(TIDE_OLD, a)
}

TIDE_OLD %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT2 Tile")) %>%
  gather(key=id, value = WAT2, 2:5) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "TIDE_OLD") %>%
  filter(!is.na(WAT2)) %>%
  group_by(site, id, date) %>%
  summarise(WAT2 = sum(WAT2)) %>%
  ungroup() %>%
  mutate(id = substr(id, 1, 2)) -> tide_old_nitrate




# STORY ----------------------------------------------------------
STORY <- read_excel(paste0(wq_dir, "STORY WQ.xlsx"), 
                    sheet = "2006", col_types = c(rep("date", 1), rep("numeric", 24)))
for(i in 2007:2009){
  a <- read_excel(paste0(wq_dir, "STORY WQ.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 24)))
  STORY <- bind_rows(STORY, a)
}

STORY %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT2 Tile")) %>%
  gather(key=id, value = WAT2, 2:13) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "STORY") %>%
  filter(!is.na(WAT2)) %>%
  group_by(site, id, date) %>%
  summarise(WAT2 = sum(WAT2)) %>%
  ungroup() %>%
  filter(id %in% c(2, 3, 5, 8, 9, 11)) -> story_nitrate





# STJOHNS ----------------------------------------------------------
STJOHNS <- read_excel(paste0(wq_dir, "STJOHNS WQ.xlsx"), 
                    sheet = "2011", col_types = c(rep("date", 1), rep("numeric", 6)))
for(i in 2012:2015){
  a <- read_excel(paste0(wq_dir, "STJOHNS WQ.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 6)))
  STJOHNS <- bind_rows(STJOHNS, a)
}

STJOHNS %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT2 Tile")) %>%
  gather(key=id, value = WAT2, 2:3) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "STJOHNS") %>%
  filter(!is.na(WAT2)) %>%
  group_by(site, id, date) %>%
  summarise(WAT2 = sum(WAT2)) %>%
  ungroup() -> stjohns_nitrate





# SERF_SD ----------------------------------------------------------
SERF_SD <- read_excel(paste0(wq_dir, "SERF_SD WQ.xlsx"), 
                      sheet = "2015", col_types = c(rep("date", 1), rep("numeric", 8)))
for(i in 2016){
  a <- read_excel(paste0(wq_dir, "SERF_SD WQ.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 8)))
  SERF_SD <- bind_rows(SERF_SD, a)
}

SERF_SD %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT02 Tile")) %>%
  gather(key=id, value = WAT2, 2:3) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "SERF_SD") %>%
  filter(!is.na(WAT2)) %>%
  group_by(site, id, date) %>%
  summarise(WAT2 = sum(WAT2)) %>%
  ungroup() %>% 
  filter(!is.na(WAT2)) -> serf_sd_nitrate



# SERF_IA ----------------------------------------------------------
SERF_IA <- read_excel(paste0(wq_dir, "SERF_IA WQ.xlsx"), 
                      sheet = "2007", col_types = c(rep("date", 1), rep("numeric", 12)))
for(i in 2008:2016){
  a <- read_excel(paste0(wq_dir, "SERF_IA WQ.xlsx"), 
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 12)))
  SERF_IA <- bind_rows(SERF_IA, a)
}

SERF_IA %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT2 Tile")) %>%
  gather(key=id, value = WAT2, 2:7) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "SERF_IA") %>%
  filter(!is.na(WAT2)) %>%
  group_by(site, id, date) %>%
  summarise(WAT2 = sum(WAT2)) %>%
  ungroup() -> serf_ia_nitrate




# MAASS ----------------------------------------------------------
MAASS <- read_excel(paste0(wq_dir, "MAASS WQ.xlsx"), 
                      sheet = "2013", col_types = c(rep("date", 1), rep("numeric", 2)))
for(i in 2014:2016){
  a <- read_excel(paste0(wq_dir, "MAASS WQ.xlsx"),
                  sheet = as.character(i), col_types = c(rep("date", 1), rep("numeric", 2)))
  MAASS <- bind_rows(MAASS, a)
}

MAASS %>%
  filter(!is.na(Date)) %>%
  select(Date, matches("WAT2 Tile")) %>%
  gather(key=id, value = WAT2, 2) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MAASS") %>%
  filter(!is.na(WAT2)) %>%
  group_by(site, id, date) %>%
  summarise(WAT2 = sum(WAT2)) %>%
  ungroup() -> maass_nitrate












































