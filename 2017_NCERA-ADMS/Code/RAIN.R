# CLEANING RAINFALL DATA

library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)

rain_dir <- "C:/Users/gio/Documents/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/RAIN/"

# ACRE ----------------------------------------------------------
ACRE <- read_excel(paste0(rain_dir, "ACRE Weather.xlsx"), 
                   sheet = "HOURLY", col_types = c("date", "date", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric"))

ACRE %>%
  mutate(timestamp = round_date(Date, unit = "minute"),
         precip_mm = `Precipitation (mm)`) %>%
  select(timestamp, precip_mm) %>%
  mutate(site = "ACRE") -> acre_hourly
  
acre_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup()%>%
  mutate(site = "ACRE") -> acre_daily



# DEFI_R ----------------------------------------------------------
DEFI_R <- read_excel(paste0(rain_dir, "DEFI_R On-Site Weather 1999-2003 1 Hour Interval.xlsx"), 
                   sheet = "1999-2003", col_types = c("date", "numeric", "numeric", "numeric"))

DEFI_R %>%
  mutate(timestamp = round_date(`DATE&TIME`, unit = "minute"),
         precip_mm = CLIM12) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>% 
  filter(!is.na(timestamp)) -> defi_r_hourly

DEFI_R <- read_excel(paste0(rain_dir, "DEFI_R Weather.xlsx"), 
                     sheet = "HOURLY", col_types = c("date", "numeric", "numeric", "numeric",
                                                        "numeric", "numeric", "numeric", "numeric"))
DEFI_R %>%
  filter(!is.na(`DATE&TIME`)) %>%
  mutate(timestamp = round_date(`DATE&TIME`, unit = "minute"),
         precip_mm = CLIM12) %>%
  mutate(timestamp = floor_date(timestamp, unit = "hours")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  filter(timestamp > max(defi_r_hourly$timestamp)) %>%
  bind_rows(defi_r_hourly) %>%
  arrange(timestamp) %>%
  mutate(site = "DEFI_R") -> defi_r_hourly

defi_r_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "DEFI_R") -> defi_r_daily


# DPAC ----------------------------------------------------------
DPAC <- read_excel(paste0(rain_dir, "DPAC Weather.xlsx"), 
                   sheet = "HOURLY", col_types = c("date", "date", "numeric",
                                                   "numeric", "numeric", "numeric"))

DPAC %>%
  filter(!is.na(Date)) %>% select(Date, Time, `Precipitation (mm)`) %>%
  mutate(hour = hour(Time) - 1) %>%
  mutate(hour = ifelse(hour < 0, 23, hour)) %>%
  mutate(timestamp = ymd_h(paste(Date, hour))) %>%
  mutate(timestamp = round_date(timestamp, unit = "minute")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(`Precipitation (mm)`)) %>% 
  ungroup() %>%
  mutate(site = "DPAC") -> dpac_hourly

dpac_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "DPAC") -> dpac_daily


# FULTON ----------------------------------------------------------
FULTON <- read_excel(paste0(rain_dir, "FULTON Weather.xlsx"), 
                   sheet = "HOURLY", col_types = c("date", "numeric", "numeric", "numeric",
                                                   "numeric", "numeric", "numeric", "numeric"))

FULTON %>%
  filter(!is.na(`DATE&TIME`)) %>%
  mutate(timestamp = round_date(`DATE&TIME`, unit = "minute"),
         precip_mm = CLIM12) %>%
  mutate(timestamp = floor_date(timestamp, unit = "hours")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(site = "FULTON") -> fulton_hourly

fulton_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "FULTON") -> fulton_daily



# HENRY ----------------------------------------------------------
HENRY <- read_excel(paste0(rain_dir, "HENRY Weather.xlsx"), 
                     sheet = "HOURLY", col_types = c("date", "date", "numeric"))

HENRY %>%
  filter(!is.na(Date)) %>%
  mutate(timestamp = round_date(Date, unit = "minute")) %>%
  mutate(timestamp = round_date(timestamp, unit = "hours")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(`Precipitation (mm)`)) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(site = "HENRY") -> henry_hourly

henry_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "HENRY") -> henry_daily




# MUDS1 ----------------------------------------------------------
MUDS1 <- read_excel(paste0(rain_dir, "MUDS1 Weather.xlsx"), 
                    col_types = c("date", "numeric", "numeric", "numeric"))
MUDS1 %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = Rainfall * 25.4) %>%
  ungroup() %>%
  mutate(site = "MUDS1") -> muds1_daily


# MUDS2 ----------------------------------------------------------
MUDS2 <- read_excel(paste0(rain_dir, "MUDS2 Weather.xlsx"), 
                    col_types = c("date", "numeric"))
MUDS2 %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = `Daily Precipitation` * 25.4) %>%
  ungroup() %>%
  mutate(site = "MUDS2") -> muds2_daily


# MUDS3_OLD ----------------------------------------------------------
MUDS3 <- read_excel(paste0(rain_dir, "MUDS3_OLD Weather.xlsx"), 
                    col_types = c("date", "numeric"))
MUDS3 %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = `Daily Precipitation ` * 25.4) %>%
  ungroup() %>%
  mutate(site = "MUDS3") -> muds3_daily


# MUDS4 ----------------------------------------------------------
MUDS4 <- read_excel(paste0(rain_dir, "MUDS4 Weather.xlsx"), 
                    col_types = c("date", "numeric"))
MUDS4 %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = `Daily rainfall` * 25.4) %>%
  ungroup() %>%
  mutate(site = "MUDS4") -> muds4_daily



# SERF_SD ----------------------------------------------------------
SERF_SD <- read_excel(paste0(rain_dir, "SERF_SD Weather.xlsx"), 
                      sheet = "HOURLY", col_types = c("date", "date", "numeric"))

SERF_SD %>%
  filter(!is.na(`Date `)) %>%
  mutate(Time = round_date(`Time `, unit = "hour")) %>%
  mutate(hour = hour(Time) - 1) %>%
  mutate(hour = ifelse(hour < 0, 23, hour)) %>%
  mutate(timestamp = ymd_h(paste(`Date `, hour))) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(`Precipitation (mm)`)) %>%
  ungroup() %>%
  mutate(site = "SERF_SD") -> serf_sd_hourly

serf_sd_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "SERF_SD") -> serf_sd_daily


# STJOHNS ----------------------------------------------------------
STJOHNS <- read_excel(paste0(rain_dir, "STJOHNS Weather.xlsx"), 
                      col_types = c("numeric", "numeric", "date", 
                                    "numeric", "numeric", "numeric", "text"))

STJOHNS %>%  
  filter(!is.na(Timestamp)) %>%
  mutate(timestamp = ifelse(Timestamp > "2013-12-1 00:00:00", 
                            round_date(Timestamp, unit = "hour"), Timestamp)) %>%
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(timestamp = floor_date(timestamp, unit = "hours")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(`Rainfall (mm)`)) %>%
  ungroup() %>%
  mutate(site = "STJOHNS") -> stjohns_hourly

stjohns_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "STJOHNS") -> stjohns_daily



# STORY ----------------------------------------------------------
STORY <- read_excel(paste0(rain_dir, "STORY Weather.xlsx"), 
                    sheet = "daily rainfall", col_types = c("date","numeric"))

STORY %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(`Daily Rainfall`)) %>%
  ungroup() %>%
  mutate(site = "STORY") -> story_daily



# TIDE_OLD ----------------------------------------------------------
TIDE <- read_excel(paste0(rain_dir, "TIDE_OLD Weather (Precip).xlsx"),
                   sheet = "2007", col_types = c("date", "date", "numeric"))
for(i in 2008:2012){
  a <- read_excel(paste0(rain_dir, "TIDE_OLD Weather (Precip).xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric"))
  TIDE <- bind_rows(TIDE, a)
}

TIDE %>%
  filter(!is.na(Date), year(Date) < 2011) %>%
  mutate(hour = hour(Time)) %>%
  mutate(timestamp = ymd_h(paste(Date, hour))) %>%
  group_by(timestamp) %>% 
  summarise(precip_mm = sum(`Precipitation (mm)`)) %>%
  ungroup() %>%
  mutate(site = "TIDE") -> tide_hourly
  
TIDE %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(`Precipitation (mm)`)) %>%
  ungroup() %>%
  mutate(site = "TIDE") -> tide_daily


# UBWC ----------------------------------------------------------
UBWC <- read_excel(paste0(rain_dir, "UBWC Weather.xlsx"),
                   col_types = c("text", "date", "numeric"))

UBWC %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(Precipitation)) %>%
  ungroup() %>%
  mutate(site = "UBWC") -> ubwc_daily



# VANWERT ----------------------------------------------------------
VANWERT <- read_excel(paste0(rain_dir, "VANWERT Weather.xlsx"), 
                     sheet = "HOURLY", col_types = c("date", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric"))

VANWERT %>%
  filter(!is.na(`DATE&TIME`)) %>%
  mutate(timestamp = round_date(`DATE&TIME`, unit = "hour")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(CLIM12)) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(site = "VANWERT") -> vanwert_hourly

vanwert_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "VANWERT") -> vanwert_daily




# HICKS ----------------------------------------------------------
HICKS <- read_excel(paste0(rain_dir, "CSCAP_WeatherStation_2011.xlsx"),
                   sheet = "RainOut", col_names = FALSE,
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
for(i in 2012:2015){
  a <- read_excel(paste0(rain_dir, "CSCAP_WeatherStation_", i, ".xlsx"),
                  sheet = "RainOut", col_names = FALSE,
                  col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
  HICKS <- bind_rows(HICKS, a)
}

HICKS %>%
  select(X0, X2) %>%
  filter(!is.na(X0)) %>% 
  mutate(timestamp = round_date(X0, unit = "hour")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(X2)) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(site = "HICKS") -> hicks_hourly
  
hicks_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "HICKS") -> hicks_daily



# SERF_IA ----------------------------------------------------------
SERF_IA <- read_csv(file = paste0(rain_dir, "SERF_IA_hourly.csv"),
                    col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                     doy = col_skip(), precip = col_double()))
SERF_IA %>%
  filter(!is.na(date)) %>%
  mutate(timestamp = round_date(date, unit = "minute")) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(precip)) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(site = "SERF_IA") -> serf_ia_hourly

serf_ia_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "SERF_IA") -> serf_ia_daily



# MAASS ----------------------------------------------------------
MAASS <- read_excel(paste0(rain_dir, "MAASS Weather.xlsx"),
                   sheet = "2013", col_types = c("date", "numeric", "numeric"))
for(i in 2014:2016){
  a <- read_excel(paste0(rain_dir, "MAASS Weather.xlsx"),
                  sheet = as.character(i), col_types = c("date", "numeric", "numeric"))
  MAASS <- bind_rows(MAASS, a)
}

MAASS %>%
  filter(!is.na(TIMESTAMP)) %>%
  mutate(timestamp = round_date(TIMESTAMP, unit = "minute")) %>%
  group_by(timestamp) %>% 
  summarise(precip_mm = sum(Rain_mm_Tot)) %>%
  ungroup() %>%
  mutate(site = "MAASS") -> maass_hourly

maass_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "MAASS") -> maass_daily



# BEAR ----------------------------------------------------------
BEAR <- read_excel(paste0(rain_dir, "BEAR Weather.xlsx"),
                    sheet = "2011", col_types = c("date", "numeric", "numeric"))
for(i in 2012:2016){
  a <- read_excel(paste0(rain_dir, "BEAR Weather.xlsx"),
                  sheet = as.character(i), col_types = c("date", "numeric", "numeric"))
  BEAR <- bind_rows(BEAR, a)
}

BEAR %>%
  filter(!is.na(TIMESTAMP)) %>%
  mutate(timestamp = round_date(TIMESTAMP, unit = "minute")) %>%
  group_by(timestamp) %>% 
  summarise(precip_mm = sum(Rain_mm_Tot)) %>%
  ungroup() %>%
  mutate(site = "BEAR") -> bear_hourly

bear_hourly %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "BEAR") -> bear_daily



# FAIRM ----------------------------------------------------------
FAIRM <- read_csv(file = paste0(rain_dir, "Rainfall FAIRM 05232015 - 12132016.csv"), 
                  col_types = cols(Date = col_datetime(format = "%m/%d/%Y"), 
                                   `Rainfall (in)` = col_double()))

FAIRM %>%
  filter(!is.na(Date)) %>%
  mutate(timestamp = as.Date(Date),
         precip_mm = `Rainfall (in)` * 25.4) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  ungroup() %>%
  mutate(site = "FAIRM") -> fairm_daily


# CLAY ----------------------------------------------------------
CLAY <- read_csv(file = paste0(rain_dir, "Rainfall CLAY_R 2013-2016.csv"), 
                  col_types = cols(Date = col_datetime(format = "%m/%d/%Y"), 
                                   `Rainfall (in)` = col_double()))

CLAY %>%
  filter(!is.na(Date)) %>%
  mutate(timestamp = as.Date(Date),
         precip_mm = `Rainfall (in)` * 25.4) %>%
  group_by(timestamp) %>%
  summarise(precip_mm = sum(precip_mm, na.rm = T)) %>%
  ungroup() %>%
  mutate(site = "CLAY") -> clay_daily





# ------------------------------------------------
bind_rows(lapply(ls(pattern = "hourly"), get)) %>%
  select(site, timestamp, precip_mm) -> TD_hourly

# convert time too actual UTC
CST_sites <- c("BEAR", "MAASS", "SERF_IA", "SERF_SD", "HICKS", "CLAY", "FAIRM")
TD_hourly %>%
  mutate(timestamp = ifelse(site %in% CST_sites, timestamp - hours(6), timestamp - hours(5))) %>%
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(timestamp = as.character.Date(timestamp)) %>%
  mutate(timestamp = paste0(timestamp, "+00")) -> TD_hourly_DARYL
  
bind_rows(lapply(ls(pattern = "daily"), get)) %>%
  select(site, date, precip_mm) -> TD_daily

write.csv(TD_hourly_DARYL, file = paste0(rain_dir, "TD_hourly_DARYL.csv"), row.names = FALSE)
write.csv(TD_hourly, file = paste0(rain_dir, "TD_hourly.csv"), row.names = FALSE)
write.csv(TD_daily, file = paste0(rain_dir, "TD_daily.csv"), row.names = FALSE)


data_dir <- "C:/Users/gio/Documents/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/"
write.csv(TD_daily, file = paste0(data_dir, "RAIN_daily.csv"), row.names = FALSE)

