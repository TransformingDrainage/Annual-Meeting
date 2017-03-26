# CLEANING FLOW DATA

library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)

data_dir <- "C:/Users/gio/Documents/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/"
flow_dir <- paste0(data_dir, "FLOW/")

# AUGLA ----------------------------------------------------------
AUGLA <- read_excel(paste0(flow_dir, "AUGLA Tile Flow.xlsx"), 
                   sheet = "2008", col_types = c("date", "date", "numeric", "numeric"))
for(i in 2009:2014){
  a <- read_excel(paste0(flow_dir, "AUGLA Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", "numeric"))
  AUGLA <- bind_rows(AUGLA, a)
}

AUGLA %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `West WAT1 Tile Flow`:`East WAT1 Tile Flow`) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "AUGLA") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> augla_flow

  

# CLAY_C ----------------------------------------------------------
CLAY_C <- read_excel(paste0(flow_dir, "CLAY_C Tile Flow.xlsx"), 
                    sheet = "2013", col_types = c("date", "date", "numeric"))
for(i in 2014:2016){
  a <- read_excel(paste0(flow_dir, "CLAY_C Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric"))
  CLAY_C <- bind_rows(CLAY_C, a)
}

CLAY_C %>%
  filter(!is.na(Date)) %>%
  mutate(date = as.Date(Date), 
         id = "FD", 
         site = "CLAY_C") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(`FD WAT1 Tile Flow`)) %>%
  ungroup() -> clay_c_flow



# CLAY_R ----------------------------------------------------------
CLAY_R <- read_excel(paste0(flow_dir, "CLAY_R Tile Flow.xlsx"), 
                    sheet = "2013", col_types = c("date", "date", "numeric", "numeric"))
for(i in 2014:2016){
  a <- read_excel(paste0(flow_dir, "CLAY_R Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", "numeric"))
  CLAY_R <- bind_rows(CLAY_R, a)
}

CLAY_R %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `SI WAT1 Tile Flow`:`CD WAT1 Tile Flow`) %>%
  mutate(Time = year(Date)) %>%
  group_by(Time, id) %>%
  mutate(WAT1 = na.approx(WAT1, Date, maxgap = 10, na.rm = FALSE)) %>%
  ungroup() %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "CLAY_R") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> clay_r_flow



# CRAWF ----------------------------------------------------------
CRAWF <- read_excel(paste0(flow_dir, "CRAWF Tile Flow.xlsx"), 
                    sheet = "2008", col_types = c("date", "date", "numeric", "numeric"))
for(i in 2009:2014){
  a <- read_excel(paste0(flow_dir, "CRAWF Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", "numeric"))
  CRAWF <- bind_rows(CRAWF, a)
}

CRAWF %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `South WAT1 Tile Flow`:`North WAT1 Tile Flow`) %>%
  mutate(Time = year(Date)) %>%
  group_by(Time, id) %>%
  mutate(WAT1 = na.approx(WAT1, Date, maxgap = 10, na.rm = FALSE)) %>%
  ungroup() %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "CRAWF") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> crawf_flow



# DEFI_M ----------------------------------------------------------
DEFI_M <- read_excel(paste0(flow_dir, "DEFI_M Tile Flow.xlsx"), 
                    sheet = "2008", col_types = c("date", "date", "numeric", "numeric"))
for(i in 2009:2014){
  a <- read_excel(paste0(flow_dir, "DEFI_M Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", "numeric"))
  DEFI_M <- bind_rows(DEFI_M, a)
}

DEFI_M %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `West WAT1 Tile Flow`:`East WAT1 Tile Flow`) %>%
  # mutate(Time = year(Date)) %>%
  # group_by(Time, id) %>%
  # mutate(WAT1 = na.approx(WAT1, Date, maxgap = 5, na.rm = FALSE)) %>%
  # ungroup() %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "DEFI_M") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> defi_m_flow



# DPAC ----------------------------------------------------------
DPAC <- read_excel(paste0(flow_dir, "DPAC Tile Flow.xlsx"), 
                     sheet = "2012", col_types = c("date", "date", "numeric", 
                                                   "numeric", "numeric", "numeric"))
for(i in 2013:2016){
  a <- read_excel(paste0(flow_dir, "DPAC Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", 
                                                         "numeric", "numeric", "numeric"))
  DPAC <- bind_rows(DPAC, a)
}

DPAC %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `NW WAT1 Tile Flow`:`SE WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "DPAC") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> dpac_flow



# FAIRM ----------------------------------------------------------
FAIRM <- read_excel(paste0(flow_dir, "FAIRM Tile Flow.xlsx"), 
                   sheet = "2013", col_types = c("date", "date", "numeric", "numeric"))
for(i in 2014:2016){
  a <- read_excel(paste0(flow_dir, "FAIRM Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", "numeric"))
  FAIRM <- bind_rows(FAIRM, a)
}

FAIRM %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `Sump1 WAT1 Tile Flow`:`Sump2 WAT1 Tile Flow`) %>%
  mutate(WAT1 = ifelse(is.na(WAT1), 0, WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "FAIRM") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> fairm_flow



# HARDIN ----------------------------------------------------------
HARDIN <- read_excel(paste0(flow_dir, "HARDIN Tile Flow.xlsx"), 
                    sheet = "2009", col_types = c("date", "date", "numeric", "numeric"))

HARDIN %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `North WAT1 Tile Flow`:`South WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "HARDIN") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> hardin_flow



# HICKS_B ----------------------------------------------------------
HICKS_B <- read_excel(paste0(flow_dir, "HICKS_B Tile Flow.xlsx"), 
                    sheet = "2006", col_types = c("date", "date", "numeric", "numeric"))
for(i in 2007:2015){
  a <- read_excel(paste0(flow_dir, "HICKS_B Tile Flow.xlsx"),
                  sheet = as.character(i), col_types = c("date", "date", "numeric", "numeric"))
  HICKS_B <- bind_rows(HICKS_B, a)
}

HICKS_B %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `BE WAT1 Tile Flow`:`BW WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "HICKS_B") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> hicks_b_flow

  

# MUDS2 ----------------------------------------------------------
MUDS2 <- read_excel(paste0(flow_dir, "MUDS2 Tile Flow.xlsx"), 
                    sheet = "2010", col_types = c("date", "text", rep("numeric", 8)))
for(i in 2011:2013){
  a <- read_excel(paste0(flow_dir, "MUDS2 Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "text", rep("numeric", 8)))
  MUDS2 <- bind_rows(MUDS2, a)
}

MUDS2 %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `103 WAT1 Tile Flow`:`206 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MUDS2") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> muds2_flow


# MUDS3_OLD ----------------------------------------------------------
MUDS3_OLD <- read_excel(paste0(flow_dir, "MUDS3_OLD Tile Flow.xlsx"), 
                    sheet = "2010", col_types = c("date", "text", rep("numeric", 4)))
for(i in 2011:2013){
  a <- read_excel(paste0(flow_dir, "MUDS3_OLD Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "text", rep("numeric", 4)))
  MUDS3_OLD <- bind_rows(MUDS3_OLD, a)
}

MUDS3_OLD %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `101 WAT1 Tile Flow`:`203 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MUDS3_OLD") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> muds3_old_flow


# MUDS4 ----------------------------------------------------------
MUDS4 <- read_excel(paste0(flow_dir, "MUDS4 Tile Flow.xlsx"), 
                        sheet = "2010", col_types = c("date", "text", rep("numeric", 8)))
for(i in 2011:2013){
  a <- read_excel(paste0(flow_dir, "MUDS4 Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "text", rep("numeric", 8)))
  MUDS4 <- bind_rows(MUDS4, a)
}

MUDS4 %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `101 WAT1 Tile Flow`:`204 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "MUDS4") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> muds4_flow



# SERF_IA ----------------------------------------------------------
SERF_IA <- read_excel(paste0(flow_dir, "SERF_IA Tile Flow.xlsx"), 
                    sheet = "2007", col_types = c("date", "date", rep("numeric", 6)))
for(i in 2008:2016){
  a <- read_excel(paste0(flow_dir, "SERF_IA Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "date", rep("numeric", 6)))
  SERF_IA <- bind_rows(SERF_IA, a)
}

SERF_IA %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `S1 WAT1 Tile Flow`:`S6 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "SERF_IA") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> serf_ia_flow




# SERF_SD ----------------------------------------------------------
SERF_SD <- read_excel(paste0(flow_dir, "SERF_SD Tile Flow.xlsx"), 
                      sheet = "2015", col_types = c("date", "date", rep("numeric", 6)))
for(i in 2016){
  a <- read_excel(paste0(flow_dir, "SERF_SD Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "date", rep("numeric", 6)))
  SERF_SD <- bind_rows(SERF_SD, a)
}

SERF_SD %>%
  filter(!is.na(Date)) %>%
  select(1, 3, 4) %>%
  gather(key=id, value = WAT1, 2:3) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "SERF_SD") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> serf_sd_flow



# STJOHNS ----------------------------------------------------------
STJOHNS <- read_excel(paste0(flow_dir, "STJOHNS Tile Flow.xlsx"), 
                      sheet = "2009", col_types = c("date", "date", rep("numeric", 2)))
for(i in 2010:2015){
  a <- read_excel(paste0(flow_dir, "STJOHNS Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "date", rep("numeric", 2)))
  STJOHNS <- bind_rows(STJOHNS, a)
}

STJOHNS %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `WS WAT1 Tile Flow`:`WN WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "STJOHNS") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> stjohns_flow



# STORY ----------------------------------------------------------
STORY <- read_excel(paste0(flow_dir, "STORY Tile Flow.xlsx"), 
                      sheet = "2006", col_types = c("date", "date", rep("numeric", 12)))
for(i in 2007:2009){
  a <- read_excel(paste0(flow_dir, "STORY Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "date", rep("numeric", 12)))
  STORY <- bind_rows(STORY, a)
}

STORY %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `1 WAT1 Tile Flow`:`12 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "STORY") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> story_flow



# TIDE_OLD ----------------------------------------------------------
TIDE_OLD <- read_excel(paste0(flow_dir, "TIDE_OLD Tile Flow.xlsx"), 
                    sheet = "2007", col_types = c("date", "date", rep("numeric", 4)))
for(i in 2008:2011){
  a <- read_excel(paste0(flow_dir, "TIDE_OLD Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "date", rep("numeric", 4)))
  TIDE_OLD <- bind_rows(TIDE_OLD, a)
}

TIDE_OLD %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `H2 WAT1 Tile Flow`:`H5 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "TIDE_OLD") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)*10) %>%
  ungroup() -> tide_old_flow



# UBWC ----------------------------------------------------------
UBWC <- read_excel(paste0(flow_dir, "UBWC Tile Flow.xlsx"), 
                       sheet = "2005", col_types = c("date", "date", rep("numeric", 2)))
for(i in 2006:2012){
  a <- read_excel(paste0(flow_dir, "UBWC Tile Flow.xlsx"), 
                  sheet = as.character(i), col_types = c("date", "date", rep("numeric", 2)))
  UBWC <- bind_rows(UBWC, a)
}

UBWC %>%
  filter(!is.na(Date)) %>%
  gather(key=id, value = WAT1, `B2 WAT1 Tile Flow`:`B4 WAT1 Tile Flow`) %>%
  filter(!is.na(WAT1)) %>%
  mutate(date = as.Date(Date), 
         id = word(id), 
         site = "UBWC") %>%
  group_by(site, id, date) %>%
  summarise(WAT1 = sum(WAT1)) %>%
  ungroup() -> ubwc_flow




# ------------------------------------------------
bind_rows(lapply(setdiff(ls(pattern = "flow"), "flow_dir"), get)) -> TD_flow


write.csv(TD_flow, file = paste0(flow_dir, "TD_flow.csv"), row.names = FALSE)
write.csv(TD_flow, file = paste0(data_dir, "FLOW.csv"), row.names = FALSE)






















