# JEFF STROCK's Synthesis Paper SOIL MOISTURE data

library(tidyverse)
library(lubridate)
library(stringr)
library(zoo)
library(readxl)


# SOIL MOISTURE -------------------------
# there are only 5 sites that feets Jeff's criteria:
#   soil moisture measured in both Free and Controlled Drainage plots of each site
# List of selected sites:
#   DPAC, HICKS_B, SERF_IA, SERF_SD, STJONS 


# read SERF_IA 2016 sm data ------------------------------------------
p1 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "1", col_names = FALSE, skip = 3)
p2 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "2", col_names = FALSE, skip = 3)
p3 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "3", col_names = FALSE, skip = 3) 
p4 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "4", col_names = FALSE, skip = 3) 
p5 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "5", col_names = FALSE, skip = 3) 
p6 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "6", col_names = FALSE, skip = 3) 
p7 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "7", col_names = FALSE, skip = 3) 
p8 <- read_excel("~/TD/Research Data/SERF_IA/SERF 2016 SMT.xlsx", 
                 sheet = "8", col_names = FALSE, skip = 3) 

p1$plotid <- "S1"
p2$plotid <- "S2"
p3$plotid <- "S3"
p4$plotid <- "S4"
p5$plotid <- "S5"
p6$plotid <- "S6"
p7$plotid <- "S7"
p8$plotid <- "S8"

p1
p2
p3 %>% 
  select(-X3) %>%
  mutate_at(vars(X1:X11), funs(as.numeric)) -> p3
p4 %>% 
  select(-X3) -> p4
p5 %>%
  select(-X3) -> p5
p6 %>%
  select(-X3) %>%
  mutate_at(vars(X1:X11), funs(as.numeric))-> p6
p7 %>%
  select(-X3) -> p7
p8 



names(p1) <- c("timestamp", "10_cm_SM", "10_cm_Temp",
               "20_cm_SM", "20_cm_Temp",
               "40_cm_SM", "40_cm_Temp",
               "60_cm_SM", "60_cm_Temp",
               "100_cm_SM", "100_cm_Temp", "plotid")
serf_sm <- p1
for(i in 2:8) {
  temp <- get(paste0("p", i))
  names(temp) <- c("timestamp", "10_cm_SM", "10_cm_Temp",
                           "20_cm_SM", "20_cm_Temp",
                           "40_cm_SM", "40_cm_Temp",
                           "60_cm_SM", "60_cm_Temp",
                           "100_cm_SM", "100_cm_Temp", "plotid")
  bind_rows(serf_sm, temp) -> serf_sm
}


serf_sm %>%
  select(timestamp, plotid, matches("SM")) %>%
  gather(key=depth, value = SM, 3:7) %>%
  separate("depth", c("depth", "unit", "rm"), sep = "_") %>%
  mutate(SM = ifelse(SM < 0, NA, SM)) %>%
  mutate(site = "SERF_IA") %>% 
  select(timestamp, site, plotid, depth, unit, rm, SM) -> serf_sm

serf_sm %>%
  mutate(timestamp = as.Date(timestamp)) %>%
  filter(year(timestamp) >= 2015) %>%
  group_by(timestamp, site, plotid, depth, unit) %>%
  summarise(SM = mean(SM, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(depth = as.numeric(depth)) %>%
  arrange(plotid, timestamp, depth) -> serf_sm_2016

rm(list = ls(pattern = "p"))

# read SERF_IA 2011-2014 sm data ------------------------------------------
sm_dir <- "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/SM/"

serf_sm_d <- read_csv(file = paste0(sm_dir, "sm_SERF_", "1", "_20110601_20151207.csv"), 
                      col_types = cols(`10 cm Temp (C)` = col_skip(), 
                                       `100 cm Temp (C)` = col_skip(), `20 cm Temp (C)` = col_skip(), 
                                       `40 cm Temp (C)` = col_skip(), `60 cm Temp (C)` = col_skip(), 
                                       dm1_f = col_skip(), dm2_f = col_skip(), 
                                       dm3_f = col_skip(), dm4_f = col_skip(), 
                                       dm5_f = col_skip(), dt1_f = col_skip(), 
                                       dt2_f = col_skip(), dt3_f = col_skip(), 
                                       dt4_f = col_skip(), dt5_f = col_skip()))

for(i in 2:8){
  temp <- read_csv(file = paste0(sm_dir, "sm_SERF_", i, "_20110601_20151207.csv"), 
                   col_types = cols(`10 cm Temp (C)` = col_skip(), 
                                    `100 cm Temp (C)` = col_skip(), `20 cm Temp (C)` = col_skip(), 
                                    `40 cm Temp (C)` = col_skip(), `60 cm Temp (C)` = col_skip(), 
                                    dm1_f = col_skip(), dm2_f = col_skip(), 
                                    dm3_f = col_skip(), dm4_f = col_skip(), 
                                    dm5_f = col_skip(), dt1_f = col_skip(), 
                                    dt2_f = col_skip(), dt3_f = col_skip(), 
                                    dt4_f = col_skip(), dt5_f = col_skip()))
  serf_sm_d <- bind_rows(serf_sm_d, temp)
}



serf_sm_d %>%
  gather(key = depth, value = SM, 3:7) %>%
  mutate(site = "SERF_IA",
         unit = word(depth, 2),
         depth =as.numeric(word(depth, 1)),
         plotid = paste0("S", plotid),
         timestamp = as.Date(timestamp)) %>% 
  select(timestamp, site, plotid, depth, unit, SM) %>% 
  bind_rows(serf_sm_2016) %>%
  arrange(plotid, timestamp, depth) %>% 
  spread(key = plotid, value =SM) %>%
  mutate(year = year(timestamp)) %>%
  group_by(depth, year) %>% 
  mutate(S1 = na.approx(S1, timestamp, maxgap = 7, na.rm = FALSE),
         S2 = na.approx(S2, timestamp, maxgap = 7, na.rm = FALSE),
         S3 = na.approx(S3, timestamp, maxgap = 7, na.rm = FALSE),
         S4 = na.approx(S4, timestamp, maxgap = 7, na.rm = FALSE),
         S5 = na.approx(S5, timestamp, maxgap = 7, na.rm = FALSE),
         S6 = na.approx(S6, timestamp, maxgap = 7, na.rm = FALSE),
         S7 = na.approx(S7, timestamp, maxgap = 7, na.rm = FALSE),
         S8 = na.approx(S8, timestamp, maxgap = 7, na.rm = FALSE)) %>%
  ungroup() %>%
  gather(key = plotid, value = SM, S1:S8) %>%
  select(timestamp, site, plotid, depth, unit, SM) -> SM



# read DPAC 2011-2016 sm data ------------------------------------------
read_csv(file = paste0(sm_dir, "sm_DPAC_", "NE", "_20110701_20170110.csv")) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_DPAC_", "NW", "_20110701_20170110.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_DPAC_", "SE", "_20110701_20170110.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_DPAC_", "SW", "_20110701_20170110.csv"))) %>%
  select(timestamp, plotid, matches("Moisture")) -> dpac_sm_d

dpac_sm_d %>%
  gather(key = depth, value = SM, 3:7) %>%
  mutate(site = "DPAC",
         unit = word(depth, 2),
         depth = as.numeric(word(depth, 1)),
         timestamp = as.Date(timestamp)) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  arrange(plotid, timestamp, depth) %>%
  spread(key = plotid, value =SM) %>%
  mutate(year = year(timestamp)) %>%
  group_by(depth, year) %>% 
  mutate(NE = na.approx(NE, timestamp, maxgap = 7, na.rm = FALSE),
         NW = na.approx(NW, timestamp, maxgap = 7, na.rm = FALSE),
         SE = na.approx(SE, timestamp, maxgap = 7, na.rm = FALSE),
         SW = na.approx(SW, timestamp, maxgap = 7, na.rm = FALSE)) %>%
  ungroup() %>%
  gather(key = plotid, value = SM, NE:SW) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  bind_rows(SM) -> SM


# read DPAC 2011-2016 sm data ------------------------------------------
read_csv(file = paste0(sm_dir, "sm_DPAC_", "NE", "_20110701_20170110.csv")) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_DPAC_", "NW", "_20110701_20170110.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_DPAC_", "SE", "_20110701_20170110.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_DPAC_", "SW", "_20110701_20170110.csv"))) %>%
  select(timestamp, plotid, matches("Moisture")) -> dpac_sm_d

dpac_sm_d %>%
  gather(key = depth, value = SM, 3:7) %>%
  mutate(site = "DPAC",
         unit = word(depth, 2),
         depth = as.numeric(word(depth, 1)),
         timestamp = as.Date(timestamp)) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  arrange(plotid, timestamp, depth) %>%
  spread(key = plotid, value =SM) %>%
  mutate(year = year(timestamp)) %>%
  group_by(depth, year) %>% 
  mutate(NE = na.approx(NE, timestamp, maxgap = 7, na.rm = FALSE),
         NW = na.approx(NW, timestamp, maxgap = 7, na.rm = FALSE),
         SE = na.approx(SE, timestamp, maxgap = 7, na.rm = FALSE),
         SW = na.approx(SW, timestamp, maxgap = 7, na.rm = FALSE)) %>%
  ungroup() %>%
  gather(key = plotid, value = SM, NE:SW) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  bind_rows(SM) -> SM


 
# read HICKS_B 2012-2016 sm data ------------------------------------------
read_csv(file = paste0(sm_dir, "sm_HICKS.B_", "E", "_20120601_20160401.csv")) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_HICKS.B_", "W", "_20120601_20160401.csv"))) %>%
  select(timestamp, plotid, matches("Moisture")) -> hicks_sm_d

hicks_sm_d %>%
  gather(key = depth, value = SM, 3:7) %>%
  mutate(site = "HICKS_B",
         unit = word(depth, 2),
         depth = as.numeric(word(depth, 1)),
         timestamp = as.Date(timestamp)) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  arrange(plotid, timestamp, depth) %>%
  spread(key = plotid, value =SM) %>%
  mutate(year = year(timestamp)) %>%
  group_by(depth, year) %>% 
  mutate(E = na.approx(E, timestamp, maxgap = 7, na.rm = FALSE),
         W = na.approx(W, timestamp, maxgap = 7, na.rm = FALSE)) %>%
  ungroup() %>%
  gather(key = plotid, value = SM, E:W) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  bind_rows(SM) -> SM




# read STJOHNS 2012-2015 sm data ------------------------------------------
read_csv(file = paste0(sm_dir, "sm_STJOHNS_", "WN1", "_20120101_20151201.csv")) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_STJOHNS_", "WN2", "_20120101_20151201.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_STJOHNS_", "WN3", "_20120101_20151201.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_STJOHNS_", "WS1", "_20120101_20151201.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_STJOHNS_", "WS2", "_20120101_20151201.csv"))) %>%
  bind_rows(read_csv(file = paste0(sm_dir, "sm_STJOHNS_", "WS3", "_20120101_20151201.csv"))) %>%
  select(timestamp, plotid, matches("Moisture")) -> stjohns_sm_d

stjohns_sm_d %>%
  gather(key = depth, value = SM, 3:7) %>%
  mutate(site = "STJOHNS",
         unit = word(depth, 2),
         depth = as.numeric(word(depth, 1)),
         timestamp = as.Date(timestamp)) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  arrange(plotid, timestamp, depth) %>%
  spread(key = plotid, value =SM) %>%
  mutate(year = year(timestamp)) %>%
  group_by(depth, year) %>% 
  mutate(WN1 = na.approx(WN1, timestamp, maxgap = 7, na.rm = FALSE),
         WN2 = na.approx(WN2, timestamp, maxgap = 7, na.rm = FALSE),
         WN3 = na.approx(WN3, timestamp, maxgap = 7, na.rm = FALSE),
         WS1 = na.approx(WS1, timestamp, maxgap = 7, na.rm = FALSE),
         WS2 = na.approx(WS2, timestamp, maxgap = 7, na.rm = FALSE),
         WS3 = na.approx(WS3, timestamp, maxgap = 7, na.rm = FALSE)) %>%
  ungroup() %>%
  gather(key = plotid, value = SM, WN1:WS3) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  bind_rows(SM) -> SM



# read SERF_SD 2015-2016 sm data ------------------------------------------
serf_sd_sm <- read_csv(file = paste0(sm_dir, "SERF_SD Soil Data - soil moisture.csv")) 

serf_sd_sm %>%
  filter(location == "Middle") %>%
  mutate(timestamp = as.Date(date, "%m/%d/%Y"),
         site = "SERF_SD",
         plotid = as.character(plotID),
         unit = word(depth, 2),
         depth = as.numeric(word(depth, 1)),
         SM = `Soil Moisture using sensor (cm3 cm-3)`) %>%
  select(timestamp, site, plotid, depth, unit, SM) %>%
  arrange(plotid, timestamp, depth) %>%
  bind_rows(SM) -> SM
  
write.csv(SM, file = paste0(sm_dir, "SM_5sites.csv"), row.names = FALSE)
write.csv(SM, 
          file = "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/SM.csv", 
          row.names = FALSE)


# Calculated Soil Water ---------------------------------------------------
# assign DWM treatments and average over them
SM <- read.csv(file = "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/SM.csv")

SM %>%
  filter(!plotid %in% c("S1", "S6", "S7", "S8")) %>%
  mutate(dwm = ifelse(plotid %in% c("NE", "SW", "E", "S2", "S5", "7", "WS1", "WS2", "WS3"), "FD", "CD")) %>%
  mutate(SM = ifelse(SM > 1, SM/100, SM)) %>%               # fixing DPAC 2013-06-20 to 2013-07-10 carzyness 
  group_by(timestamp, site, dwm, depth) %>%
  summarize(SM = mean(SM, na.rm = TRUE)) %>%
  mutate(thickness = ifelse(depth < 30, 15, 
                            ifelse(depth == 60, 30, 20))) %>%
  ungroup() %>%
  mutate(SWV = thickness * SM) %>%                                   # Soil Water Volume in cm at each layer
  mutate(timestamp = as.Date(timestamp)) -> SW

SW %>%
  group_by(timestamp, site, dwm) %>%
  summarise(water = sum(SWV, na.rm = FALSE)) %>%
  ungroup()-> SWV100cm
SW %>%
  filter(depth < 50) %>%
  group_by(timestamp, site, dwm) %>%
  summarise(water = sum(SWV, na.rm = FALSE)) %>%
  ungroup()-> SWV50cm
SW %>%
  filter(depth < 100) %>%
  group_by(timestamp, site, dwm) %>%
  summarise(water = sum(SWV, na.rm = FALSE)) %>%
  ungroup()-> SWV80cm



SWV100cm %>%
  ggplot(aes(x=timestamp, y=water, colour = dwm)) +
  geom_point(alpha=0.15, na.rm = TRUE) +
  stat_summary(fun.y = mean, geom = "line", size = 1, na.rm = T) +
  scale_color_manual(values = c("#2D79B6", "#EDB22A")) 
  #geom_smooth(se = FALSE) 

SWV100cm %>%
  mutate(site = as.factor(site)) %>%
  ggplot(aes(x=timestamp, y=water, colour = dwm)) +
  geom_point(size = 0.5) +
  theme_light() +
  facet_grid(site ~ .) +
  ggtitle("Soil Water Variation in Soil Profile",
          subtitle = "(depth of the profile = 100 cm)") +
  scale_y_continuous("Amount of Water in Soil Profile, cm") +
  scale_x_date("", date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("#2D79B6", "#EDB22A"), name = "DWM") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(20,10,0,10), "pt")) +
  ggsave(file = "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Figures/SM/SM100.png",
         width = 12, height = 8)

SWV50cm %>%
  mutate(site = as.factor(site)) %>%
  ggplot(aes(x=timestamp, y=water, colour = dwm)) +
  geom_point(size = 0.5) +
  theme_light() +
  facet_grid(site ~ .) +
  ggtitle("Soil Water Variation in Soil Profile",
          subtitle = "(depth of the profile = 50 cm)") +
  scale_y_continuous("Amount of Water in Soil Profile, cm") +
  scale_x_date("", date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("#2D79B6", "#EDB22A"), name = "DWM") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(20,10,0,10), "pt")) +
  ggsave(file = "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Figures/SM/SM50.png",
         width = 12, height = 8)  



SWV100cm %>%
  mutate(year = year(timestamp),
         month = month(timestamp)) %>%
  group_by(site, dwm, year, month) %>%
  summarise(water = sum(water)) %>%
  group_by(site, dwm, month) %>%
  summarise(water = mean(water, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = factor(month.abb[month], levels = month.abb)) %>%
  ggplot(aes(x=month, y=water, fill = dwm)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Monthly Soil Water Variation in Soil Profile") +
  scale_y_continuous("Amount of Water in 1m Soil Profile, cm") +
  scale_fill_manual(values = c("#2D79B6", "#EDB22A"), name = "DWM") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))
  


SWV100cm %>%
  mutate(year = year(timestamp),
         month = month(timestamp)) %>%
  group_by(site, dwm, year, month) %>%
  summarise(water = mean(water, na.rm = TRUE)) %>%
  group_by(site, dwm, month) %>%
  summarise(water = mean(water, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = factor(month.abb[month], levels = month.abb)) %>%
  ggplot(aes(x=month, y=water, fill = dwm)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Monthly Soil Water Variation in Soil Profile",
          subtitle = "(depth of the profile = 100 cm)") +
  scale_y_continuous("Amount of Water in Soil Profile, cm") +
  facet_grid(site ~ .) +
  scale_fill_manual(values = c("#2D79B6", "#EDB22A"), name = "DWM") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 4),
        strip.text = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(20,10,10,10), "pt"),
        plot.background = element_blank()) +
  ggsave(file = "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Figures/SM/SM100sites.png",
         width = 12, height = 8) 







SWV50cm %>%
  mutate(year = year(timestamp),
         month = month(timestamp)) %>%
  group_by(site, dwm, year, month) %>%
  summarise(water = mean(water, na.rm = TRUE)) %>%
  group_by(site, dwm, month) %>%
  summarise(water = mean(water, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = factor(month.abb[month], levels = month.abb)) %>%
  ggplot(aes(x=month, y=water, fill = dwm)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Monthly Soil Water Variation in Soil Profile",
          subtitle = "(depth of the profile = 50 cm)") +
  scale_y_continuous("Amount of Water in Soil Profile, cm") +
  facet_grid(site ~ .) +
  scale_fill_manual(values = c("#2D79B6", "#EDB22A"), name = "DWM") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 4),
        strip.text = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 16, face = "bold"), 
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(20,10,10,10), "pt"),
        plot.background = element_blank()) +
  ggsave(file = "~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Figures/SM/SM50sites.png",
         width = 12, height = 8) 



