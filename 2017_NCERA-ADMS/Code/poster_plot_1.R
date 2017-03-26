# PLOTTING for TD Annual Meeting 2017

library(extrafont)
font_import(pattern = "Oswald")
loadfonts(device = "win")

YIELD <- read_csv("~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/YIELD.csv")

YIELD %>%
  transmute(state = as.factor(State),
            site = as.factor(`Site ID`),
            plot = as.factor(`Plot ID`),
            year = year,
            dwm = factor(`drainage treatment`, 
                         levels = c("no drainage", "free drainage", "shallow drainage", 
                                    "controlled drainage", "controlled drainage with subirrigation"),
                         labels = c("nd", "fd", "sd", "cd", "si")),
            crop = as.factor(crop),
            yield = as.numeric(`yield (kg/ha)`)) -> yield

yield %>%
  filter(crop %in% c("corn", "soy")) %>%
  filter(dwm %in% c("nd", "fd","cd","si")) %>%
  group_by(site, year, dwm, crop) %>%
  summarise(yield = mean(yield, na.rm = T)) %>%
  ungroup() %>%
  filter(!site %in% c("AUGLA", "CROWF", "DEFI_M", "HARDIN", "HENTRY")) %>%
  group_by(dwm, crop, year) %>%
  summarise(yield = mean(yield, na.rm = T)) %>%
  spread(key = dwm, value= yield) %>%
  mutate(nd = ifelse(is.na(nd), 0, nd),
         fd = ifelse(is.na(fd), 0, fd),
         cd = ifelse(is.na(cd), 0, cd),
         si = ifelse(is.na(si), 0, si)) %>% 
  gather(key=dwm, value=yield, nd:si) %>%
  mutate(dwm = factor(dwm, levels = c("nd", "fd", "cd", "si"))) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=yield, fill =dwm)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_grid(crop ~ ., scales = "free") +
  theme_minimal() +
  #scale_fill_brewer(palette = "YlGnBu") +
  #scale_fill_manual(values = c("darkgoldenrod1", "chocolate", "cornflowerblue", "darkolivegreen")) +
  #scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c')) + #edb22a','#2d79b6
  scale_fill_manual(values = c('#e66101','#edb22a','#2d79b6','#abd9e9'),
                    labels = c("Not Drained", "Free Drainage", 
                               "Controlled Drainage", "CD with Subirrigation")) +
  scale_x_continuous(breaks = 1996:2016) +
  xlab(" ") + 
  ylab("Crop Yiled, kg/ha") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 24, family = "Oswald Regular"),
        axis.text.y = element_text(size = 20, family = "Oswald Regular"),
        axis.text.x = element_text(size = 20, family = "Oswald Regular", angle = 90, vjust = 0.5),
        axis.ticks.y = element_line(),
        axis.line.y = element_line(),
        legend.text = element_text(size = 16, family = "Oswald Regular", margin = 3),
        legend.key.size = unit(2, 'lines'),
        panel.spacing = unit(3, 'lines'))









yield %>%
  filter(crop %in% c("corn", "soy")) %>%
  filter(dwm %in% c("fd","cd")) %>%
  group_by(state, site, year, dwm, crop) %>%
  summarise(yield = mean(yield, na.rm = T)) %>%
  ungroup() %>%
  filter(site %in% c("SERF_IA", "STORY", "DPAC", "HICKS_B", "MUDS2",
                     "MUDS4", "TIDE_OLD", "HARDIN", "STJOHNS")) %>%
  group_by(state, dwm, crop, year) %>%
  summarise(yield = mean(yield, na.rm = T)) %>%
  spread(key = dwm, value= yield) %>%
  mutate(fd = ifelse(is.na(fd), 0, fd),
         cd = ifelse(is.na(cd), 0, cd)) %>% 
  gather(key=dwm, value=yield, fd:cd) %>%
  mutate(dwm = factor(dwm, levels = c("fd", "cd"))) %>%
  ungroup() %>%
  ggplot(aes(x=state, y=yield, fill = dwm)) +
  geom_bar(stat = "summary", fun.y = mean, position = "dodge") +
  facet_grid(crop ~ ., scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = c('#edb22a','#2d79b6'),
                    labels = c("Free Drainage","Controlled Drainage")) +
  xlab(" ") + 
  ylab("Crop Yiled, kg/ha") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 24, family = "Oswald Regular"),
        axis.text.y = element_text(size = 20, family = "Oswald Regular"),
        axis.text.x = element_text(size = 20, family = "Oswald Regular", vjust = 0.5),
        axis.ticks.y = element_line(),
        axis.line.y = element_line(),
        legend.text = element_text(size = 16, family = "Oswald Regular", margin = 3),
        legend.key.size = unit(2, 'lines'),
        panel.spacing.y = unit(5, 'lines'))



# ROUND PLOTS
library(googlesheets)
library(fmsb)

FLOW <- read_csv("~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/FLOW.csv")
CDsites <- c("SERF_IA", "STORY", "DPAC", "HICKS_B", "MUDS2", "MUDS4", "TIDE_OLD", "AUGLA", 
             "CRAWF", "DEFI_M", "HARDIN", "HARDIN_NW", "HENRY", "STJOHNS", "UBWC", "SERF_SD")

#get google sheet docs
as.character(gs_ls("Instrumentation")[, 1]) %>%
  gs_title() %>%
  gs_read(ws = "Plot IDs", col_names = TRUE) -> TDplotsDWM

TDplotsDWM %>%
  filter(`Site ID` %in% CDsites) %>%
  select(STATE, `Site ID`, `Suugested ID`, y1996:y2016) %>%
  rename(state = STATE, site = `Site ID`, plot = `Suugested ID`) %>%
  gather(key=year, value=dwm, y1996:y2016) %>%
  filter(dwm != "n/a") %>%
  mutate(year = sub("y", " ", year)) %>%
  mutate(year = as.integer(year)) %>%
  filter(dwm %in% c("FD", "CD")) -> TDplots


FLOW %>%
  mutate(WAT1 = ifelse(site == "HICKS_B" & year(date) == 2015, NA, WAT20)) %>%  # remove 2015 HICKS_B flow data because of some errors
  filter(site %in% CDsites) %>%
  rename(plot = id) %>%
  mutate(year = year(date)) %>%
  left_join(TDplots, by = c("site", "plot", "year")) %>%
  mutate(dwm = ifelse(site == "AUGLA" & year == 2012 & date < "2012-06-18" & plot == "East", "CD", dwm)) %>%
  mutate(dwm = ifelse(site == "AUGLA" & year == 2012 & date < "2012-06-18" & plot == "West", "FD", dwm)) %>%
  filter(!is.na(dwm)) -> flow
  
  
flow %>%
  mutate(month = month(date)) %>%
  group_by(state, site, year, month, date, dwm) %>%
  summarise(WAT1 = mean(WAT20, na.rm = TRUE)) %>%       #average over DWM plots
  group_by(state, site, year, month, dwm) %>%
  summarise(WAT1 = sum(WAT20, na.rm = TRUE)) %>%        #sum over month   
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")) %>%
  mutate(date = format.Date(date, "%Y-%m")) -> flow_monthly

flow_monthly %>%
  group_by(state, site, year, dwm) %>%
  summarise(WAT1 = sum(WAT20, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key=dwm, value= WAT20) %>%
  mutate(CD = ifelse(is.na(CD), 0, CD),
         FD = ifelse(is.na(FD), 0, FD)) %>%
  gather(key=dwm, value=WAT20, CD:FD) %>%
  mutate(dwm = factor(dwm, levels=c("FD", "CD"))) %>%
  ggplot(aes(x=year, y=WAT20, fill=dwm)) +
  geom_bar(stat = "summary", fun.y = mean, na.rm = T, position = "dodge") +
  scale_fill_manual(values = c('#edb22a','#2d79b6'),
                    labels = c("Free Drainage","Controlled Drainage")) +
  scale_x_continuous(breaks = 2005:2016) +
  xlab(" ") + 
  ylab("Tile Flow, mm") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 26, family = "Oswald Regular"),
        axis.text.y = element_text(size = 24, family = "Oswald Regular"),
        axis.text.x = element_text(size = 24, family = "Oswald Regular", angle = 90, vjust = 0.5),
        axis.ticks.y = element_line(),
        axis.line.y = element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, family = "Oswald Regular", margin = 3),
        legend.key.size = unit(2, 'lines'),
        panel.spacing = unit(3, 'lines'))
  
  
  
  
flow_monthly %>%
  group_by(state, site, year, dwm) %>%
  summarise(WAT1 = sum(WAT20, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dwm = factor(dwm, levels=c("FD", "CD"))) %>%
  ggplot(aes(x=state, y=WAT20, fill=dwm)) +
  geom_bar(stat = "summary", fun.y = mean, na.rm = T, position = "dodge") +
  scale_fill_manual(values = c('#edb22a','#2d79b6'),
                    labels = c("Free Drainage","Controlled Drainage")) +
  xlab(" ") + 
  ylab("Tile Flow, mm") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 26, family = "Oswald Regular"),
        axis.text.y = element_text(size = 24, family = "Oswald Regular"),
        axis.text.x = element_text(size = 24, family = "Oswald Regular", angle = 90, vjust = 0.5),
        axis.ticks.y = element_line(),
        axis.line.y = element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, family = "Oswald Regular", margin = 3),
        legend.key.size = unit(2, 'lines'),
        panel.spacing = unit(3, 'lines'))


# LOADS ------------------
LOAD <- read_csv("~/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting/Data/LOAD.csv", 
                 col_types = cols(date = col_datetime(format = "%Y-%m-%d"), 
                                  id = col_character()))

LOAD %>%
  filter(site %in% CDsites) %>% 
  rename(plot = id) %>%
  mutate(year = year(date)) %>%
  left_join(TDplots, by = c("site", "plot", "year")) %>%
  mutate(dwm = ifelse(site == "AUGLA" & year == 2012 & date < "2012-06-18" & plot == "East", "CD", dwm)) %>%
  mutate(dwm = ifelse(site == "AUGLA" & year == 2012 & date < "2012-06-18" & plot == "West", "FD", dwm)) %>%
  filter(!is.na(dwm)) -> load

load %>%
  mutate(month = month(date)) %>%
  group_by(state, site, year, month, date, dwm) %>%
  summarise(WAT20 = mean(WAT20, na.rm = TRUE)) %>%       #average over DWM plots
  group_by(state, site, year, month, dwm) %>%
  summarise(WAT20 = sum(WAT20, na.rm = TRUE)) %>%        #sum over month   
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")) %>%
  mutate(date = format.Date(date, "%Y-%m")) -> load_monthly




load_monthly %>%
  group_by(state, site, year, dwm) %>%
  summarise(WAT20 = sum(WAT20, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key=dwm, value= WAT20) %>%
  mutate(CD = ifelse(is.na(CD), 0, CD),
         FD = ifelse(is.na(FD), 0, FD)) %>%
  gather(key=dwm, value=WAT20, CD:FD) %>%
  mutate(dwm = factor(dwm, levels=c("FD", "CD"))) %>%
  ggplot(aes(x=year, y=WAT20, fill=dwm)) +
  geom_bar(stat = "summary", fun.y = mean, na.rm = T, position = "dodge") +
  scale_fill_manual(values = c('#edb22a','#2d79b6'),
                    labels = c("Free Drainage","Controlled Drainage")) +
  scale_x_continuous(breaks = 2005:2016) +
  xlab(" ") + 
  ylab("Tile Nitrate-N Load, kg/ha") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 26, family = "Oswald Regular"),
        axis.text.y = element_text(size = 24, family = "Oswald Regular"),
        axis.text.x = element_text(size = 24, family = "Oswald Regular", angle = 90, vjust = 0.5),
        axis.ticks.y = element_line(),
        axis.line.y = element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, family = "Oswald Regular", margin = 3),
        legend.key.size = unit(2, 'lines'),
        panel.spacing = unit(3, 'lines'),
        plot.margin = unit(c(10,0,0,5), "pt"))



load_monthly %>%
  group_by(state, site, year, dwm) %>%
  summarise(WAT20 = sum(WAT20, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dwm = factor(dwm, levels=c("FD", "CD"))) %>%
  ggplot(aes(x=state, y=WAT20, fill=dwm)) +
  geom_bar(stat = "summary", fun.y = mean, na.rm = T, position = "dodge") +
  scale_fill_manual(values = c('#edb22a','#2d79b6'),
                    labels = c("Free Drainage","Controlled Drainage")) +
  xlab(" ") + 
  ylab("Tile Flow, mm") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 26, family = "Oswald Regular"),
        axis.text.y = element_text(size = 24, family = "Oswald Regular"),
        axis.text.x = element_text(size = 24, family = "Oswald Regular", angle = 90, vjust = 0.5),
        axis.ticks.y = element_line(),
        axis.line.y = element_line(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20, family = "Oswald Regular", margin = 3),
        legend.key.size = unit(2, 'lines'),
        panel.spacing = unit(3, 'lines'))









