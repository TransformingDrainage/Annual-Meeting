dpac <- read_csv("~/TD/Research Data/DPAC/DPAC_NE_20060701_20111222.csv", 
                                      col_types = cols(depth_f = col_skip()))

dpac %>%
  rename(depth = `Depth (mm)`) %>%
  filter(year(timestamp) == 2011) %>%
  mutate(timestamp = round_date(timestamp, unit = "hour")) %>%
  spread(key=plotid, value = depth) %>%
  head()



vsm <- read_excel("~/TD/Research Data/BEAR/TD_soil moisture.xlsx", 
                  sheet = 1,
                  col_types = c("date", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))

vsm2 <- read_excel("~/TD/Research Data/BEAR/TD_soil moisture.xlsx", 
                  sheet = 2,
                  col_types = c("date", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))


vsm %>%
  bind_rows(vsm2) -> vsm

str(vsm)

vsm %>%
  mutate(Date = round_date(Date)) %>%
  gather(key = depth, value = soil_moisture, 2:11) %>%
  mutate(location = word(depth), depth = word(depth, 2)) %>%
  mutate(depth = sub('*"', " ", depth)) %>%
  mutate(depth = as.numeric(depth)) %>%
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date, location, depth) %>%
  summarize_at(vars(soil_moisture), funs(mean)) %>%
  filter(!is.na(Date)) -> vsm_bear

vsm_bear %>% 
  mutate(depth = depth*2.54) %>%
  write.csv(file = "~/TD/Research Data/BEAR/daily_soil_moisture.csv")












flow_monthly %>%
  #filter(site == "SERF_SD") %>%
  #filter(year == 2015) %>%
  filter(site %in% c("SERF_IA", "HICKS_B", "DPAC", 
                     "MUDS2", "MUDS4", "SERF_SD", "STJOHNS")) %>%
  mutate(season = ifelse(month %in% c(1:3), "Jan-Mar", 
                          ifelse(month %in% c(4:6), "Apr-Jun",
                                 ifelse(month %in% c(7:9), "Jul-Sep", "Oct-Dec")))) %>%
  group_by(site, year, season, dwm) %>%
  summarise(flow = sum(WAT1, na.rm = T)) %>%
  group_by(site, season, dwm) %>%
  summarise(flow = mean(flow, na.rm = T)) %>%
  ungroup() %>%
  filter(site == "STJOHNS") %>%
  mutate(site = as.factor(site),
         season = factor(season, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), ordered = T),
         dwm = factor(dwm, levels = c("FD", "CD"), ordered = T)) %>%
  ggplot(aes(x=season, y=flow, fill = dwm)) +
  geom_bar(stat = "identity", na.rm = T, position = "dodge") +
  scale_fill_manual(values = c("blue", "red"))


