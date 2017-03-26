library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(ggrepel)
library(maps)
library(maptools)


library(googlesheets)
library(lubridate)
library(stringr)
library(extrafont)

#import font Oswald
font_import(pattern = "[O/o]swald")
#look at the installed font list
fonts()
fonttable()
#load fonts to Windows device
loadfonts(device = "win")



setwd("C:/Users/Gio/Documents/Posters & Presentations/2017.03 - NCERA-ADMS Task Force Annual Meeting")
dir()

# GET DATA from GOOGLE ---------------------------------------------- 
# request authentication from Google
gs_ls()
# get googlesheet with TD site medatadata  
TD_metadata <- gs_ls("TD Site Metadata")
# Register a sheet
gap <- gs_title(as.character(TD_metadata[, 1]))    #gap <- gs_title("TD Site Metadata + History Master")
# open the sheet in browser 
gs_browse(gap)
# read the names of the sheets
gs_ws_ls(gap) -> sheets
# read headings
gap %>% 
  gs_read(ws = 1, range = cell_rows(1:3)) %>% 
  names() -> heading
# read data
gap %>% 
  gs_read_csv(ws = sheets, col_names = FALSE, skip = 2, na = "NAN") -> metaTD
# assign heading 
names(metaTD) <- heading
# select columns of interest
metaTD %>%
  select(UniqueID, `Lead PI`, `State PI`, `Drainage Retention Practice`, State, County, Latitude, Longitude) -> meta
# add column with full name of the state
meta$region <- str_to_lower(state.name[match(meta$State, state.abb)])
# add column with coordinates of the states' center
state.center %>%
  as.data.frame() %>%
  cbind(state.abb) %>%
  mutate(state.abb = as.character(state.abb)) %>%
  rename(State = state.abb, cent_long = x, cent_lat = y) %>%
  right_join(meta, by = "State") -> meta



# PLOT SITES on MAP -------------------------------------------------
map_data("state") %>%
  mutate(TDstate = ifelse(region %in% unique(meta$region), "#8CAAC8", "#F0F0F2")) -> all_states

ggplot() +
  geom_polygon(data = all_states, aes(x=long, y=lat, group = group, fill = I(TDstate)), 
               colour = "gray80") +
  coord_map(projection = "stereo") +
  theme_void() -> tdmap 


tdmap +
  geom_jitter(data = meta, aes(x=Longitude, y=Latitude), 
              #position=position_jitter(width=0.3, height=0.3),
              colour = "#FFAF1E", size = 1.5, alpha = 0.9)


for (i in 1:nrow(meta)) {
  print(meta$UniqueID[i])
  sitename <- meta$UniqueID[i]
  tdmap +
    geom_point(data = meta, aes(x=Longitude, y=Latitude), colour = "#FFC800", alpha = 0.8) +
    geom_point(data = meta[meta$UniqueID == sitename, ], 
               aes(x=Longitude, y=Latitude), colour = "#FFAB00", size = 7) +
    geom_text(data = meta[meta$UniqueID == sitename, ], 
              aes(x=Longitude, y=Latitude, label = UniqueID), 
              size = 12, face = "bold", colour = "#FFAA00", 
              family = "Oswald Regular",
              hjust=1.2, vjust=0.2)
  
  ggsave(filename = paste0(sitename, ".png"), 
         path = paste0(getwd(), "/Figures/Site_States/tdstates"),
         width = 10)
}


# sitename <- "BATH_A"
# tdmap +
#   geom_point(data = meta, aes(x=Longitude, y=Latitude), colour = "#FFC800", alpha = 0.8) +
#   geom_point(data = meta[meta$UniqueID == sitename, ], 
#              aes(x=Longitude, y=Latitude), colour = "#FFAB00", size = 7) +
#   geom_text(data = meta[meta$UniqueID == sitename, ], 
#             aes(x=Longitude, y=Latitude, label = UniqueID), 
#             size = 12, face = "bold", colour = "#FFAA00", 
#             family = "Oswald Regular",
#             hjust=1.2, vjust=0.2)
# 
# ggsave(filename = paste0(sitename, ".png"), 
#        path = paste0(getwd(), "/Figures/Site_States/tdstates"),
#        width = 10)


