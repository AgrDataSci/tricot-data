# ..........................................
# Prepare a dataset focussed on yield to BMGF
# ..........................................
# ..........................................
library("readxl")
library("tidyverse")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

dat = read_excel("data/raw/Data_EG.xlsx", sheet = 1)

longlat = strsplit(dat$pointofdelivery_reg, " ")

longlat = as.data.frame(do.call("rbind", longlat))

names(longlat) = c("lat", "long", "alt", "p")

map = plot_map(longlat, c("long", "lat"),  
         make.clusters = FALSE,
         map_provider = "OpenStreetMap.Mapnik")

mapview::mapshot(map, 
                 url = paste0("output/trial_map.html"),
                 file = paste0("output/trial_map.png"))

