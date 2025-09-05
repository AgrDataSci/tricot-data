# ..........................................
# Prepare map with tricot trial location
# ..........................................
# ..........................................
library("tidyverse")
library("readxl")
library("raster")
library("sf")
library("geodata")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")


list.files("processing", full.names = TRUE)
list.files("data/raw", full.names = TRUE)

load("processing/trial-data.rda")
d = list()
for(i in 1:length(cmdata)) {
  print(i)
  x = as.data.frame(cmdata[[i]], tidynames = TRUE, pivot.wider = TRUE)
  
  if(nrow(x) < 10) {
    x = data.frame()
    next
  }
  
  x = x[grep("longitude|latitude", names(x))]
  
  d[[i]] = x
  
}

d2 = d

for(i in seq_along(d)){

  lonlat = d[[i]]
  
  names(lonlat)
  
  lon = grep("_longitude", names(lonlat))
  lon = lonlat[, lon]
  
  names(lon)
  
  lon = as.vector(apply(lon, 1, function(x){
    # I'll take the reverse as this increases the likelihood of
    # getting the coordinates from the trial, not the point of
    # delivery
    names(x)[rev(which(!is.na(x)))[1]]
  }))
  
  lon
  
  lon[is.na(lon)] = unique(lon[!is.na(lon)])[1]
  
  
  lat = gsub("_longitude", "_latitude", lon)
  
  rownames(lonlat)= 1:nrow(lonlat)
  
  # keep only the selected columns, one per plot
  lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                      latitude = lonlat[cbind(1:nrow(lonlat), lat)])
  
  lonlat
  
  lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
  
  d[[i]] = lonlat
  
}

d = d[1:i]

d = rowbind(d)

d$technology = ""
d$country = ""

d$longitude[d$longitude < 0] = NA


x = read.csv("data/tricot-data-long.csv")
names(x)
x = x[ ,c("crop_name", "longitude", "latitude", "country")]
names(x)[1] = "technology"


x2 = read.csv("data/raw/nextgen-uganda.csv")
x2 = x2[,c("registration_farm_geo_longitude", "registration_farm_geo_latitude", "registration_survey_start")]
names(x2) = c("longitude", "latitude","registration_date")
x2$technology = "cassava"
x2$country = "UG"

x3 = read.csv("data/raw/nextgen-tanzania.csv")
x3 = x3[ ,c("technology", "longitude", "latitude")]
x3$country = "TZ"

x4 = read.csv("data/raw/seetpotato_data.csv")
names(x4) 
x4 = x4[,c("registration_biogps_longitude", "registration_biogps_latitude")]
names(x4) = c("longitude", "latitude")
x4$technology = "sweetpotato"
x4$country = "GH"

x5 = read_excel("data/raw/Sweetpotato_PVS_Farmer_Selection_2021_final_Nov_10.xlsx")
names(x5)
x5 = x5[,c("GPS_farmersfield_longitude", "GPS_farmersfield_latitude")]
names(x5) = c("longitude", "latitude")
x5$technology = "sweetpotato"
x5$country = "UG"

x6 = read_excel("data/raw/pvsfarmerselection_gpsfield_2021.xls")
names(x6)
x6 = x6[,c("x", "y")]
names(x6) = c("longitude", "latitude")
x6$technology = "sweetpotato"
x6$country = "UG"

x7 = read.csv("data/rice-india-tricot/data/rice.csv")
x7 = x7[c("lon", "lat")]
x7$technology = "rice"
x7$country = "IN"
names(x7)[1:2] = c("longitude", "latitude")

x8 = read.csv("data/bean-central-america/data.csv")

x9 = read.csv("data/wheat-india/wheat_data.csv")
x9 = x9[c("lon", "lat")]
x9$technology = "wheat"
x9$country = "IN"
names(x9)[1:2] = c("longitude", "latitude")

# norway and laos 
x10 = data.frame(longitude = c(5.3967953,
                               5.7174933,
                               6.4339196,
                               5.60332104,
                               5.6911785,
                               5.8169552,
                               5.6911785,
                               5.6864513,
                               7.781496,
                               5.3401123,
                               6.1065858,
                               6.6202796,
                               6.0470237,
                               6.7884989,
                               10.686855,
                               9.7569797,
                               9.7776454,
                               8.6725137),
                 latitude = c(59.5256129,
                              59.4951234,
                              59.4641332,
                              58.68936225,
                              58.5683827,
                              59.1712155,
                              58.5683827,
                              59.0425867,
                              58.93723667,
                              60.2675248,
                              61.9251951,
                              61.7096732,
                              61.9114451,
                              62.0773894,
                              62.0797145,
                              62.9874715,
                              63.1798677,
                              63.3313224))

x11 = read.csv("data/Report_data_forageLao.csv")

dat = rowbind(list(d, x, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))

dat$longitude[dat$longitude==0] = NA
dat$latitude[dat$latitude==0] = NA
dat$latitude[dat$latitude > 70] = NA

dat = dat[!is.na(dat$longitude) & !is.na(dat$latitude), ]

# plot a map

# # ADM data
# # shape with adm units
# adm = st_read("data/gadm/africa/africa_adm0.shp")
# adm = st_as_sf(adm)
# adm = adm[-c(2:3)]
# adm

adm = world(path = "data")

adm = st_as_sf(adm)

adm = adm[adm$NAME_0 != "Israel", ]

adm = adm[adm$NAME_0 != "Antarctica", ]

coord = dat[,c("longitude", "latitude")]
coord = na.omit(coord)

p = ggplot() +
  geom_sf(adm$geometry,
          mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#f7fcfd") +
  geom_jitter(data = coord, aes(x = longitude,
                               y = latitude),
             size = 1) +
  theme_void() +
  theme(legend.position = "right",
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12,
                                   colour = "grey20"),
        panel.background = element_blank(),
        plot.margin = unit(rep(1, 4), "mm"),
        legend.title = element_blank())

ggsave(filename = "output/map-tricot-plots.pdf",
       width = 30,
       height = 15,
       units = "cm")

ggsave(filename = "output/map-tricot-plots.png",
       width = 30,
       height = 15,
       units = "cm",
       dpi = 300)

# a zoom in Tanzania
keep = dat$country %in% c("TZ", "UG", "KE", "RW")
coord = dat[keep , ]
coord = coord[,c("longitude", "latitude","technology")]
coord = na.omit(coord)

coord = coord[coord$technology != "potato", ]

coord$technology = ClimMobTools:::.title_case(coord$technology)

table(coord$technology)

summary(coord)

adm = st_crop(adm, st_bbox(c(xmin = 28.8, ymin = -12, 
                             xmax = 41,  ymax = 4)))

ggplot() +
  geom_sf(adm$geometry,
          mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#ffffd9") +
  geom_jitter(data = coord, aes(x = longitude,
                                y = latitude, 
                                color = technology),
              size = 1) +
  theme_void() +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a',
                                '#984ea3','#ff7f00','#999999',
                                '#a65628','#f781bf','#000000')) +
  theme(legend.position = "right",
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12,
                                   colour = "grey20"),
        panel.background = element_blank(),
        plot.margin = unit(rep(1, 4), "mm"),
        legend.title = element_blank())

ggsave(filename = "output/map-tricot-plots-east-africa.pdf",
       width = 15,
       height = 15,
       units = "cm")


