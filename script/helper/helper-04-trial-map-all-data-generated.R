# ..........................................
# Prepare map with tricot trial location
# ..........................................
# ..........................................
library("tidyverse")
library("readxl")
library("raster")
library("sf")
library("geodata")
library("gosset")
library("ClimMobTools")
# additional functions from ClimMob-analysis 
# 
# list.files("processing", full.names = TRUE)
# list.files("data/raw", full.names = TRUE)
# 
# load("processing/trial-data.rda")
# d = list()
# for(i in 1:length(cmdata)) {
#   print(i)
#   x = as.data.frame(cmdata[[i]], tidynames = TRUE, pivot.wider = TRUE)
#   
#   if(nrow(x) < 10) {
#     x = data.frame()
#     next
#   }
#   
#   x = x[grep("longitude|latitude", names(x))]
#   
#   d[[i]] = x
#   
# }
# 
# d2 = d
# 
# for(i in seq_along(d)){
# 
#   lonlat = d[[i]]
#   
#   names(lonlat)
#   
#   lon = grep("_longitude", names(lonlat))
#   lon = lonlat[, lon]
#   
#   names(lon)
#   
#   lon = as.vector(apply(lon, 1, function(x){
#     # I'll take the reverse as this increases the likelihood of
#     # getting the coordinates from the trial, not the point of
#     # delivery
#     names(x)[rev(which(!is.na(x)))[1]]
#   }))
#   
#   lon
#   
#   lon[is.na(lon)] = unique(lon[!is.na(lon)])[1]
#   
#   
#   lat = gsub("_longitude", "_latitude", lon)
#   
#   rownames(lonlat)= 1:nrow(lonlat)
#   
#   # keep only the selected columns, one per plot
#   lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
#                       latitude = lonlat[cbind(1:nrow(lonlat), lat)])
#   
#   lonlat
#   
#   lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
#   
#   d[[i]] = lonlat
#   
# }
# 
# d = d[1:i]
# 
# d = rowbind(d)
# 
# d$technology = ""
# d$country = ""
# 
# d$longitude[d$longitude < 0] = NA

load('raw/trial-data.rda')

# first fix technology names
techs = lapply(cmdata, function(x) {
  data.frame(id = x$project$project_id,
             technology = tolower(x$combination$elements[[1]]$technology_name),
             crop_name = "",
             desc = x$project$project_abstract)
})

techs = do.call(rbind, techs)


techs$crop_name[grepl("Soybean", techs$desc)] = "soybean"
techs$crop_name[grepl("IP", techs$desc)] = "potato"
techs$crop_name[grepl("maize", techs$desc)] = "maize"
techs$crop_name[grepl("Sorghum", techs$desc)] = "sorghum"
techs$crop_name[grepl("wheat", techs$desc)] = "wheat"
techs$crop_name[grepl("cover crops", techs$desc)] = "forages"
techs$crop_name[grepl("Climbing beans |Bush beans ", techs$desc)] = "commonbean"
techs$crop_name[grepl("maize", techs$technology)] = "maize"
techs$crop_name[grepl("amaranth", techs$technology)] = "amaranth"
techs$crop_name[grepl("bean", techs$technology)] = "commonbean"
techs$crop_name[grepl("cowpe", techs$technology)] = "cowpea"
techs$crop_name[grepl("pepper", techs$technology)] = "chilipepper"
techs$crop_name[grepl("tomato", techs$technology)] = "tomato"
techs$crop_name[grepl("drc variet", techs$technology)] = "cassava"
techs$crop_name[grepl("cassava", techs$technology)] = "cassava"
techs$crop_name[grepl("nextgen", techs$technology)] = "cassava"
techs$crop_name[grepl("sorghum", techs$technology)] = "sorghum"
techs$crop_name[grepl("groundnut", techs$technology)] = "groundnut"
techs$crop_name[grepl("banana|plantain", techs$technology)] = "banana"
techs$crop_name[grepl("ethiopiagrass|forage", techs$technology)] = "forages"
techs$crop_name[grepl("potato", techs$technology)] = "potato"
techs$crop_name[grepl("sweetpotato", techs$technology)] = "sweetpotato"
techs$crop_name[grepl("jute", techs$technology)] = "jutemallow"
techs$crop_name[grepl("durum", techs$technology)] = "durumwheat"
techs$crop_name[grepl("eggplant", techs$technology)] = "eggplant"
techs$crop_name[grepl("millet", techs$technology)] = "millets"
techs$crop_name[grepl("barley", techs$technology)] = "barley"
techs$crop_name[grepl("millet", techs$technology)] = "millets"
techs$crop_name[grepl("chick", techs$technology)] = "chickpea"
techs$crop_name[grepl("barley", techs$technology)] = "barley"
techs$crop_name[grepl("okra", techs$technology)] = "okra"
techs$crop_name[grepl("pigeon", techs$technology)] = "pigeonpea"
techs$crop_name[techs$crop_name == ""] = "others"

d = data.frame()

out = data.frame()

for(i in seq_along(cmdata)){

  x = cmdata[[i]]

  index = which(techs$id %in% x$project$project_id)
  
  block = try(exportBlockData(x), silent = TRUE)
  
  if("try-error" %in% class(block)) {
    out = rbind(out, techs[index, ])
    next
  }
  
  xy = try(block[,c("longitude", "latitude")], silent = TRUE)
  
  if("try-error" %in% class(xy)) {
    out = rbind(out, techs[index, ])
    next
  }
  
  xy$technology = techs$crop_name[index]
  
  xy$country = ClimMobTools:::.safe_extract(x, c("project", "project_cnty"), default = NA)[1]
  
  d = rbind(d, xy)
    
}

x = read.csv("raw/tricot-data-long.csv")
names(x)
x = x[ ,c("crop_name", "longitude", "latitude", "country")]
x = x[x$country!="TZ", ]
names(x)[1] = "technology"
x$technology[grepl("bean", x$technology)] = "commonbean"
x$technology[grepl("millet", x$technology)] = "millets"
unique(x$technology)

x2 = read.csv("raw/nextgen-uganda.csv")
x2 = x2[,c("registration_farm_geo_longitude", "registration_farm_geo_latitude", "registration_survey_start")]
names(x2) = c("longitude", "latitude","registration_date")
x2$technology = "cassava"
x2$country = "UG"

x3 = read.csv("raw/nextgen-tanzania.csv")
x3 = x3[ ,c("technology", "longitude", "latitude")]
x3$country = "TZ"

x4 = read.csv("raw/seetpotato_data.csv")
names(x4) 
x4 = x4[,c("registration_biogps_longitude", "registration_biogps_latitude")]
names(x4) = c("longitude", "latitude")
x4$technology = "sweetpotato"
x4$country = "GH"

x5 = read_excel("raw/Sweetpotato_PVS_Farmer_Selection_2021_final_Nov_10.xlsx")
names(x5)
x5 = x5[,c("GPS_farmersfield_longitude", "GPS_farmersfield_latitude")]
names(x5) = c("longitude", "latitude")
x5$technology = "sweetpotato"
x5$country = "UG"

x6 = read_excel("raw/pvsfarmerselection_gpsfield_2021.xls")
names(x6)
x6 = x6[,c("x", "y")]
names(x6) = c("longitude", "latitude")
x6$technology = "sweetpotato"
x6$country = "UG"

x7 = read.csv("raw/rice-india-tricot/data/rice.csv")
x7 = x7[c("lon", "lat")]
x7$technology = "rice"
x7$country = "IN"
names(x7)[1:2] = c("longitude", "latitude")

x8 = read.csv("raw/bean-central-america/data.csv")

x9 = read.csv("raw/wheat-india/wheat_data.csv")
x9 = x9[c("lon", "lat")]
x9$technology = "wheat"
x9$country = "IN"
names(x9)[1:2] = c("longitude", "latitude")

x10 = read.csv("raw/red_beans_data.csv")
x10 = x10[c("lon", "lat")]
x10$technology = "commonbean"
x10$country = ""
names(x10)[1:2] = c("longitude", "latitude")

x11 = read.csv("raw/Report_data_forageLao.csv")
x11$technology = "forages"

x12 = data.frame(longitude = 46.9, latitude = -19.6, technology = "rice")

dat = rowbind(list(d, x, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))

dat$longitude[dat$longitude==0] = NA
dat$latitude[dat$latitude==0] = NA
dat$latitude[dat$latitude > 70] = NA

dat = dat[!is.na(dat$longitude) & !is.na(dat$latitude), ]

dat = dat[,c("technology","country", "latitude", "longitude")]

dat$network = ""

sort(unique(dat$technology))

dat$network[grepl("sweetpotato|cassava|potato|banana", dat$technology)] = "RTB"

dat$network[grepl("amaranth|chilip|eggpla|jutema|okra|tomato", dat$technology)] = "Vegetables"

dat$network[grepl("wheat", dat$technology)] = "Wheat"

dat$network[grepl("rice", dat$technology)] = "Rice"

dat$network[grepl("millet|chick|pigeon|cowpea|ground|sorgh", dat$technology)] = "Dryland Crops"

dat$network[grepl("forage", dat$technology)] = "Forages"

dat$network[grepl("commonbean", dat$technology)] = "Beans"

dat$network[grepl("maize", dat$technology)] = "Maize"

dat$network[dat$network == ""] = "Others"

table(dat$technology, dat$network)

dat[dat$latitude > 25 & dat$technology == "maize", ] = NA

# plot a map

# load world administrative boundaries
adm = world(path = "docs")

# convert to simple features format
adm = st_as_sf(adm)

# remove antarctica to clean the map
adm = adm[adm$NAME_0 != "Antarctica", ]

# select relevant columns: longitude, latitude, and crop name
coord = dat[c("longitude", "latitude","network")]

table(coord$network)


# remove rows with missing values
coord = na.omit(coord)

table(coord$network)

trialmap =
  ggplot() +
  geom_sf(data = adm, aes(geometry = geometry),
          colour = "#4d4d4d", fill = "grey99") +
  geom_jitter(data = coord,
              aes(x = longitude,
                  y = latitude,
                  shape = network,
                  fill  = network),   # fill depends on crop
              size = 2,
              colour = "black") +     # border always black
  theme_void() +
  scale_shape_manual(values = rep(21:25, length.out = length(unique(coord$network)))) +
  scale_fill_brewer(palette = "Set3") +  # nice qualitative palette
  theme(legend.position = c(0.1, 0.5),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(1,1,1,1), "mm"),
        legend.title = element_blank())



# save map to file as png
ggsave("docs/tricot-trials.png",
       plot = trialmap,
       width = 36,
       height = 18,
       units = "cm",
       dpi = 400)

