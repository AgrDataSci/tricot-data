# load packages
library("sf")
library("janitor")
library("magrittr")
library("tidyverse")
library("readxl")
library("raster")
library("sf")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

dat = read_xlsx("data/dataForReport_Both_servers.xlsx")

names(dat) = make_clean_names(names(dat))

names(dat)

dat$number_of_registered_participants = as.integer(dat$number_of_registered_participants)

keep = !is.na(dat$number_of_registered_participants)

dat = dat[keep, ]

keep = dat$number_of_registered_participants > 25 

dat = dat[keep, ]

dat$crop = gsub("Not a crop |[(]|[)]", "", dat$crop)

dat$crop = tolower(dat$crop)

length(table(dat$crop))

# cropname = data.frame(name = names(table(dat$crop)), standard_name = "" )

#write.csv(cropname, "data/crop-tech-names-climmob.csv", row.names = FALSE)

cropname = read.csv("data/crop-tech-names-climmob.csv")

dat$cropname = NA

for (i in seq_along(cropname$name)) {
  dat$cropname[dat$crop == cropname$name[i]] = cropname$standard_name[i]
}

table(dat$cropname)


keep = !is.na(dat$cropname)

dat = dat[keep, ]

unique(dat$project_country)


dat$longitude = ifelse(is.na(dat$longitude), 0, dat$longitude)

dat$latitude = ifelse(is.na(dat$latitude), 0, dat$latitude)


keep = dat$longitude > -20 & dat$longitude < 55 & dat$latitude < 37

sum(keep)

dat = dat[keep, ]


keep = !grepl("chocolate|animal|eggplant",dat$crop)

dat = dat[keep, ]

write.csv(dat, "data/climmob-projects-partial-clean.csv")


# map 
lonlat = data.frame(long = dat$longitude, lat = dat$latitude)

lonlat$long[lonlat$long == 0] = NA

lonlat = na.omit(lonlat)

map = plot_map(lonlat, c("long", "lat"),
               map_provider = "OpenStreetMap.Mapnik")

map

mapview::mapshot(map, 
                 file = "output/trial-map.pdf")



# ADM data
# shape with adm units
adm = st_read("data/gadm/africa/africa_adm0.shp")
adm = st_as_sf(adm)
adm = adm[-c(2:3)]
adm

st_bbox(adm)

adm = st_crop(adm, st_bbox(c(xmin = -17.5, ymin = -34, 
                             xmax = 51,  ymax = 15)))


adm = adm[adm$ADMIN != "Madagascar", ]

plot(adm)

coord = dat[,c("longitude", "latitude","cropname")]

coord$longitude[coord$longitude == 0] = NA

coord$cropname[coord$cropname == ""] = "Others"

coord = na.omit(coord)

coord$technology = ClimMobTools:::.title_case(coord$cropname)

table(coord$technology)

ggplot() +
  geom_sf(adm$geometry,
          mapping = aes(), 
          colour = "#4d4d4d", 
          fill = "#f2f2f2") +
  geom_jitter(data = coord, aes(x = longitude,
                                y = latitude, 
                                color = technology),
              size = 1.8) +
  theme_void() +
  # scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a',
  #                               '#984ea3','#ff7f00','#999999',
  #                               '#a65628','#f781bf','#000000')) +
  theme(legend.position = "right",
        #legend.direction = "horizontal",
        legend.text = element_text(size = 12,
                                   colour = "grey20"),
        panel.background = element_blank(),
        plot.margin = unit(rep(1, 4), "mm"),
        legend.title = element_blank())

ggsave(filename = "output/map-tricot-plots.pdf",
       width = 20,
       height = 20,
       units = "cm")



# number of registered farmers over time 
dat$cropname[dat$cropname == ""] = NA

dat = dat[!is.na(dat$cropname), ]

dat$date = as_date(dat$project_date)

plot(dat$date, dat$number_of_registered_participants)

dat$year = year(dat$date)

data_entries = 
  dat %>% 
  group_by(year) %>% 
  summarise(N = sum(number_of_registered_participants))

data_entries$csum = cumsum(data_entries$N)


p = ggplot(data_entries) +
  geom_bar(stat = "identity", aes(x = year, y = csum), fill = "#33a02c") +
  ylim(0, 45000) +
  labs(x = "Year",
       y = "Cumulative N of registered participants") +
  theme_classic() +
  theme(text = element_text(size = 20))
p

ggsave(filename = "output/n-cumulative-participants.pdf",
       plot = p,
       width = 25,
       height = 20,
       units = "cm")



# by crop 
table(dat$cropname)





