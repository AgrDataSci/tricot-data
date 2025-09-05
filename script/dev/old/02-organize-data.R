# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("raster")
library("sf")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

load("processing/trial-data.rda")

# keep only the projects with data on it
keep = lapply(cmdata, function(x){
  length(x[["data"]]) > 1
})

keep = unlist(keep)

sum(keep) / length(keep)

# add the logic vector to the data with project info
projects$keep = keep

cmdata = cmdata[keep]

# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

projects$project_id

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))


# for the summaries here, I only use data from package distribution and participant registration
uniquenames = uniquenames[grep("id$|package_|registration_|_longitude|_latitude", uniquenames)]

uniquenames

# fix some codes that diverged over time
dat = lapply(dat, function(x){
  names(x)[names(x) == "registration_gender1"] = "registration_gender"
  names(x)[names(x) == "registration_biogender"] = "registration_gender"
  names(x)[names(x) == "registration_bioage"] = "registration_age"
  names(x)[names(x) == "registration_kebele"] = "registration_village"
  names(x)[names(x) == "registration_location"] = "registration_village"
  
  k1 = names(x) %in% uniquenames
  k2 = uniquenames %in% names(x)
  k = unique(c(names(x)[k1], uniquenames[k2]))
  
  x = x[, k]
  x
})

# put the data together 
dat = rowbind(dat)

# Coordinates
# get the coordinates as an independent data.frame
keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

dat = dat[, !keep]

lon = grep("_longitude", names(lonlat))
lon = lonlat[, lon]

lon = as.vector(apply(lon, 1, function(x){
  # I'll take the reverse as this increases the likelihood of 
  # getting the coordinates from the trial, not the point of 
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

lon[is.na(lon)] = "registration_pointofdelivery_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)], 
                    latitude = lonlat[cbind(1:nrow(lonlat), lat)])


lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

plot(lonlat)

# latitude higher than 23 is wrong
lonlat$longitude[lonlat$latitude > 23] = NA
lonlat$latitude[lonlat$latitude > 23] = NA

# also lonlat with 0, 0
lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA

plot(lonlat)

# fix technology names
unique(dat$package_technology)

dat$package_technology = ifelse(grepl("Groundnut", dat$package_technology), 
                                "Groundnut", 
                                dat$package_technology)

table(dat$package_technology)

# fix trial coordinator name
sort(unique(dat$package_coordinator))

dat$package_coordinator[dat$package_coordinator == "Fadhilidhili Kasubiri"] = "Fadhili Kasubiri"
dat$package_coordinator[dat$package_coordinator == "Fadhili kasubiri"] = "Fadhili Kasubiri"
dat$package_coordinator[dat$package_coordinator == "Fadhili and Sylvia"] = "Fadhili Kasubiri"
dat$package_coordinator[dat$package_coordinator == "HAPPY DAUDI"] = "Happy Daudi"

projects$coordinator[projects$coordinator == "Fadhilidhili Kasubiri"] = "Fadhili Kasubiri"
projects$coordinator[projects$coordinator == "Fadhili kasubiri"] = "Fadhili Kasubiri"
projects$coordinator[projects$coordinator == "Fadhili and Sylvia"] = "Fadhili Kasubiri"
projects$coordinator[projects$coordinator == "HAPPY DAUDI"] = "Happy Daudi"

# countries
unique(dat$package_country)

table(paste(dat$package_coordinator, dat$package_project_name, sep = " - "))

x = dat[dat$package_technology == "Groundnut" & dat$package_country == "TZ", ]

table(x$package_project_name)

x = x[x$package_project_name == "groundnut2" | x$package_project_name == "OPP1114827", ]

c("package|gender|_pos|_neg")


# ..................................
# get summaries ####
table(dat$package_technology, dat$package_country)

names(dat) = gsub("package_", "", names(dat))

dat %>% 
  group_by(country, technology, coordinator) %>% 
  summarise(N = length(technology)) %>% 
  ungroup() ->
  nplots

nplots

dir.create("output/", showWarnings = FALSE)

write.csv(nplots, "output/AVISA-oft-plots.csv", row.names = FALSE)


# filter the data and retain only some relevant variables
names(dat)

names(dat) = gsub("registration_", "", names(dat))

paste(names(dat), collapse = "', '")

sel = c('id', 'item_a','item_b','item_c',
        'project_name','technology',
        'coordinator','country', 'submitted_date',
        'age','gender','experiencecrop','education', 'civilstatus')

head(dat[sel])

dat = cbind(dat[sel], lonlat)

# export 
write.csv(dat, "output/tricot-avisa-registration-data.csv", row.names = FALSE)

# get some summaries

table(dat$gender)

dat$gender = ifelse(dat$gender == "Male", "Man", 
                    ifelse(dat$gender == "Female", "Woman", 
                           dat$gender))

dat$gender[dat$gender != "Man" & dat$gender != "Woman"] = NA

table(dat$gender)

# fix the age
dat$age = as.integer(dat$age)

boxplot.stats(dat$age)

dat$age[dat$age > 100] = NA

dat$age[dat$age < 20] = NA

boxplot(dat$age)

boxplot(dat$age ~ dat$gender, xlab = "Gender", ylab = "Age")

dat %>% 
  filter(is.na(age) == FALSE) %>%
  filter(is.na(gender) == FALSE) %>%
  ggplot(aes(x = gender, y = age)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Age") +
  theme_classic()

ggsave(filename = "output/farmer-age.png",
       width = 10,
       height = 12,
       units = "cm",
       dpi = 300)


# experience with the crop 

dat$experiencecrop = as.integer(dat$experiencecrop)

boxplot(dat$experiencecrop)

boxplot.stats(dat$experiencecrop)

dat$experiencecrop[dat$experiencecrop > 70] = NA

boxplot(dat$experiencecrop ~ dat$gender, xlab = "Gender", ylab = "Experience")


dat %>% 
  filter(is.na(experiencecrop) == FALSE) %>%
  filter(is.na(gender) == FALSE) %>%
  ggplot(aes(x = gender, y = experiencecrop)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Experience with the crop (years)") +
  theme_classic()

ggsave(filename = "output/farmer-experience.png",
       width = 10,
       height = 12,
       units = "cm",
       dpi = 300)




dat %>% 
  filter(is.na(experiencecrop) == FALSE) %>%
  filter(is.na(technology) == FALSE) %>%
  ggplot(aes(x = technology, y = experiencecrop)) +
  geom_boxplot() +
  labs(x = "Crop", y = "Experience with the crop (years)") +
  theme_classic()

ggsave(filename = "output/farmer-experience-by-crop.png",
       width = 15,
       height = 12,
       units = "cm",
       dpi = 300)


# proportion by gender and crop 
gendercrop = as.data.frame(table(dat$gender, dat$technology))
names(gendercrop)[1:2] = c("Gender", "Crop")

gendercrop %<>% 
  group_by(Crop) %>% 
  summarise(Freq = Freq / sum(Freq),
            Gender = Gender) %>% 
  ungroup()

gendercrop

ggplot(gendercrop, aes(x = Crop, 
                       y = Freq, 
                       fill = Gender)) +
  scale_fill_brewer(palette = "Set1") +
  geom_bar(stat = "identity") +
  labs(y = "Proportion by gender") +
  theme_classic() 


ggsave(filename = "output/farmer-sample-by-gender.png",
       width = 15,
       height = 12,
       units = "cm",
       dpi = 300)


# plot a map

# ADM data
# shape with adm units
adm = st_read("data/gadm/africa/africa_adm0.shp")
adm = st_as_sf(adm)
adm = adm[-c(2:3)]
adm

plot(adm)

coord = dat[,c("longitude", "latitude","technology")]
coord = na.omit(coord)
# 
# coord = st_as_sf(coord,
#                   coords = c("longitude", "latitude"), 
#                   crs = 4326)

ggplot() +
  geom_sf(adm$geometry, mapping = aes(), colour = "#4d4d4d", fill = NA) +
  geom_point(data = coord, aes(x = longitude,
                               y = latitude, 
                               colour = technology,
                               shape = technology)) +
  theme_void() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        legend.title = element_blank())

ggsave(filename = "output/map-tricot-plots.png",
       width = 15,
       height = 15,
       units = "cm",
       dpi = 450)
