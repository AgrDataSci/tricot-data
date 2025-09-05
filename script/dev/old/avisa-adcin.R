library("terra")
library("geodata")
library("sf")
library("ggplot2")

load("processing/trial-data.rda")

techs = read.csv("data/crop-tech-names-climmob.csv")

adm = st_read("data/gadm/africa", "africa_adm0")

adm1 = gadm("TZA", level = 1, path = "data/gadm/")

adm1 = st_as_sf(adm1)

sort(adm1$NAME_1)

adm1$region = ifelse(adm1$NAME_1 == "Kagera" | adm1$NAME_1 == "Mwanza" | adm1$NAME_1 == "Mara" , "Lake",
                     ifelse(adm1$NAME_1 == "Kigoma" | adm1$NAME_1 ==  "Shinyanga" | adm1$NAME_1 == "Tabora" | adm1$NAME_1 == "Geita", "Western",
                            ifelse(adm1$NAME_1 == "Arusha" | adm1$NAME_1 == "Kilimanjaro" | adm1$NAME_1 == "Manyara" | adm1$NAME_1 == "Tanga" | adm1$NAME_1 == "Simiyu", "Northern",
                                   ifelse(adm1$NAME_1 == "Singida" | adm1$NAME_1 =="Dodoma", "Central",
                                          ifelse(adm1$NAME_1 == "Rukwa" | adm1$NAME_1 == "Mbeya" | adm1$NAME_1 == "Iringa" | adm1$NAME_1 == "Katavi" | adm1$NAME_1 == "Songwe" | adm1$NAME_1 == "Njombe", "Southern Highlands",
                                                 ifelse(adm1$NAME_1 == "Lindi" | adm1$NAME_1 == "Mtwara" | adm1$NAME_1 == "Ruvuma", "Southern",
                                                        "Eastern"))))))


z = data.frame(cbind(adm1$NAME_1, adm1$region))

z = z[order(z$X2), ]

output = "output/avisa-adcin/"

dir.create(output, showWarnings = FALSE, recursive = TRUE)

look = "technology|package_country|registration_survey_start|longitude|latitude"

dat = lapply(dat, function(x){
  x[,grep(look, names(x))]
})

techs = techs[!duplicated(techs$name), ]

techs$standard_name[techs$standard_name == "beans"] = "bean"

techs$standard_name[techs$standard_name == "millet"] = "pearl millet"

for(i in seq_along(techs$name)) {
  dat = lapply(dat, function(x){
    x$package_technology = tolower(x$package_technology)
    x$package_technology[x$package_technology == techs$name[i]] = techs$standard_name[i]
    x
  })
}

keep = unlist(lapply(dat, function(x) x$package_technology[1]))

sort(unique(keep))

crops = c("pearl millet", "sorghum", "bean",
          "cowpea", "pigeonpea", "groundnut", "potato")

keep = keep %in% crops

dat = dat[keep]

# no older than 2021
keep = unlist(lapply(dat, function(x) x$registration_survey_start[1]))

keep = as.Date(keep) > 2020

dat = dat[keep]

unlist(lapply(dat, nrow))

dat = dat[-c(29, 55)]

# now put the GPS all together
# Coordinates ####
# get the coordinates as an independent data.frame
for(i in seq_along(dat)){
  
  X = dat[[i]]
  
  rownames(X) = 1:nrow(X)
  
  keep = grepl("_longitude|_latitude", names(X))
  
  lonlat = X[, keep]
  
  X = X[, !keep]
  
  lon = grep("_longitude", names(lonlat))
  lon = lonlat[, lon]
  
  lon = as.vector(apply(lon, 1, function(y){
    # I'll take the reverse as this increases the likelihood of
    # getting the coordinates from the trial, not the point of
    # delivery
    names(y)[rev(which(!is.na(y)))[1]]
  }))
  
  lon[is.na(lon)] = "registration_longitude"
  
  lat = gsub("_longitude", "_latitude", lon)
  
  # keep only the selected columns, one per plot
  lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                      latitude = lonlat[cbind(1:nrow(lonlat), lat)])
  
  
  lonlat[1:2] = lapply(lonlat[1:2], as.numeric)
  
  nas = lonlat$longitude == 0 & lonlat$latitude == 0 & !is.na(lonlat$longitude)
  
  lonlat[nas, ] = NA
  
  X = cbind(lonlat, X)
  
  keep = !grepl("gps_precision|elevation", names(X))
  
  X = X[keep]
  
  dat[[i]] = X
}


#dat = do.call("rbind", dat)

dat = gosset::rowbind(dat)

dat$year = format(as.Date(dat$registration_survey_start), "%Y")

sort(table(dat$package_country))

sort(adm$ADM0_A3)

adm$ADM0_A2 = substr(adm$ADM0_A3, 1, 2)

dat$package_country[is.na(dat$package_country)] = "RW"

dat$group = paste(dat$package_technology, dat$package_country, sep = "-")

table(dat$group)

groups = sort(unique(dat$group))

for (i in seq_along(groups)) {
  X = dat[dat$group == groups[i], ]
  
  cr = crop_spam(X$package_technology[1], "prod", path = output, africa = TRUE)
  cr = cr[[1]]
  
  ad = adm[adm$ADM0_A2 == X$package_country[1], ]
  
  cr = crop(cr, ad)
  
  cr = mask(cr, ad)
  
  r = as.data.frame(cr, xy = TRUE)
  r = r[!is.na(r[, 3]), ]
  names(r)[3] = "layer"
  
  p = ggplot() +
    geom_sf(ad$geometry, mapping = aes(), colour = "black", fill = "#f7fcf0") +
    #geom_sf(adm1, mapping = aes(fill = region, geometry = geometry), show.legend = T, alpha = 0.3) +
    #geom_sf_label(adm1, mapping = aes(label = NAME_1)) +
    geom_tile(r, mapping = aes(x = x, y = y, fill = layer), alpha = 0.5) +
    geom_jitter(data = X, aes(x = longitude,
                              y = latitude, 
                              color = year), size = 1.5) +
    scale_color_brewer(palette = "Set1" ) + 
    #scale_fill_brewer(palette = "Pastel1") +
    scale_fill_gradient(low = "#c7e9b4", high = "#081d58") +
    theme_void() + 
    theme(legend.position = "right",
          legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 15)) +
    labs(title = paste(X$package_technology[1], X$package_country[1]))
  p
  ggsave(paste0(output, X$package_technology[1], X$package_country[1], ".pdf"),
         p,
         width = 20,
         height = 20,
         units = "cm")
}



