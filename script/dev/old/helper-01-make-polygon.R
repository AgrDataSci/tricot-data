library("sf")
library("rgdal")
library("rgeos")
library("raster")
library("maptools")
library("tidyverse")
library("magrittr")


adm <- readOGR(dsn = "/Users/kauedesousa/local-workflow/geo-raster-shapefile/world_borders/", layer = "world_borders_adm0")

adm <- adm[adm$CONTINENT %in% "Africa", ]

# pick focal columns 
adm@data <- adm@data[,c("ADMIN","ADM0_A3", "GEOUNIT","CONTINENT")]

head(adm)

plot(adm)

# set a new bbox to remove countries areas outside Europe
e <- as(raster::extent(-17.5, 51, -35, 25), "SpatialPolygons")
proj4string(e) <- proj4string(adm)

plot(e, add = TRUE)

# intersect
adm <- raster::intersect(adm, e)

plot(adm)

dir.create("data/gadm/africa", recursive = TRUE)

writeOGR(adm, 
         dsn = "data/gadm/africa",
         layer = "africa_adm0",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
