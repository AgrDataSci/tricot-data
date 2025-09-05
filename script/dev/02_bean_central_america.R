# ..........................................
# ..........................................
# Read and clean bean data in Central America
# ..........................................
# ..........................................
library("janitor")
library("readr")
library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("sf")
library("sp")
library("rgeos")
library("rgdal")
source("script/helper01_functions.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/01_organize_bean_central_america.txt")


#.....................................
#.....................................
# Read data ####
here <- "data/raw/bean_central_america/"

trials <- list.dirs(here, full.names = FALSE)[-1]

# read the files to check the list of traits
namestraits <- list()
for(i in seq_along(trials)) {
  
  lf <- list.files(paste0(here,trials[i]))
  
  # read trial data
  # some files has an mirror with geo coordinates, check if the file exists and use it
  # otherwise take the original one
  if (paste0(trials[i],"_coords.csv") %in% lf) {
    dt <- read_csv(paste0(here, trials[i], "/", trials[i],"_coords.csv"),
                   na = c("ERROR"," ","", "NA","undefined"), 
                   locale = locale(encoding = "UTF-8"))
  }else{
    dt <- read_csv(paste0(here, trials[i],"/",trials[i],".csv"),
                   na = c("ERROR"," ","", "NA","undefined"),
                   locale = locale(encoding = "UTF-8"))
  }
  
  traits <- as.vector(unlist(dt[1,]))
  
  traits <- traits[which(grepl("(A/B/C)", traits))]
  
  namestraits[[i]] <- traits
  
}


traits <- unique(unlist(namestraits))

# select the traits that will remain
# and drop some traits difficult to explain/standardize 
drop <- c("Altitud_de_la_parcela_(msnm)-(A/B/C)", 
          "Sanidad-(A/B/C)",
          "Abono_foliar-(A/B/C)", 
          "Fungicidas-(A/B/C)",                
          "Bioinsumos-(A/B/C)")

traits <- traits[!traits %in% drop]

# now make a standard name for all traits
# put in a data frame
newtraits <- c("vigor", "architecture",
               "maturity", "resistance_to_pests",
               "resistance_to_diseases", "tolerance_to_drought",
               "yield", "marketability", "taste",
               "overall_appreciation", "germination",
               "architecture", "maturity", "tolerance_to_drought",
               "marketability", "taste")

traitsclean <- make_clean_names(traits)

traits <- data.frame(traits, traitsclean, newtraits)

traits

# a vector with patterns to look at the cols 
cols <- c("localizacion_longitud$", "localizacion_latitud$",
          "fecha_de_siembra", "fecha_de_cosecha", "pueblo", "distrito", 
          "genero_m_f", "edad$")

# a vector to rename the cols
colsnew <- c("lon","lat","planting_date","harvesting_date","adm3","adm1","gender", "age")

# overall vs local
ovl <- "desempeno_general_vs_local_mejor_peor"

# a list to keep the data frames
dtl <- list()

for(i in seq_along(trials)) {
  
  lf <- list.files(paste0(here, trials[i]))
  
  # read package data
  pkg <- read_csv(paste0(here, trials[i], "/paquete_", trials[i],".csv"),
                  na = c("ERROR"," ","", "NA"),
                  locale = locale(encoding = "UTF-8"))
  
  names(pkg) <- c("package", paste0("variety_", letters[1:3]))
  
  # read trial data
  # some files has an mirror with geo coordinates, check if the file exists and use it
  # otherwise take the original one
  if (paste0(trials[i],"_coords.csv") %in% lf) {
    dt <- read_csv(paste0(here, trials[i],"/", trials[i], "_coords.csv"),
                   na = c("ERROR"," ","", "NA","undefined"), 
                   locale = locale(encoding = "UTF-8"))
  }else{
    dt <- read_csv(paste0(here, trials[i], "/", trials[i], ".csv"),
                   na = c("ERROR"," ","", "NA","undefined"),
                   locale = locale(encoding = "UTF-8"))
  }
  
  names(dt)[names(dt) == "codigo_paquetes"] <- "package"
  
  # merge then by package code
  dt <- merge(dt, pkg, by = "package", all.x = TRUE)
  
  # make clean names using janitor
  names(dt) <- make_clean_names(names(dt))
  
  # look for the indices of target columns
  index <- unlist(sapply(cols, function(x){
    which(grepl(x, names(dt)))
  })) 
  
  nm <- make_clean_names(unlist(dt[1,]))
  
  chars_i <- integer()
  whichtrait <- character()
  
  for (k in seq_along(traits$traitsclean)) {
    
    wheretrait <- which(nm %in% traits$traitsclean[k])
    
    if (length(wheretrait) != 1) {next}
    
    chars_i <- c(chars_i, 
                 c(wheretrait + 1, 
                   wheretrait + 2))
    
    whichtrait <- c(whichtrait, traits$newtraits[k])
    
  }
  
  # get the indices for overall vs local
  if(length(ovl) == 1){
    ovl_i <- which(grepl(ovl, nm)) + c(1:3)
    chars_i <- c(chars_i, ovl_i)
  }  
  
  # get the indices for the packages and varieties
  pkg <- which(names(dt) %in% c("package", paste0("variety_", letters[1:3])))
  
  # keep only target indices
  dt <- dt[, c(pkg, index, chars_i)]
  
  # rename cols
  names(dt) <- c(c("package_id", paste0("variety_", letters[1:3])), 
                 colsnew,
                 paste0(rep(whichtrait, each = 2), c("_best","_worst")),
                 paste0("overall_vs_local_", paste0("variety_", letters[1:3])))
  
  dt <- cbind(trial = trials[i], dt)
  
  dtl[[i]] <- dt 
  
}

dt <- rowbind(dtl)

# ..........................................
# ..........................................
# ..........................................
# Repair varieties names ####
vars <-  paste0("variety_", letters[1:3])

sort(unique(unlist(dt[vars])))

dt[vars] <- lapply(dt[vars], function(x){
  x[x == "Cabecar"] <- "Amadeus 77"
  x[x == "Cabécar"] <- "Amadeus 77"
  x[x == "CabÃ©car"] <- "Amadeus 77"
  x[x == "ALS 0536-6"] <- "ALS 0532-6"
  x[x == "Honduras Nutriva"] <- "Honduras Nutritiva"
  x[x == "cod 18 - Cabecar"] <- "Amadeus 77"
  x[x == "MCH 2-13-49"] <- "MHC 2-13-49"
  x[x == "BRT 103- 182"] <- "BRT 103-182"
  x[x == "INTA Rojo (testigo)"]<-"Amadeus 77"
  x[x == "PM2- Don Rey"] <- "IBC 302-29"
  x[x == "INTA Sequia"] <- "SX 14825-7-1"
  x[x == "INTA Rojo"] <- "Amadeus 77"
  x[x == "PM2-Don Rey"] <- "IBC 302-29"
  x[x == "PM2 Don Rey"] <- "IBC 302-29"
  x[x == "Amilcar"] <- "Amilcar 58"
  x[x == "SJC"] <- "SJC 730-79"
  x[x == "Campechano"] <- "SX 14825-7-1"
  x[x == "Don Rey"] <- "IBC 302-29"
  x[x == "Cedrón"] <- "Cedron"
  x[x == "CedrÃ³n"] <- "Cedron"
  x[x == "Campechano"] <- "SX 14825-7-1"
  x[x == "don Rey"] <- "IBC 302-29"
  x[x == "cod 02 - Amadeus 77"] <- "Amadeus 77"
  x[x == "Honduras Nutritiva"] <- "Honduras Nutritivo"
  x[x == "SJC-730-79"] <- "SJC 730-79"
  return(x)
})

sort(unique(unlist(dt[vars])))

length(sort(unique(unlist(dt[vars]))))

# remove rows with no variety info
keep <- apply(dt[,vars], 1, function(x){
  sum(is.na(x)) < 1
})

dt <- dt[keep, ]

sort(unique(unlist(dt[vars])))

# ..........................................
# ..........................................
# ..........................................
# Check and clean planting and harvest dates ####
dates <- c("planting_date","harvesting_date")
unique(dt[, dates[1]])
unique(dt[, dates[2]])
sum(is.na(dt$planting_date))

# three different formats of dates, 
# dd/mm/yyyy and mm/dd/yyyy and yyyy/mm/dd
# try to solve by applying the default format and then 
# if NA, try with the others
dt$planting_date <- sapply(dt$planting_date, function(x){
  
  y <- as.Date(x, format = "%Y-%m-%d")
  
  if (is.na(y)) {
    y <- as.Date(as.integer(x), origin = "1970-01-01")
  }
  
  if (is.na(y)) {
    y <- as.Date(x, format = "%d/%m/%Y")
  }
  if (is.na(y)) {
    y <- as.Date(x, format = "%m/%d/%Y") 
  }
  return(as.integer(y))
})

dt$harvesting_date <- sapply(dt$harvesting_date, function(x){
  y <- as.Date(x, format = "%Y-%m-%d")
  
  if (is.na(y)) {
    y <- as.Date(as.integer(x), origin = "1970-01-01")
  }
  if(is.na(y)){
    y <- as.Date(x, format = "%d/%m/%Y")
  }
  if(is.na(y)){
    y <- as.Date(x, format = "%m/%d/%Y") 
  }
  return(as.integer(y))
})

sum(is.na(dt$planting_date))

boxplot(dt$planting_date ~ dt$trial)

boxplot(dt$harvesting_date ~ dt$trial)

# remove outliers in planting date
# some points are out the median by project
# we define a threshold of -40 and +40 from the median of each project
# return the median per project for values outside this threshold
for(i in seq_along(trials)){
  
  planting_date_i <- dt$planting_date[dt$trial == trials[i]]
  
  replc <- median(planting_date_i, na.rm = TRUE)
  
  r <- planting_date_i > median(planting_date_i, na.rm = TRUE) + 40 | 
    planting_date_i < median(planting_date_i, na.rm = TRUE) - 40 | 
    is.na(planting_date_i)
  
  planting_date_i[r] <- replc
  
  dt$planting_date[dt$trial == trials[i]] <- planting_date_i
  
}

rm(planting_date_i, r, replc, i)

sum(is.na(dt$planting_date))

boxplot(dt$planting_date ~ dt$trial)

# get time to to maturity, difference between planting_date and harvesting_date
dt$tmaturity <- dt$harvesting_date - dt$planting_date

boxplot(dt$tmaturity ~ dt$trial)

# remove any value above 120 or below 60
out <- dt$tmaturity > 120 | dt$tmaturity < 60

dt$tmaturity[out] <- NA
dt$harvesting_date[out] <- NA

boxplot(dt$tmaturity ~ dt$trial)

boxplot(dt$harvesting_date ~ dt$trial)

# add tmaturity in the missing entries
for(i in seq_along(trials)){
  
  tmat_i <- dt$tmaturity[dt$trial == trials[i]]
  
  replc <- median(tmat_i, na.rm = TRUE)
  
  r <- is.na(tmat_i)

  tmat_i[r] <- replc  
  
  dt$tmaturity[dt$trial == trials[i]] <- tmat_i
  
}

rm(tmat_i, r, replc, i)

sum(is.na(dt$tmaturity))

boxplot(dt$tmaturity ~ dt$trial)

dt$harvesting_date <- ifelse(is.na(dt$harvesting_date), dt$planting_date + dt$tmaturity, 
                   dt$harvesting_date)

boxplot(dt$harvesting_date ~ dt$trial, las = 2)

# put dates as class Date
dt[dates] <- lapply(dt[dates], function(x){
  as.Date(x, origin = "1970-01-01")
})

boxplot(dt$planting_date ~ dt$trial)
boxplot(dt$harvesting_date ~ dt$trial)

names(dt)

dt <- dt[, -which(names(dt) %in% "tmaturity")]

dt$harvesting_date <- ifelse(is.na(dt$overall_appreciation_best), NA, dt$harvesting_date)

dt$harvesting_date <- as.Date(dt$harvesting_date, origin = "1970-01-01")

# ..........................................
# ..........................................
# ..........................................
# # # Check geographical info ####
lonlat <- c("lon","lat")

sum(is.na(dt[lonlat]))

plot_map(dt, coords = lonlat)

# read data from van Etten (2019) which has the coordinates
nic <- read_csv(paste0(here, "tricot_data.csv"),
                locale = locale(encoding = "UTF-8"))

nic <- nic[nic$crop == "commonbean", ]

nictrial <- trials[which(grepl("nic", trials))]

sum(dt$trial %in% nictrial)

# slice the data to work only in the Nicaragua trials
x <- dt[dt$trial %in% nictrial, ]
y <- dt[!dt$trial %in% nictrial, ]

x$id <- paste0(x$package, x$trial)

x <- x[,-match(lonlat, names(x))]

nic$id <- paste0(nic$package, nic$project)

# remove duplicated ids
x <- x[!duplicated(x$id), ]
nic <- nic[!duplicated(nic$id), ]

sum(x$id %in% nic$id)

# merge with the Nicaragua data
x <- merge(x, nic[, c("id","lon","lat")], by = "id", all.x = TRUE)
x <- x[,-match("id", names(x))]

# put the data back together
dt <- rbind(y, x)

plot_map(dt, coords = lonlat)

# ........................................
# ........................................
sum(is.na(dt[, lonlat]))

# input lat and lon using closest value to the median of z within the adm3 
dt$adm3 <- ClimMobTools:::.title_case(dt$adm3)

dt$id <- paste0(dt$trial, gsub(" |-","", dt$adm3))

p <- unique(dt$id)

dt$z <- rowSums(dt[, c("lon","lat")])

for (i in seq_along(p)){
  # data frame with coordinates and z 
  xy <- dt[dt$id==p[i] & !is.na(dt$adm3), c("lon","lat","z")]
  
  #vector with z minus the mean of z
  z <- xy$z - mean(xy$z, na.rm = TRUE)
  
  #which of these values are closest to 0 (closest to the mean)
  z <- xy[which.min(abs(z)), c("lon","lat")]
  
  #replace NA using the closest value
  dt[,"lon"] <- ifelse(dt[,"id"] == p[i] & is.na(dt[,"lon"]),
                       z[,1], dt[,"lon"])
  dt[,"lat"] <- ifelse(dt[,"id"] == p[i] & is.na(dt[,"lat"]),
                       z[,2], dt[,"lat"])
  
}

sum(is.na(dt[,lonlat]))

dt <- dt[,-match(c("z","id"), names(dt))]

rm(x, y, xy, z, i, keep, nic)


dt[is.na(dt$lon), c("trial", "adm3", "adm1")]


# input lat and lon using closest value to the median of z within the adm1
dt$adm1 <- ClimMobTools:::.title_case(dt$adm1)

dt$id <- paste0(dt$trial, gsub(" |-","", dt$adm1))

p <- unique(dt$id)

dt$z <- rowSums(dt[, c("lon","lat")])

for (i in seq_along(p)){
  # data frame with coordinates and z 
  xy <- dt[dt$id==p[i] & !is.na(dt$adm1), c("lon","lat","z")]
  
  #vector with z minus the mean of z
  z <- xy$z - mean(xy$z, na.rm = TRUE)
  
  #which of these values are closest to 0 (closest to the mean)
  z <- xy[which.min(abs(z)), c("lon","lat")]
  
  #replace NA using the closest value
  dt[,"lon"] <- ifelse(dt[,"id"] == p[i] & is.na(dt[,"lon"]),
                       z[,1], dt[,"lon"])
  dt[,"lat"] <- ifelse(dt[,"id"] == p[i] & is.na(dt[,"lat"]),
                       z[,2], dt[,"lat"])
  
}

sum(is.na(dt[,lonlat]))

dt[is.na(dt$lon), c("trial", "adm3", "adm1", "overall_appreciation_best", "overall_appreciation_worst")]

# rm geo identity in coordinates
dt[, c("lon", "lat")] <- rmGeoIdentity(dt[,c("lon", "lat")])

sum(is.na(dt[,lonlat]))

plot_map(dt, coords = lonlat)

# adm gadm info
# this is a large file stored outside the project dir 
# but can be downloaded from https://gadm.org
here  <- "/Users/kauedesousa/local-workflow/geo-raster-shapefile/world_borders"
gadm <- readOGR(dsn = here, layer = "world_borders_adm0")

myproj <- proj4string(gadm)

val <- !is.na(dt$lon)

coord <- dt[val, c("lon", "lat")]

coord <- SpatialPoints(coord, proj4string = CRS(myproj))

coord <- over(coord, gadm)

dt[val, "adm0"] <- coord$ADMIN

dt$adm2 <- NA

dt[is.na(dt$lat), c("adm0", "trial")]

table(dt$trial, dt$adm0)

dt$adm0[dt$trial=="frijolcr_rojohuetarchorotega2018b"] <- "Costa Rica"
dt$adm0[dt$trial=="frijolrojo_huetarchorotega2017"] <- "Costa Rica"
dt$adm0[dt$trial=="nic_apante_2015"] <- "Nicaragua"
dt$adm0[dt$trial=="nic_apante_2016_1"] <- "Nicaragua"
dt$adm0[dt$trial=="nic_apante_2016_2"] <- "Nicaragua"

table(dt$trial, dt$adm0)

dt[dt$trial == "evaluaciones_masivas_de_frijol_2016", ]

dt <- dt[dt$trial != "evaluaciones_masivas_de_frijol_2016", ]

# # # ..........................................
# # # ..........................................
# # # ..........................................
# Check rankings consistency ####
# overall vs local, change to English
names(dt)

ovl <- paste0("overall_vs_local_", paste0("variety_", letters[1:3]))

unique(unlist(dt[, ovl]))

dt[, ovl] <- lapply(dt[, ovl], function(x) {
    ifelse(x == "Mejor", "Better", 
           ifelse(x == "Peor", "Worse", x))
})

unique(unlist(dt[,ovl]))

# make an id for the data
id <- as.integer(as.factor(paste0(dt$trial, dt$package, dt$adm3, dt$adm1, 
                                  dt$variety_a, dt$variety_b, dt$variety_c)))

dt$id <- id

dt <- dt[!duplicated(dt$id), ]

names(dt)

dt$gender <- ifelse(dt$gender == "M", "Man",
                    ifelse(dt$gender == "F", "Woman", dt$gender))

unique(dt$gender)

head(dt)

names(dt)

dt <- dt[, -which(names(dt) %in% c("package_id", "z"))]

dt <- dt[,union(c("id", paste0("adm", 0:3), "lon", "lat"), names(dt))]


names(dt)[names(dt)=="lon"] <- "longitude"
names(dt)[names(dt)=="lat"] <- "latitude"

head(dt)

# build the main data
main <- matrix(c("Crop", "Common bean", "http://aims.fao.org/aos/agrovoc/c_4098",
                 "Family", "Leguminosae", "http://purl.obolibrary.org/obo/NCBITaxon_3803",
                 "Genus", "Phaseolus", "http://purl.obolibrary.org/obo/NCBITaxon_3883",
                 "Taxa", "Phaseolus vulgaris L.", "http://purl.obolibrary.org/obo/NCBITaxon_3885"),
               ncol = 3, nrow = 4, byrow = TRUE)

dimnames(main)[[2]] <- c("data_label", "description", "keywords")

# build file with varieties description
varieties <- data.frame(variety_name = sort(unique(unlist(dt[,paste0("variety_", letters[1:3])]))),
                        improved_y_n = "",
                        breeding_stage = "",
                        pedigree = "",
                        year_of_release = "")


# build file with descriptors
descriptors <- data.frame(data_label = names(dt),
                          descrition = "", 
                          units_or_code_used = "")

descriptors[1:12,2] <- c("unique identifier given to each tricot package",
                     "the country name",
                     "the first level of adminstrative unit within the country",
                     "the second level of adminstrative unit within the country",
                     "the third level of adminstrative unit within the country",
                     "the geographical coordinate longitude",
                     "the geographical coordinate latitude",
                     "the trial (project) name as registered in ClimMob",
                     "the name of variety assigned as option A in the given package",
                     "the name of variety assigned as option B in the given package",
                     "the name of variety assigned as option C in the given package",
                     "the planting date")

descriptors[1:12,3] <- c("numerical",
                         "",
                         "",
                         "",
                         "",
                         "numerical",
                         "numerical",
                         "",
                         "",
                         "",
                         "",
                         "date")


output <- "output/bean_central_america/"
dir.create(output, recursive = TRUE, showWarnings = FALSE)

write.csv(dt, paste0(output, "data.csv"), row.names = FALSE)
write.csv(main, paste0(output, "main.csv"), row.names = FALSE)
write.csv(varieties, paste0(output, "varieties.csv"), row.names = FALSE)
write.csv(descriptors, paste0(output, "descritors.csv"), row.names = FALSE)

# library(jsonlite)
# 
# result <- list()
# 
# dt2 <- split(dt, dt$id)
# main <- split(main, 1:nrow(main))
# 
# result[["main"]] <- toJSON(main)
# result[["data"]] <- toJSON(dt)
# 
# result <- toJSON(result)
# write(result, "output.json")


# # this takes a subset for the gosset package
# dat <- dt[dt$adm0=="Nicaragua", ]
# 
# dat <- dat[, !apply(dat, 2, function(x)sum(is.na(x))) > 1000]
# 
# dat <- dat[!grepl("overall_vs_local|harvesting_date|adm1|adm3", names(dat))]
# 
# dat <- na.omit(dat)
# 
# table(dat$trial)
# 
# #dat <- dat[dat$trial == "nic_apante_2015", ]
# 
# unique(sort(unlist(dat[,8:10])))
# 
# plot_map(dat, coords = c("longitude", "latitude"))
# 
# 
# names(dat)
# bean_covar <- dat[,c(1:11)]
# rownames(bean_covar) <- 1:nrow(bean_covar)
# 
# # now transform into rankings
# traits <- names(dat)[grepl("_best$", names(dat))]
# traits <- gsub("_best", "", traits)
# 
# bean_rank <- data.frame()
# G <- list()
# 
# for (i in seq_along(traits)) {
# 
#   r <- rankTricot(dat,
#                   items = paste0("variety_", letters[1:3]),
#                   input = paste0(traits[i], c("_best", "_worst")))
#   
#   G[[i]] <- r
#   
#   r <- unclass(r)
# 
#   r <- apply(cbind(id = dat$id, r), 1, function(x){
#     id <- x[1]
#     x <- x[-1]
#     x <- x[x!=0]
# 
#     data.frame(id = as.vector(id),
#                item = names(x),
#                trait = traits[i],
#                rank = as.vector(x))
# 
#   })
# 
#   r <- do.call("rbind", r)
# 
#   bean_rank <- rbind(bean_rank, r)
# 
# }
# 
# traits
# 
# kendall <- lapply(G[-9], function(x){
#   kendallTau(x, G[[9]])
# })
# 
# do.call(rbind, kendall)
# 
# mod <- lapply(G, PlackettLuce)
# 
# worth_map(mod, labels = traits, ref = "Amadeus 77")
# 
# save(bean_covar, bean_rank, file = "nicabean.rda")


