# ..........................................
# Prepare a dataset focussed on yield to BMGF
# ..........................................
# ..........................................
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("multcompView")
library("ggplot2")
library("ggparty")
library("patchwork")
library("tidyverse")
library("raster")
library("sf")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

load("processing/trial-data.rda")

# from list to data.frame
dat = list()

for(i in seq_along(cmdata)) {
  
  x = as.data.frame(cmdata[[i]],
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  dat[[i]] = x
  
}

# check the structure of the data sets
lapply(dat, function(x){unique(x$packagecoordinator)})

names(dat[[17]])

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

# some variables that were identified and are going to be selected 
# this reduces the work when merging, but some could be left out
# this needs to be checked carefully
selvar = c("id$", "package", "longitude", "latitude",
           "yieldperse", "yield_", "yld_", "yieldunit",
           "grainyieldweight_",
           "overall_", "date$", "previouslanduse",
           "plotsize","overallperf_")

selvar = paste(selvar, collapse = "|")

# fix some codes that are not harmonized 
dat = lapply(dat, function(x){
  
  sel = grepl(selvar, names(x))
  
  x = x[, sel]  
  
  names(x) = gsub("overallperf_", "overall_", names(x))
  names(x) = gsub("_a", "a", names(x))
  names(x) = gsub("_b", "b", names(x))
  names(x) = gsub("_c", "c", names(x))
  names(x) = gsub("_pos", "pos", names(x))
  names(x) = gsub("_neg", "neg", names(x))
  names(x) = gsub("_date", "date", names(x))
  names(x) = gsub("item_A", "itemA", names(x))
  names(x) = gsub("item_B", "itemB", names(x))
  names(x) = gsub("item_C", "itemC", names(x))
  names(x) = gsub("project_name", "projectname", names(x))
  names(x) = gsub("-", "_", names(x))
  
  x
  
})

uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

# merge names and replace code synonyms 
dat2 = lapply(dat, function(x){
  
  # break the colnames by _ and keep only the last part
  z = strsplit(names(x), "_")
  
  z = unlist(lapply(z, function(y){
    y[length(y)]
  }))
  
  z = gsub("yielda", "yield_metric_a", z)
  z = gsub("yieldb", "yield_metric_b", z)
  z = gsub("yieldc", "yield_metric_c", z)
  z = gsub("grainyieldweight|yieldperse", "yield_metric_", z)
  z = gsub("grainyield", "yield_", z)
  z = gsub("podyieldpos", "yield_pos", z)
  z = gsub("podyieldneg", "yield_neg", z)
  z = gsub("yieldpos", "yield_pos", z)
  z = gsub("yieldneg", "yield_neg", z)
  z = gsub("yldpos", "yield_pos", z)
  z = gsub("yldneg", "yield_neg", z)
  z = rename_duplicates(z)
  names(x) = z
  x
  
})

uniquenames = lapply(dat2, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

# remove projects with less than 10 data points
keep = unlist(lapply(dat2, nrow)) > 10

dat2 = dat2[keep]

lapply(dat2, function(x) unique(x$packagecoordinator))

# merge all data sets
dat2 = rowbind(dat2)

names(dat2)

# remove some columns 
rmv = c("overalla", "overallb", "overallc",
        "assessmentdate", "haulmyield_", 
        "podyield_", "surveyid", "stoveryield_")

rmv = paste(rmv, collapse = "|")

rmv = !grepl(rmv, names(dat2))

dat2 = dat2[rmv]

names(dat2)

# remove farmer name
dat2 = dat2[,names(dat2) != "name"]

# fix technology names
unique(dat2$technology)

dat2$technology = tolower(dat2$technology)

dat2$technology[grepl("maize", dat2$technology)] = "maize"
dat2$technology[grepl("sorghum", dat2$technology)] = "sorghum"
dat2$technology[grepl("soybeans", dat2$technology)] = "soybean"
dat2$technology[grepl("groundnut", dat2$technology)] = "groundnut"
dat2$technology[grepl("potato", dat2$technology)] = "potato"
dat2$technology[grepl("cowpe", dat2$technology)] = "cowpea"

table(dat2$technology)

# .........................
# .........................
# fix longlat ####
long = grepl("longitude", names(dat2))
long = dat2[long]

lat = grepl("latitude", names(dat2))
lat = dat2[lat]

# select the column with longlat data, from the last
l = unlist(apply(long, 1, function(x){
  x = which(!is.na(x))
  if(length(x) == 0) x = 1
  x[length(x)]
}))

long = as.numeric(long[cbind(1:nrow(long), l)])
lat = as.numeric(lat[cbind(1:nrow(lat), l)])

dat2 = dat2[, !grepl("latitude|longitude", names(dat2))]

names(dat2)

long[is.na(long)] = 0
lat[is.na(lat)] = 0
long[lat > 20] = 0
lat[lat > 20] = 0

plot(long, lat)

rmv = long == 0 & lat == 0 

sum(rmv)

dat2$longitude = long

dat2$latitude = lat

dat2$longitude[rmv] = NA
dat2$latitude[rmv] = NA

plot(dat2[, c("longitude", "latitude")])

plot_map(dat2, c("longitude", "latitude"), 
         map_provider = "OpenStreetMap.Mapnik")

# submitted date
plot(as.Date(dat2$submitteddate1))

write.csv(dat2, 
          "processing/tricot-trials-bmgf-all-data.csv",
          row.names = FALSE)

# ........................................
# ........................................
# Organize yield data ######
# same approach for yield
yP = grepl("yield_pos", names(dat2))
yP = dat2[yP]
yP[yP == 'Not observed'] = NA

yN = grepl("yield_neg", names(dat2))
yN = dat2[yN]

# select the column with longlat data, from the last
y = unlist(apply(yP, 1, function(x){
  x = which(!is.na(x))[1]
  if(is.na(x)) x = 1
  x
}))

table(y)

yP = yP[cbind(1:nrow(yP), y)]
yN = yN[cbind(1:nrow(yN), y)]

dat2 = dat2[, !grepl("yield_pos|yield_neg", names(dat2))]

dat2$yield_pos = yP

dat2$yield_neg = yN


# same for yield quantitative
yA = grepl("yield_metric_a", names(dat2))
yA = dat2[yA]

yB = grepl("yield_metric_b", names(dat2))
yB = dat2[yB]

yC = grepl("yield_metric_c", names(dat2))
yC = dat2[yC]

# select the column with yield data, from the last
y = unlist(apply(yA, 1, function(x){
  x = which(!is.na(x))[1]
  if(is.na(x)) x = 1
  x
}))

table(y)

yA = as.numeric(yA[cbind(1:nrow(yA), y)])
yB = as.numeric(yB[cbind(1:nrow(yB), y)])
yC = as.numeric(yC[cbind(1:nrow(yC), y)])

dat2 = dat2[, !grepl("yield_metric", names(dat2))]

dat2$yield_quant_a = yA

dat2$yield_quant_b = yB

dat2$yield_quant_c = yC


# start preparing the final data set
result = dat2[!is.na(dat2$yield_pos), ]

result = result[!is.na(result$yield_quant_a), ]

table(result$technology)

# make an id that combines the project id and the package id
result$id = paste(result$projectname, result$id, sep = "-")

plot_map(result, c("longitude", "latitude"), 
         map_provider = "OpenStreetMap.Mapnik")

# put tricot rankings into ordinal rankings and 
# put the data into the long format 
trait_labels = c("overall", "yield")
trait_list = getTraitList(result, pattern = c("pos", "neg"),
                          trait.labels = trait_labels)

pack_index = paste0("item", letters[1:3])

# now we build the PlackettLuce rankings
R = lapply(trait_list, function(x){
  rank_tricot(data = result,
              items = pack_index,
              input = x$string,
              validate.rankings = F)
})


pdf(file = "output/trial-network.pdf",
    width = 150,
    height = 150)
plot(network(R[[2]]))
dev.off()


# run over traits and put in the long format
rank_data = data.frame()

for (i in seq_along(trait_labels)) {
  
  r = unclass(R[[i]])
  
  for (j in seq_along(result$id)) {
    
    id = result$id[j]
    
    plots = as.vector(unlist(result[result$id == id, pack_index]))
    
    x = r[j, plots]
    
    d = data.frame(id = id, 
                   plot = LETTERS[1:3],
                   tech_name = plots,
                   trait = as.vector(trait_labels[i]),
                   rank = x)
    
    rank_data = rbind(rank_data, d)
    
  }
  
}

# make a plot id, which is a combination of the previous id and 
# the plot label A, B, C
rank_data$plot_id = paste0(rank_data$id, "-", rank_data$plot)

rank_data = pivot_wider(rank_data, names_from = "trait", values_from = "rank")

head(rank_data)


# now the yield data
names(result)

yield = data.frame(id = rep(result$id, 3),
                   plot = rep(LETTERS[1:3], each = nrow(result)),
                   yield_quant = unlist(result[c(paste0("yield_quant_", letters[1:3]))]))


yield$plot_id = paste0(yield$id, "-", yield$plot)

yield = yield[c("plot_id", "yield_quant")]

result2 = merge(rank_data, yield, by = "plot_id")

paste(names(result), collapse = "', '")

result = result[c('id', 'technology', 'packagecoordinator', 
                  'packagecountry', 'submitteddate1', 'submitteddate2',
                  'plantingdate', 'previouslanduse', 'trialplotsize', 
                  'plotsizeunit', 'yieldunit', 'plotsize',  'yieldunit1',
                  'yieldunit2', 'longitude', 'latitude')]

result = merge(result, result2, by = "id", all.y = TRUE)

write.csv(result, "output/tricot-data-for-bmgf.csv", row.names = FALSE)
