# #################################
# Collate data 
# This script will pick all the 
# #################################
# load packages
library("ClimMobTools")
library("gosset")
library("readxl")
library("tidyverse")
library("geodata")
library("janitor")
library("sf")
library("readxl")
# additional functions from ClimMob-analysis 
#source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
#source("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/modules/01_functions.R")
# 1000FARMS and data on ClimMob 3
load("processing/trial-data.rda")

ids = unlist(lapply(cmdata, function(x){
  x$project$project_id
}))

duplicated(ids)

for(i in seq_along(ids)){
  dat[[i]]$package_trial_id = ids[i]
}

# add data from climmob 2 
# # ethiopia ISSD data
names(dat[[1]])[1:30]

issd = read.csv("data/issd-ethiopia/tricot-data.csv")

issd$crop = ifelse(issd$crop == "haricotbean",
                   "commonbean",
                   issd$crop)

names(issd)[1:20]
names(issd) = gsub("_worst", "_neg", names(issd))
names(issd) = gsub("_best", "_pos", names(issd))
names(issd)[1:15] = paste0("package_", names(issd)[1:15]) 
names(issd) = gsub("package_crop", "package_technology", names(issd))
names(issd) = gsub("package_package_id", "id", names(issd))
names(issd) = gsub("package_trial", "package_project_name", names(issd))
names(issd) = gsub("variety_", "item_", names(issd))
issd$package_country = "ET"
issd$package_coordinator = "ISSD"
issd$package_trial_id = substr(gsub("_", "", issd$package_project_name), 1, 12)

# cassava data
nextgen = read.csv("data/cassava-nigeria/tricot-data-nextgen-cassava.csv")
nextgen$package_technology = "cassava"
nextgen$package_country = "NG"
nextgen$package_project_name = paste0("nextgen_nigeria-year-", nextgen$year)
names(nextgen)[1:21] = paste0("package_", names(nextgen)[1:21]) 
names(nextgen) = gsub("package_variety", "package_item_", names(nextgen))
names(nextgen) = gsub("_sex", "_gender", names(nextgen))
nextgen$id = nextgen$package_farmerpackage
names(nextgen) = gsub("final_evaluation_choiceforreplant_", "overall_", names(nextgen))
names(nextgen) = gsub("rootweight_plot", "yieldperse", names(nextgen))
nextgen$package_coordinator = "Bela Teeken"
nextgen$package_trial_id = substr(gsub("_|-", "", nextgen$package_project_name), 8, 20)
names(nextgen)

dat[["issd"]] = issd

dat[["nextgen"]] = nextgen

dat = lapply(dat, function(x){
  names(x)[names(x) == "id"] = "package_id"
  x
})

# select only package info and the overall and yield
dat = lapply(dat, function(x){
  d = grep("package|longitude|latitude|overall|yield|gender|registration_survey_end", names(x))
  x = x[,d]
})

unique(unlist(lapply(dat, names)))

dat = rowbind(dat)

names(dat)[names(dat) == "registration_survey_end"] = "planting_date"
table(dat$visit4_threshing_100_percent_grainyield_pos)
table(dat$visit4_threshing_100_percent_grainyield_neg)

table(issd$overall_performance_neg)
table(dat$overall_performance_neg)

# ....................................................
# ....................................................
# Package and crop info ####
dat$package_technology = tolower(dat$package_technology)

table(dat$package_coordinator)
table(dat$package_technology)
table(dat$package_country)

dat$package_technology[dat$package_technology == "amaranths"] = "amaranth"
dat$package_technology[grepl("cowp", dat$package_technology)] = "cowpea"
dat$package_technology[grepl("groundnu", dat$package_technology)] = "groundnut"
dat$package_technology[grepl("nextgen|drc varieties|cassava", dat$package_technology)] = "cassava"
dat$package_technology[grepl("sorghum", dat$package_technology)] = "sorghum"
dat$package_technology[grepl("potato", dat$package_technology)] = "potato"
dat$package_technology[grepl("banana", dat$package_technology)] = "banana"
dat$package_technology[grepl("maize |crop designs|24a wet|24a dry|24a transition|24a highlands|gmcc 24a", dat$package_technology)] = "maize"
dat$package_technology[grepl("soybeans ", dat$package_technology)] = "soybean"
dat$package_technology[grepl("climbing beans ", dat$package_technology)] = "climbing beans"
dat$package_technology[grepl("bush beans ", dat$package_technology)] = "bush beans"
dat$package_technology[grepl("fingermillet", dat$package_technology)] = "finger millet"
dat$package_technology[grepl("ethiopiagrass", dat$package_technology)] = "forages"
dat$package_technology[grepl("haricotbean|lines of common beans|commonbean", dat$package_technology)] = "common bean"
dat$package_technology[dat$package_technology=="bean"] = "common bean"

table(dat$package_technology)

dat = dat[dat$package_country != "LA", ]
dat = dat[dat$package_technology != "forages", ]
dat = dat[dat$package_technology != "banana", ]
dat = dat[!dat$package_technology == "wheat", ]
dat = dat[dat$package_technology != "african eggplant", ]
dat = dat[dat$package_technology != "amaranth", ]
dat = dat[dat$package_technology != "jute mallow", ]
dat = dat[dat$package_technology != "maize-bean intercrops", ]
dat = dat[dat$package_technology != "oat", ]
dat = dat[dat$package_technology != "okra", ]
dat = dat[dat$package_technology != "sesame", ]
dat = dat[dat$package_technology != "blackcumin", ]
dat = dat[dat$package_technology != "lentil", ]

keep = grepl("ET|RW|NG|TZ", dat$package_country)

table(keep)

dat = dat[keep, ]

rownames(dat) = 1:nrow(dat)

table(dat$overall_performance_neg)

# fix PI name
unique(dat$package_coordinator)

dat$package_coordinator = ClimMobTools:::.title_case(tolower(dat$package_coordinator))
dat$package_coordinator = gsub("Dr. |Dr |Dr.|Dr", "", dat$package_coordinator)
dat$package_coordinator[grepl(" Sikirou Mouritala", dat$package_coordinator)] = "Sikirou Mouritala"
dat$package_coordinator[grepl("Evans Ouma,", dat$package_coordinator)] = "Evans Ochieng Ouma"
dat$package_coordinator[grepl("Fadhilidhili Kasubiri|Fadhili And Sylvia" , dat$package_coordinator)] = "Fadhili Kasubiri"
dat$package_coordinator[grepl("Ml Umar", dat$package_coordinator)] = "Muhammad Lawan Umar"
dat$package_coordinator[grepl("Maryam A. Dawud", dat$package_coordinator)] = "Maryam Abba Dawud"
dat$package_coordinator[grepl("Placide Rukondo|Hyacinthe Nyirahabimana", dat$package_coordinator)] = "Placide Rukundo"
dat$package_coordinator[grepl("Patrick Ongom", dat$package_coordinator)] = "Ousmane Boukar"
dat$package_coordinator[grepl("Elysé Tuyishime", dat$package_coordinator)] = "Elyse Tuyishime"
dat$package_coordinator[grepl("Abel Moges Firew", dat$package_coordinator)] = "Berhanu Fenta"
dat$package_coordinator[grepl("Abolore Bello|Bello Abolore|Bela Teeken|Rabbi Ismail", dat$package_coordinator)] = "Bela Teeken"

unique(dat$package_coordinator)

# remove columns with no info after taking out rows
keep = unlist(lapply(dat[1:ncol(dat)], function(x){
  sum(is.na(x))
}))

keep = keep != nrow(dat)

dat = dat[keep]

names(dat)

table(dat$package_technology, dat$package_country)

table(dat$overall_performance_neg)

# ....................................................
# ....................................................
# Gender ####
keep = grep("gender", names(dat))

gender = dat[, keep]

dat = dat[, -keep]

sort(unique(unlist(gender)))

gender[gender == "F"] = "Woman"

gender[gender == "M"] = "Man"

gender = as.vector(apply(gender, 1, function(x){
  x = ifelse(x == "Man" | x == "Woman", x, NA)
  x[which(!is.na(x))[1]]
}))

dat$gender = gender

# ....................................................
# ....................................................
# Planting dates ####
# fix planting date and year
keep = grep("planting_date", names(dat))

pdates = dat[, keep]

dat = dat[, -keep]

sort(unique(unlist(pdates)))

pdates = as.vector(apply(pdates, 1, function(x){
  x[which(!is.na(x))[1]]
}))

dat$planting_date = as.Date(pdates)

pdates = dat[, keep]

dat = dat[, -keep]

dat$year = format(dat$planting_date, "%Y")

# ....................................................
# ....................................................
# Coordinates ####
# get the coordinates as an independent data.frame
keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

names(lonlat)

dat = dat[, !keep]

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

unique(lon)

lon[is.na(lon)] = "registration_pointofdelivery_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

rownames(dat) = 1:nrow(dat)
rownames(lonlat) = 1:nrow(lonlat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                    latitude = lonlat[cbind(1:nrow(lonlat), lat)])

lonlat

lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

# latitude higher than 23 is wrong
lonlat$longitude[lonlat$latitude > 15] = NA
lonlat$latitude[lonlat$latitude > 15] = NA

# also lonlat with 0, 0
lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA
lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] = NA

lonlat$longitude[lonlat$longitude > 43] = NA

lonlat$latitude[is.na(lonlat$longitude)] = NA

plot(lonlat)

dat = cbind(dat, lonlat)

plot(dat[c("longitude", "latitude")])

w = st_read("data/gadm/africa", "africa_adm0")

w = st_as_sf(w)

pmap = ggplot() +
  geom_sf(w$geometry,
          mapping = aes(),
          colour = "#4d4d4d",
          fill = "#ffffd1") +
  geom_jitter(data = dat, aes(x = longitude,
                                y = latitude, 
                                color = package_technology),
              shape = "+") +
  theme(legend.position = "right",
        legend.text = element_text(size = 12,
                                   colour = "grey20"),
        panel.background = element_blank(),
        plot.margin = unit(rep(1, 4), "mm"),
        legend.title = element_blank()) +
  theme_void()

# ....................................................
# ....................................................
# Overall ####
# put together the rankings on overall performance
names(dat)
table(dat$overall_performance_pos)
names(dat) = gsub("overallcharfr", "overall", names(dat))
names(dat) = gsub("overallper_pos", "overall_pos", names(dat))
names(dat) = gsub("overallper_neg", "overall_neg", names(dat))
names(dat) = gsub("overallperf_pos", "overall_pos", names(dat))
names(dat) = gsub("overallperf_neg", "overall_neg", names(dat))
names(dat) = gsub("overallimpression_", "overall_", names(dat))
names(dat) = gsub("overall_performance_", "overall_", names(dat))
names(dat) = gsub("overallchar2_", "overall_", names(dat))
names(dat) = gsub("finalevaluationoverall_", "overall_", names(dat))
names(dat) = gsub("_best", "_pos", names(dat))
names(dat) = gsub("_worst", "_neg", names(dat))

names(dat)[grepl("overall", names(dat))]

keep = grepl("overall_pos|overall_neg", names(dat))

overall = dat[, keep]

lapply(overall[1:ncol(overall)], function(x){
  table(x)
})

keep = grepl("overall", names(dat))

dat = dat[, !keep]

names(dat)

unique(unlist(overall))

substr(unique(unlist(overall)), 1, 1)

overall[1:ncol(overall)] = lapply(overall[1:ncol(overall)], function(x){
  x = substr(x, 1, 1)
  z = x %in% LETTERS[1:3]
  x[!z] = NA
  x
})

lapply(overall[1:ncol(overall)], function(x){
  table(x)
})

names(overall) = make_clean_names(names(overall))

# get the positive ranking
pos = grep("overall_pos", names(overall))
pos = overall[, pos]

unique(unlist(pos))

pos = as.vector(apply(pos, 1, function(x){
  # I'll take the reverse as this increases the likelihood of 
  # getting the coordinates from the trial, not the point of 
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

sort(unique(pos))

pos[is.na(pos)] = "overall_pos"

neg = gsub("_pos$", "_neg", pos)

table(neg)
table(pos)

sum(table(overall[cbind(1:nrow(overall), pos)]))

# keep only the selected columns, one per block
ov = data.frame(overall_pos = overall[cbind(1:nrow(overall), pos)], 
                overall_neg = overall[cbind(1:nrow(overall), neg)])

items_index = paste0("package_item_", letters[1:3])

dat = cbind(dat, ov)

# ....................................................
# ....................................................
# Yield ranking ####
names(dat)

#x = data.frame(yield_var = names(dat)[grepl("yield", names(dat))], final_name = "")
#write.csv(x, "data/clean-yield-variables.csv", row.names = FALSE)

yield_vars = read.csv("data/clean-yield-variables.csv")

rmv = yield_vars$yield_var[yield_vars$keep == F]

keep = !names(dat) %in% rmv

dat = dat[keep]

# replace names
for(i in seq_along(yield_vars$yield_var)) {
  names(dat)[names(dat) == yield_vars$yield_var[i]] = yield_vars$final_name[i]
}

names(dat)

# now make two tables, one with the quantitative yield and the other 
# with the ranking yield
keep = grepl("yieldperse_|yieldunit", names(dat))

yield_q = dat[, keep]

yield_q[yield_q == "Not applicable"] = NA

dat = dat[, !keep]

# yield ranking 
keep = grepl("yield_", names(dat))

yield_r = dat[, keep]

dat = dat[, !keep]

yield_r[1:ncol(yield_r)] = lapply(yield_r[1:ncol(yield_r)], function(x){
  z = x %in% LETTERS[1:3]
  x[!z] = NA
  x
})

unique(unlist(yield_r))

# get the positive ranking
pos = grep("yield_pos", names(yield_r))
pos = yield_r[, pos]
head(pos)
unique(unlist(pos))

pos = as.vector(apply(pos, 1, function(x){
  # I'll take the reverse as this increases the likelihood of 
  # getting the coordinates from the trial, not the point of 
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

sort(unique(pos))

pos[is.na(pos)] = "yield_pos"

neg = gsub("yield_pos", "yield_neg", pos)

table(neg)
table(pos)

sum(table(yield_r[cbind(1:nrow(yield_r), pos)]))

unique(pos[!neg %in% names(yield_r)])
unique(neg[!neg %in% names(yield_r)])

# keep only the selected columns, one per plot
yd = data.frame(yield_pos = yield_r[cbind(1:nrow(yield_r), pos)],
                yield_neg = yield_r[cbind(1:nrow(yield_r), neg)])


dat = cbind(dat, yd)

#...............................................
#...............................................
unique(unlist(yield_q))

# get the positive ranking
pos = grep("yieldperse_a", names(yield_q))
pos = yield_q[, pos]
head(pos)
unique(unlist(pos))

pos = as.vector(apply(pos, 1, function(x){
  # I'll take the reverse as this increases the likelihood of 
  # getting the coordinates from the trial, not the point of 
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

sort(unique(pos))

pos[is.na(pos)] = "yieldperse_a"

pos2 = gsub("yieldperse_a", "yieldperse_b", pos)

pos3 = gsub("yieldperse_a", "yieldperse_c", pos)

table(pos)
table(pos2)
table(pos3)

# get the units
unit = grep("unit", names(yield_q))

unit = yield_q[unit]

unit = as.vector(apply(unit, 1, function(x){
  # I'll take the reverse as this increases the likelihood of 
  # getting the coordinates from the trial, not the point of 
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

sort(unique(unit))

unit[is.na(unit)] = "breederassessment_harvestyieldunit"

# keep only the selected columns, one per plot
ym = data.frame(yield_metric_a = yield_q[cbind(1:nrow(yield_q), pos)],
                yield_metric_b = yield_q[cbind(1:nrow(yield_q), pos2)],
                yield_metric_c = yield_q[cbind(1:nrow(yield_q), pos3)],
                yield_metric_unit = yield_q[cbind(1:nrow(yield_q), unit)])

unique(ym$yield_metric_unit)

dat = cbind(dat, ym)

names(dat)

paste(names(dat), collapse = "','")

rmv = c('package_participant_name', 'package_X', 
        'package_district', 'package_village', 
        'package_tricot_pkg', 'package_rhomis',
        'package_farmerpackage', 'package_maritalstatus', 'package_farmingstatus', 
        'package_educatiolqualification', 'package_communityme', 'package_lga', 
        'package_setorialzone', 'package_state', 'package_coordinate', 'package_rhomisid')

rmv = names(dat) %in% rmv

result = dat[, !rmv]

result$package_id = paste0(result$package_project_name, "-",result$package_id, "-", result$package_trial_id)

keep = !is.na(result$overall_pos) & !is.na(result$overall_neg)

result = result[keep, ]

table(result$package_technology, result$package_country)

result$package_id[duplicated(result$package_id)]

result = result[!duplicated(result$package_id), ]

names(result)

#..........................................
#...........................................
# put data into the long format
names(result) = gsub("package_", "", names(result))
names(result)[names(result) == "technology"] = "crop"
table(result$gender)
result$gender[result$gender == "M"] = "Man"
result$gender[result$gender == "F"] = "Woman"
table(result$gender)

# get traits
trait_labels = c("overall", "yield")

trait_list = getTraitList(result, pattern = c("_pos", "_neg"),
                          trait.labels = trait_labels)

pack_index = paste0("item_", letters[1:3])

result[pack_index] = lapply(result[pack_index], function(x){
  x = ifelse(is.na(x), "NotAvailable", x)
  x
})

sort(unique(unlist(result[pack_index])))

# now we build the PlackettLuce rankings
R = lapply(trait_list, function(x){
  rank_tricot(data = result,
              items = pack_index,
              input = x$string,
              validate.rankings = F)
})

rank_data = list()

for (i in seq_along(trait_labels)) {
  
  r = unclass(R[[i]])
  
  rank_df = data.frame()
  
  for (j in seq_along(result$id)) {
    
    id = result$id[j]
    
    plots = as.vector(unlist(result[result$id == id, pack_index]))
    
    x = r[j, plots]
    
    d = data.frame(crop_name = result$crop[j],
                   trial_id = result$trial_id[j],
                   block_id = id, 
                   plot_id = LETTERS[1:3],
                   genotype_name = plots,
                   rank = x)
    
    rank_df = rbind(rank_df, d)
    
  }
  
  rank_data[[i]] = rank_df
  
}

names(rank_data[[1]])[names(rank_data[[1]]) == "rank"] = "overall"

names(rank_data[[2]])[names(rank_data[[2]]) == "rank"] = "yield"

rank_data = cbind(rank_data[[1]], yield = rank_data[[2]]$yield)

sel = c("yield_metric_a", "yield_metric_b", "yield_metric_c", "yield_metric_unit")

yield_metric = data.frame()

for(i in seq_len(nrow(result))) {
  print(i)
  ym = data.frame(yield_metric = unlist(result[i, sel[1:3]]),
                  yield_unit = unlist(result[i,sel[4]]))
  
  yield_metric = rbind(yield_metric, ym)
}

result2 = cbind(rank_data, yield_metric)

names(result)

sel = c("id", "coordinator", "country", "age","gender",
        "year","planting_date","longitude","latitude")

names(result)[names(result) == "id"] = "block_id"

sel = c("block_id", "coordinator", "country", "age","gender",
        "planting_date","longitude","latitude")

result[sel]

result2 = merge(result2, result[sel], by = "block_id", all.x = TRUE)

sort(table(result2$coordinator))

f = list.files("data/variety-metadata", full.names = TRUE)

var_info = data.frame()

for(i in seq_along(f)) {
  x = read_excel(f[i])
  var_info = rbind(var_info, x)
}


result3 = result2

head(var_info)

unique(result3$crop_name) %in% unique(var_info$crop_name)

var_info = 
  var_info %>% 
  group_by(crop_name) %>% 
  filter(!duplicated(genotype_name))
  
result3$final_genotype_name = NA

for(i in seq_along(var_info$genotype_name)) {
  print(i)
  result3$final_genotype_name = ifelse(result3$crop_name == var_info$crop_name[i] &
                                         result3$genotype_name == var_info$genotype_name[i],
                                       var_info$final_genotype_name[i],
                                       result3$final_genotype_name)
  
  
}

paste(names(result3), collapse = "', '")


result3$year = format(as.Date(result3$planting_date), "%Y")

sel = c('crop_name', 'trial_id', 'block_id', 'plot_id', 'coordinator', 'country',
        'genotype_name', 'final_genotype_name', 'overall', 'yield', 'yield_metric', 'yield_unit', 
        'age', 'gender', 'year', 'planting_date', 'longitude', 'latitude')

result3 = result3[sel]

#result3 = result3[result3$genotype_name != "NotAvailable", ]

unique(result3$crop_name)

result3$yield_metric = as.numeric(result3$yield_metric)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "cassava", "kg", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "cowpea", "gm", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "finger millet", "gm", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "groundnut", "gm", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "pearl millet", "gm", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "potato" & result3$yield_metric < 21, "kg", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "potato" & result3$yield_metric > 20, "g", result3$yield_unit)

result3$yield_unit = ifelse(is.na(result3$yield_unit) & result3$crop_name == "potato", "gm", result3$yield_unit)

write.csv(result3, "data/tricot-data-long.csv", row.names = FALSE)

write.csv(var_info, "data/tricot-data-long-variety-metadata.csv", row.names = FALSE)

head(result3)

missing_units = 
  result3 %>% 
  group_by(trial_id) %>% 
  summarise(no_unit = all(is.na(yield_unit)),
            crop_name = unique(crop_name),
            coordinator = unique(coordinator))

write.csv(missing_units, "data/missing-units.csv", row.names = FALSE)


