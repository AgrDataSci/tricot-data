# ..........................................
# ..........................................
# Read and organize groundnut data in Tanzania
# ..........................................
# ..........................................
# This is a demonstrative workflow with tricot data 
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("janitor")
library("multcompView")
library("ggplot2")
library("ggparty")
library("patchwork")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

output = "output/groundnut-tanzania"

dir.create(output, recursive = TRUE, showWarnings = FALSE)

# read file with API key, this is a txt file 
# indicating the key(s) and the server from which the data should 
# be retrieved. Its a matrix with two coluns, the first being the key and the second 
# being the server
key = read.table("token/api-key.txt", sep = ",")
key = data.frame(key = key[,1], server = key[, 2])

# get the list of projects from the user indicated in the API key
projects = data.frame()

for(i in seq_along(key$key)) {
  
  p = getProjectsCM(key$key[i], server = key$server[i])
  
  p$key = key$key[i]
  
  projects = rbind(projects, 
                   p)
}

projects$unique = paste0(projects$project_id, projects$user_owner)

keep = !duplicated(projects$unique)

projects = projects[keep, ]

projects = projects[order(projects$user_owner), ]

# remove users that are not providing real projects
unique(projects$user_owner)

table(projects$user_owner)

keep = projects$user_owner == "Happy"

projects = projects[keep, ]

# drop by keyword
drop = "xercise"

keep = !grepl(drop, projects$keywords)

projects = projects[keep, ]

# fetch the data from climmob
cmdata = list()

for(i in seq_along(projects$project_id)) {
  print(i)
  d_i = getDataCM(projects$key[i], 
                  project = projects$project_id[i],
                  userowner = projects$user_owner[i],
                  server = projects$server[i],
                  as.data.frame = FALSE)
  
  cmdata[[i]] = d_i
  
}


# keep only the projects with data on it
keep = lapply(cmdata, function(x){
  isTRUE(nrow(x$data) > 20)
})

keep = unlist(keep)

length(keep)

sum(keep)

cbind(unlist(lapply(cmdata, function(x) {x$project$project_name})), keep)

cmdata = cmdata[keep]

# get question descriptors
questions = lapply(cmdata, function(x){
  x$registry$fields
})

questions

q2 = list()

for(i in seq_along(cmdata)) {
  q2 = c(q2, cmdata[[i]]$assessments$fields)
}

questions = c(questions, q2)

unlist(lapply(questions, nrow)) > 0

keep = unlist(lapply(questions, class)) == "data.frame"

questions = questions[keep]

questions = rowbind(questions)

keep = !duplicated(questions$name)

questions = questions[keep, ]

write.csv(questions, paste0(output, "/metadata.csv"), row.names = FALSE)


# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

# fix some codes that diverged over time
dat = lapply(dat, function(x){
  names(x)[names(x) == "physiologicalmaturity_overallchar2_pos"] = "physiologicalmaturity_overall_pos"
  names(x)[names(x) == "physiologicalmaturity_overallchar2_neg"] = "physiologicalmaturity_overall_neg"
  names(x)[names(x) == "vegetative_overall_pos"] = "vegetative_overallchar2_pos"
  names(x)[names(x) == "vegetative_overall_neg"] = "vegetative_overallchar2_neg"
  x
})

dat = rowbind(dat)

# make a new id
dat$id = paste(dat$package_project_name, dat$id, sep = "-")

# check the regions and gender
table(dat$registration_gender)

dat$registration_gender = ifelse(dat$registration_gender == "Prefer Not To Respond", 
                                 NA, dat$registration_gender)

# debug the names of districts
table(dat$registration_district)

dat$registration_district = toupper(dat$registration_district)
dat$registration_district = gsub("  ", " ", dat$registration_district)
dat$registration_district = gsub(" $", "", dat$registration_district)
dat$registration_district = gsub(",", "", dat$registration_district)
dat$registration_district = gsub("[.]", "", dat$registration_district)

table(dat$registration_district)

# Fix variety names
# check the tags with columns names for the packages 
pack_index = grep("package_item", names(dat))
pack_index = names(dat)[pack_index]
pack_index

genotype_info = read_xlsx("data/groundnut-genotype-release-information.xlsx")






rmv = grepl("overallchar2", names(dat))

rmv

dat = dat[!rmv]

# run this loop over the data to filter it
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traits

traitlabels = traits
traitlabels
traitlabels = gsub("_qst_|_|tricot", " ", traitlabels)
traitlabels = title_case(traitlabels)
traitlabels = gsub(" ", "", traitlabels)
traitlabels = gsub("resistance", "Resistance", traitlabels)
traitlabels = gsub("tolerance", "Tolerance", traitlabels)
traitlabels = gsub("colour", "Colour", traitlabels)
traitlabels = gsub("feeding", "Feeding", traitlabels)
traitlabels = gsub("colour", "Colour", traitlabels)
traitlabels = gsub("requirement", "Requirement", traitlabels)
traitlabels = gsub("population", "Population", traitlabels)
traitlabels = gsub("maturity", "Maturity", traitlabels)
traitlabels = gsub("performance", "Performance", traitlabels)
traitlabels = gsub("yield", "Yield", traitlabels)
traitlabels = gsub("content", "Content", traitlabels)

traitlabels

trait_list = getTraitList(dat, pattern = c("_pos", "_neg"),
                          trait.labels = traitlabels)


# check data availability
lapply(trait_list, function(x){
  c(x$trait_label, round(sum(x$keep) / nrow(dat), 2))
})

# fill not responded with tie
for(i in seq_along(trait_list)) {
  x = dat[, trait_list[[i]]$string]
  dat[, trait_list[[i]]$string] = t(apply(x, 1, function(xx){
    no = xx == "Not observed"
    if(any(is.na(no))) return(xx)
    if (sum(no) == 1) {
      ABC = LETTERS[1:3] != xx[!no]
      set.seed(1133)
      ABC = sample(LETTERS[1:3][ABC], 1)
      xx[no] = ABC
    }
    xx
    }))
}

trait_list = getTraitList(dat, pattern = c("_pos", "_neg"),
                          trait.labels = traitlabels)


# check data availability
lapply(trait_list, function(x){
  c(x$trait_label, round(sum(x$keep) / nrow(dat), 2))
})

# now we build the PlackettLuce rankings
R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = FALSE)
})

# plot the trial network
plot(network(R[[1]]))


lapply(R, PlackettLuce)


rank_data = data.frame()

for (i in seq_along(traitlabels)) {
  
  r = unclass(R[[i]])
  
  for (j in seq_along(dat$id)) {
    
    id = dat$id[j]
    
    plots = as.vector(unlist(dat[dat$id == id, pack_index]))
    
    x = r[j, plots]
    
    d = data.frame(id = id, 
                   plot = LETTERS[1:3],
                   tech_name = plots,
                   trait = as.vector(traitlabels[i]),
                   rank = x)
    
    rank_data = rbind(rank_data, d)
    
  }
  
}


# get the other data in a horizontal data.frame
# remove the trait data 
result = dat

keep = !grepl("_pos|_neg|participant_name|telephone", names(dat))

result = result[keep]

names(result)


# check coordinates
dat$vegetative_geotrial_longitude = as.numeric(dat$vegetative_geotrial_longitude)
dat$vegetative_geotrial_latitude = as.numeric(dat$vegetative_geotrial_latitude)

names(dat) = gsub("vegetative_geotrial_", "trial_", names(dat))

dat$trial_longitude[dat$trial_longitude == 0] = NA

dat$trial_latitude[dat$trial_latitude == 0] = NA

dat[,c("trial_longitude", "trial_latitude")]

plot(dat[,c("trial_longitude", "trial_latitude")])

plot_map(dat, c("trial_longitude", "trial_latitude"), 
         map_provider = "OpenStreetMap.Mapnik")




write.csv(rank_data, 
          paste0(output, "/trial-data.csv"),
          row.names = F)

write.csv(result, 
          paste0(output, "/farm-data.csv"), 
          row.names = F)

write.csv(questions,  
          paste0(output, "/descriptors.csv"), 
          row.names = F)

map = plot_map(dat, c("trial_longitude", "trial_latitude"), 
               map_provider = "OpenStreetMap.Mapnik")

# mapview::mapshot(map, 
#                  url = paste0(output, "/trial-map.html"),
#                  file = paste0(output, "/trial-map.png"))
# 
# list.files(output, full.names = TRUE)
# 
# file.remove(paste0(output, "/trial-map_files"))
# file.remove(paste0(output, "/trial-map.html"))




plot(result$physiologicalmaturity_yieldperse_a)
plot(result$physiologicalmaturity_yieldperse_b)
plot(result$physiologicalmaturity_yieldperse_c)


# ......................................

rank = read.csv("output/groundnut-tanzania/trial-data.csv")

dat = read.csv("output/groundnut-tanzania/farm-data.csv") 

head(rank)

unique(rank$trait)

trait = "PhysiologicalMaturityYield"

keep = rank$trait %in% trait

rank = rank[keep, ]

rank$id = paste(rank$id, rank$plot, sep = "_")

yield = dat[,c("id", paste0("physiologicalmaturity_yieldperse_", letters[1:3]))]

head(yield)

yield = data.frame(id = paste(rep(yield$id, times = 3), 
                              rep(LETTERS[1:3], each = nrow(dat)), 
                              sep = "_"),
                   yield = c(yield[,"physiologicalmaturity_yieldperse_a"],
                             yield[,"physiologicalmaturity_yieldperse_b"],
                             yield[,"physiologicalmaturity_yieldperse_c"]))

dat = merge(rank, yield, by = "id", all.x = TRUE)

id = strsplit(dat$id, "_")

id = lapply(id, function(x){
  x[1]
})

dat$id = unlist(id)

write.csv(dat, "output/groundnut-tanzania/yield-data.csv",
          row.names = F)


