# ..........................................
# ..........................................
# Read and organize bean data in East Africa
# ..........................................
# ..........................................
# This is a demonstrative workflow with tricot data 
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("multcompView")
library("ggplot2")
library("ggparty")
library("patchwork")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

list.files("data/raw/bean_east_africa", full.names = TRUE)

dat = read.csv("data/raw/bean_east_africa/bean_data_east_africa.csv")

sort(unique(dat$package_project_name))

str(dat)

names(dat)

# check the regions and gender
table(dat$registration_gender)

dat$registration_gender = ifelse(dat$registration_gender == "Gender Nonconforming", 
                                 NA, dat$registration_gender)

# debug the names of districts
table(dat$registration_district)

dat$registration_district = toupper(dat$registration_district)
dat$registration_district = gsub("  ", " ", dat$registration_district)
dat$registration_district = gsub(" $", "", dat$registration_district)
dat$registration_district = gsub(",", "", dat$registration_district)
dat$registration_district = gsub("[.]", "", dat$registration_district)

table(dat$registration_district)

# now create a data.frame to fix this by hand
# districts = data.frame(original_name = sort(unique(dat$registration_district)), 
#                         replace_by = "",
#                         region = "")
# 
# # write it in the disk
# write.csv(districts, file = "data/districts_info.csv", row.names = FALSE)

# # open the final file 
# districts = read.csv("data/raw/bean_east_africa/districts_info.csv", na.strings = "")
# 
# dat$district = NA
# dat$region = NA
# # replace the names
# for (i in seq_along(districts$original_name)){
#   dat$district = ifelse(dat$registration_district == districts$original_name[i],
#                         districts$replace_by[i],
#                         dat$district)
#   dat$region = ifelse(dat$registration_district == districts$original_name[i],
#                       districts$region[i],
#                       dat$region)
# }
# 
# dat[,c("district", "region")]

# after cleaning the data you may choose to write a new file 
# and end the script here and open a new one only for analysis 


# check the tags with columns names for the packages 
pack_index = grep("package_item", names(dat))
pack_index = names(dat)[pack_index]
pack_index

rmv = grepl("overallchar2", names(dat))

dat = dat[!rmv]

# run this loop over the data to filter it
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traits

traitlabels = traits

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

trait_list = getTraitList(dat, pattern = c("_pos", "_neg"),
                          trait.labels = traitlabels)

# now we build the PlackettLuce rankings
R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = F)
})

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


# get it other data in a horizontal data.frame
# remove the trait data 
result = dat

keep = !grepl("_pos|_neg|participant_name|telephone", names(dat))

result = result[keep]

names(result)

dir.create("output/bean-east-africa")

write.csv(rank_data, "output/bean-east-africa/bean-east-africa-trial-data.csv", row.names = F)
write.csv(result, "output/bean-east-africa/bean-east-africa-farm-data.csv", row.names = F)

descrip = read.csv("data/raw/bean_east_africa/bean_data_east_africa_descriptors.csv")

write.csv(descrip, "output/bean-east-africa/bean-east-africa-descriptors.csv", row.names = F)














