# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("reshape2")
library("readxl")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

# read file with variety names
varieties = read_excel("data/groundnut-genotype-release-information.xlsx")

# read file with API key, this is a txt file 
# indicating the key(s) and the server from which the data should 
# be retrieved. Its a matrix with two coluns, the first being the key and the second 
# being the server

# key = read.table("token/api-key.txt", sep = ",")
# key = data.frame(key = key[,1], server = key[, 2])

key = "28abd92b-406a-4260-9614-601cdf243afe"
server = "1000FARMS"

# get the list of projects from the user indicated in the API key
projects = data.frame()

projects = getProjectsCM(key, server = server)

# select by userowner
userowner = c('Happy')

projects = projects[projects$user_owner %in% userowner, ]

projects = projects[grepl("groundnut", tolower(projects$project_name)), ]

projects

# fetch the data from climmob
cmdata = list()

for(i in seq_along(projects$project_id)) {
  
  d_i = getDataCM(key, 
                  project = projects$project_id[i],
                  userowner = projects$user_owner[i],
                  server = projects$server[i],
                  as.data.frame = FALSE)
  
  cmdata[[i]] = d_i
  
}


save(cmdata, file = "data/groundnut-data-tanzania.rda")

# load("data/groundnut-data-tanzania.rda")

# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

keep = unlist(lapply(dat, nrow)) > 0

dat = dat[keep]

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

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
  names(x) = gsub("overallchar2", "overall", names(x))
  x
})

# put the data together 
dat = rowbind(dat)

# fix trial coordinator name
sort(unique(dat$package_coordinator))

dat$package_coordinator[dat$package_coordinator == "HAPPY DAUDI"] = "Happy Daudi"

# countries
unique(dat$package_country)

# ..................................
# get summaries ####
traits = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(traits, function(x){
  x$trait_label
}))

traits

traits = gsub("_qst_|_|tricot", " ", traits)
traits = ClimMobTools:::.title_case(traits)
traits = gsub("Podyield", "Pod yield", traits)
traits = gsub("Haulmyield", "Haulm yield", traits)
traits = gsub("yield", "Yield", traits)
traits = gsub("Grainsize", "Grain Size", traits)
traits = gsub("Graincolour", "Grain Color", traits)
traits = gsub("content", " Content", traits)
traits = gsub("resistance", " Resistance", traits)
traits = gsub("tolerance", " Tolerance", traits)
traits = gsub("labour", " Labor", traits)
traits = gsub("Physiologicalmaturity ", "", traits)
traits = gsub("Agronomicperformance ", "", traits)
traits = gsub("Reproductive Diseases Resistance", "Diseases Resistance [Reproductive]", traits)
traits = gsub("Reproductive Vigor", "Vigor [Reproductive]", traits)
traits = gsub(" Overall", "", traits)
traits = gsub("Vegetative ", "", traits)
traits = gsub("Reproductive ", "", traits)
traits

out = !traits %in% c("Vegetative", "Reproductive", "Flowering")

traitlist = getTraitList(dat, c("_pos", "_neg"), trait.labels = traits)

traits = traits[out]

traitlist = traitlist[out]

pack_index = paste0("package_item_", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames




itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

reference = "Mnanje 09"

# PlackettLuce rankings
traits

reference = which(traits == "Overall")

dat$gender = ifelse(dat$registration_gender == "Prefer Not To Respond", NA, dat$registration_gender)

dat = dat[!is.na(dat$gender), ]

table(dat$gender)

# ...............................................
# ...............................................
# Kendall partial correlation ####
traits

# check data availability
cbind(traits, unlist(lapply(traitlist, function(x) sum(x$keep))))

gender = unique(dat$gender)

trait_cor = data.frame()

for (g in seq_along(gender)) {
  
  R = lapply(traitlist, function(x){
    rank_tricot(data = dat[dat$gender == gender[g], ],
                items = pack_index,
                input = x$string,
                validate.rankings = TRUE)
  })
  
  rN = kendallTauCorMatrix(R)
  
  r = rN[[1]]
  
  N = rN[[2]]
  
  # Test with the full matrix, just to see if it works
  pr = cor2pcor(r)
  
  # Simple way to work with these values is to convert to Pearson's r
  r_pearson = sin(3.141592654 * r * .5)
  
  dimnames(r_pearson) = list(traits, traits)
  
  r_pearson
  
  #calculate pearson correlation P-value 
  ttv = list()
  ttest_pvalue_matrix = matrix(NA, nrow = length(traits), ncol = length(traits))
  
  #Fill the matrix
  for(i in seq_along(traits)){
    for (j in seq_along(traits)){ #Fills lower triangle only, to avoid calculating the same value twice
      
      keep_tt = r_pearson[j,i] *sqrt((mean(N) - 2)/ (1-(r_pearson[j,i]^2)))
      ttv = keep_tt
      keep_tt_value = 2 * (pt(-abs(ttv), mean(N) - 2))
      ttest_pvalue_matrix[j,i] = keep_tt_value
    }
  }
  
  cor_g = data.frame(trait= traits, 
                     cor = r_pearson[, reference],
                     pval = ttest_pvalue_matrix[, reference],
                     gender = gender[g])
  
  cor_g = cor_g[-reference, ]
  
  trait_cor = rbind(trait_cor, cor_g)
  
}


table(dat$package_project_name, dat$gender)

table(dat$gender)

# get the traits with p < 0.1 in at least one gender
keep = trait_cor$pval < 0.1

keep = unique(trait_cor$trait[keep])

keep = trait_cor$trait %in% keep

trait_cor = trait_cor[keep, ]

trait_cor$trait = factor(trait_cor$trait, levels = traits)

ngender = table(dat$gender)

trait_cor$gender = ifelse(trait_cor$gender == "Man",
                          paste0(trait_cor$gender, " (n = ", ngender[1], ")"),
                          ifelse(trait_cor$gender == "Woman",
                                 paste0(trait_cor$gender, " (n = ", ngender[2], ")"),
                                 trait_cor$gender))

# make a bar plot plot
cor_plot =
  ggplot(data = trait_cor,
         aes(y = cor,
             x = trait,
             fill = trait)) +
  geom_chicklet(show.legend = FALSE) +
  coord_flip() +
  facet_grid(~ gender) +
  scale_fill_manual(values = rev(col_pallet(length(traits)))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        strip.background =element_rect(fill="white"),
        text = element_text(color = "grey10"),
        strip.background.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        strip.text = element_text(size = 16, color = "grey10"),
        legend.text = element_text(size = 16, color = "grey10"),
        axis.text = element_text(size = 16, color = "grey10"),
        axis.title = element_text(size = 16, color = "grey10"),
        legend.title = element_blank()) +
  labs(x = "",
       y = "Correlation with Overall Preference")

cor_plot

dir.create("output/groundnut", recursive = TRUE, showWarnings = FALSE)

ggsave("output/groundnut/kendall-tau-groundnut.pdf",
       plot = cor_plot,
       height = 8,
       width = 14)



# kendall = data.frame()
# 
# gender = unique(dat$gender)
# 
# for(i in seq_along(gender)) {
#   
#   r = lapply(traitlist, function(x){
#     rank_tricot(data = dat[dat$gender == gender[i], ],
#                 items = pack_index,
#                 input = x$string,
#                 validate.rankings = TRUE)
#   })
#   
#   k = lapply(r[-reference], function(x){
#     kendallTau(x, r[[reference]])
#   })
#   
#   k = do.call(rbind, k)
#   
#   k$trait = traits[-reference]
#   
#   k$gender = gender[i]
#   
#   kendall = rbind(kendall, k)
#   
# }
# 
# # get the traits with p < 0.1
# keep = kendall$`Pr(>|z|)` < 0.1
# 
# keep = unique(kendall$trait[keep])
# 
# keep = kendall$trait %in% keep
# 
# kendall = kendall[keep, ]
# 
# kendall$trait = factor(kendall$trait, levels = traits)
# 
# # make a bar plot plot 
# kendall_plot = 
#   ggplot(data = kendall, 
#          aes(y = kendallTau,
#              x = trait, 
#              fill = trait)) +
#   geom_chicklet(show.legend = FALSE) +
#   coord_flip() +
#   facet_grid(~ gender) +
#   scale_fill_manual(values = rev(col_pallet(nrow(kendall)))) +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         strip.background =element_rect(fill="white"),
#         text = element_text(color = "grey10"),
#         strip.background.x = element_blank(),
#         strip.placement = "outside",
#         legend.position = "bottom",
#         strip.text = element_text(size = 12, color = "grey10"),
#         legend.text = element_text(size = 12, color = "grey10"),
#         axis.text = element_text(size = 12, color = "grey10"),
#         axis.title = element_text(size = 12, color = "grey10"),
#         legend.title = element_blank()) +
#   labs(x = "",
#        y = "Kendall tau") 
# 
# dir.create("output/groundnut", recursive = TRUE, showWarnings = FALSE)
# 
# ggsave("output/groundnut/kendall-tau-groundnut.pdf",
#        plot = kendall_plot,
#        height = 8,
#        width = 10)
# 
# 
# Coordinates ####
# get the coordinates as an independent data.frame
keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

#dat = dat[, !keep]

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

plot_map(lonlat, c("longitude", "latitude"),
         map_provider = "OpenStreetMap.Mapnik")
# 
# # fix technology names
# unique(dat$package_technology)
# 
# dat$package_technology = ifelse(grepl("Groundnut", dat$package_technology), 
#                                 "Groundnut", 
#                                 dat$package_technology)
# 
# table(dat$package_technology)
# 
