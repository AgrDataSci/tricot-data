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
library("tidyverse")
library("knitr")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

load("processing/trial-data.rda")

meta = read.csv("data/variety-metadata/PABRA.csv")

techs = unlist(lapply(dat, function(x) unique(x$package_technology)))

keep = which(techs == "Bean")

dat = dat[keep]

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

uniquenames[grep("overall", uniquenames)]

uniquenames[grep("gender", uniquenames)]

# fix some codes that diverged over time
dat = lapply(dat, function(x){
  names(x) = gsub("gender1", "gender", names(x))
  names(x) = gsub("_qst_", "", names(x))
  
  x
})

# put the data together 
dat = rowbind(dat)

dat

table(dat$package_country)

table(dat$package_coordinator)

keep = dat$package_coordinator != "Girum Kifle"

dat = dat[keep, ]

pack_index = paste0("package_item_", letters[1:3])

itemnames = unique(sort(unlist(dat[pack_index])))

itemnames

for (i in seq_along(meta$genotype_name)) {
  dat[pack_index] = lapply(dat[pack_index], function(x){
    x[x == meta$genotype_name[i]] = meta$final_genotype_name[i]
    x[x == "RCB 395"] = NA
    x
  })
}

itemnames = unique(sort(unlist(dat[pack_index])))

itemnames

# check list of varieties tested
items_available = data.frame(items = unlist(dat[pack_index]),
                             country = rep(dat$package_country, times = 3))


items_available = table(items_available$items, items_available$country)

items_available


# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x){
  x$trait_label
}))

sort(traits)

table(dat$package_country)

trait_available = lapply(trait_list, function(x){
  z = data.frame(N = x$keep, country = dat$package_country)
  table(z$N, z$country)[2,]
})

trait_available = do.call("rbind", trait_available)

trait_available = cbind(trait_available, N = rowSums(trait_available))

trait_available = as.data.frame(trait_available)

trait_available$trait = traits

trait_available


keep = dat$package_country == "TZ"

dat = dat[keep, ]


# Coordinates ####
# get the coordinates as an independent data.frame
rownames(dat) = 1:nrow(dat)

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

lon[is.na(lon)] = "registration_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                    latitude = lonlat[cbind(1:nrow(lonlat), lat)])


lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

nas = lonlat$longitude == 0 & lonlat$latitude == 0

lonlat[nas, ] = NA


plot(lonlat)



dir.create("output/bean-eastafrica", recursive = TRUE, showWarnings = FALSE)

trial_map = plot_map(lonlat, c("longitude", "latitude"),
                     map_provider = "OpenStreetMap.Mapnik")


dat = cbind(lonlat, dat)


keep = !grepl("gps_precision|elevation", names(dat))

dat = dat[keep]


table(dat$registration_gender)

dat$registration_gender[dat$registration_gender == "Gender Nonconforming"] = "Woman"


# some analysis 
trait_list
traits
# clean trait names and put them title case
traitlabels = gsub("post_harvest", "post-harvest", traits, perl = TRUE)
traitlabels = gsub("cookingtime", "cooking time", traitlabels, perl = TRUE)
traitlabels = gsub("graincolour", "grain colour", traitlabels, perl = TRUE)
traitlabels = gsub("growagain", "grow again", traitlabels, perl = TRUE)
traitlabels = gsub("droughttolerance", "drought tolerance", traitlabels, perl = TRUE)
traitlabels = gsub("diseasesresistance", "diseases resistance", traitlabels, perl = TRUE)
traitlabels = gsub("diseasesresistance", "diseases resistance", traitlabels, perl = TRUE)
traitlabels = gsub("pestresistance", "pest resistance", traitlabels, perl = TRUE)
traitlabels = gsub("overallchar2", "overall", traitlabels, perl = TRUE)
traitlabels = gsub("flood", "flood ", traitlabels, perl = TRUE)
traitlabels = gsub("plant", "plant ", traitlabels, perl = TRUE)
traitlabels = gsub("size", " size", traitlabels, perl = TRUE)
traitlabels = gsub("_", " ", traitlabels, perl = TRUE)
traitlabels = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", traitlabels, perl = TRUE)

traitlabels

trait_available$trait_name = traitlabels

trait_available$string = trait_available$trait 

trait_available$trait = gsub("Post-harvest |Vegetative |Reproductive ", "", traitlabels)

traitlabels

keep = !grepl(c("Flood Tolerance|Germination|Pest|Maturity|Colour|Cooking|Again"), traitlabels)

trait_available = trait_available[keep, ]

#..........................................................
#..........................................................
#..........................................................
# Organize traits ####
# traits will be grouped as a single trait
# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

trait_list = trait_list[keep]

lapply(trait_list, function(x) x$trait_label)

itemnames = unique(sort(unlist(dat[pack_index])))

#..........................................................
#..........................................................
#..........................................................
# PlackettLuce with aggregated rankings ####
R = list()

trait_unif = unique(trait_available$trait)

for (i in seq_along(trait_unif)) {
  
  print(trait_unif[i])
  
  index = grep(trait_unif[i], trait_available$trait)
  
  indicesbase = 1:nrow(dat)
  
  RG = lapply(trait_list[index], function(x){
    rank_tricot(items = pack_index,
                input = x$string,
                data = dat,
                validate.rankings = TRUE)
  })
  
  RG = do.call("rbind", RG)
  
  RG = group(RG, index = rep(1:nrow(dat), length(index)))
  
  R[[i]] = RG
  
}

traits = unique(trait_available$trait)
traitlabels =  unique(trait_available$trait)

plot(network(R[[1]]))

# ........................................
# .......................................
# ANOVA  ####
# this will be used to test whether the varieties are different 
# from each other and we will retain the ones with alpha > 0.05
# get the reference trait
output = list()

ov = grep("Overall", traits)

pdf(file = "output/bean-eastafrica/trial-network.pdf",
    width = 9,
    height = 9)
plot(network(R[[ov]]))
dev.off()

output$network = network(R[[ov]])

anov = lapply(R, function(x){
  a = anova(PlackettLuce(x))
  a[2, ]
})

anov = do.call("rbind", anov)

anov[,1] = traitlabels

anov

write.csv(anov, 
          "output/bean-eastafrica/anova.csv")

output$anova = anov
# ........................................
# ........................................
# Calculate PL model  ####
ref = "Uyole 03"

mod = lapply(R, PlackettLuce)

worth = 
  worth_map(mod, labels = traitlabels) +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(vjust = 0.5, 
                                   hjust = 1, 
                                   angle = 0)) 


worth

ggsave("output/bean-eastafrica/worth-map-trait-performance.pdf",
       plot = worth,
       height = 15,
       width = 30,
       units = "cm")

output$worth = worth

# # ...........worth# # ........................................
# # ........................................
# # Apply selection index  ####
# coeffs = lapply(mod, function(x){
#   resample(x, bootstrap = TRUE, seed = 1424, n1 = 10, log = FALSE) 
# })
# 
# coeffs = do.call(cbind, coeffs)
# 
# rmv = which(names(coeffs) == "item")[-1]
# 
# coeffs = coeffs[-rmv]
# 
# names(coeffs)[-1] = traitlabels
# 
# for (w in seq_along(trait_available$trait)) {
#   coeffs[, trait_available$trait[w]] = coeffs[,trait_available$trait[w]] * trait_available$w[w]
# }
# 
# coeffs$index = rowSums(coeffs[2:ncol(coeffs)])
# 
# coeffs$subset = "All"
# 
# lvls = unique(coeffs$item[order(coeffs$index)])
# 
# coeffs$item = factor(coeffs$item, levels = lvls)
# 
# ggplot(coeffs, aes(y = item, x = index,
#                    fill = subset, color = subset)) +
#   geom_boxplot(alpha = 0.2) +
#   theme_minimal() +
#   #scale_fill_manual(values = c("grey50","#225ea8", "#e31a1c")) +
#   #scale_color_manual(values = c("grey50","#225ea8", "#e31a1c")) +
#   labs(y = "", x = "Selection index") +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         text = element_text(colour = "grey20"))


# ........................................
# ........................................
# Split the data in subsets  ####
# do it by region
split = dat$registration_gender

region = ifelse(dat$latitude < -6 & dat$longitude < 35.5, "Southern Highlands", 
                ifelse(dat$latitude > -4 & dat$longitude < 34, "Kagera", 
                       "Dodoma"))


plot(dat$longitude, dat$latitude, col = as.factor(region))

split = region

group_split = table(split)

group_split

# Likelihood ratio will check whether the ranks are significantly distinct 
# from each split
llr = lapply(R, function(x){
  likelihood_ratio(x, split = split)
})

llr = do.call("rbind", llr)

llr$trait = traitlabels

llr

write.csv(llr, 
          "output/bean-eastafrica/likelihood-ratio-region.csv")

output$llr_region = llr

# PCA and worth map
n_points = 10
worth_plot = list()

lvls = itemnames

for (i in seq_along(group_split)) {
  
  # run the model 
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })
  
  mod_i = lapply(R_subset, PlackettLuce)
  
  # selection index
  coeffs_i = lapply(mod_i, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = n_points, log = FALSE) 
  })
  
  coeffs_i = do.call(cbind, coeffs_i)
  
  rmv = which(names(coeffs_i) == "item")[-1]
  
  coeffs_i = coeffs_i[-rmv]
  
  names(coeffs_i)[-1] = traitlabels
  
  for (w in seq_along(trait_available$trait)) {
    coeffs_i[, trait_available$trait[w]] = coeffs_i[,trait_available$trait[w]] * trait_available$w[w]
  }
  
  coeffs_i$index = rowSums(coeffs_i[2:ncol(coeffs_i)])
  
  coeffs_i$subset = names(group_split[i])
  
  coeffs_i$item = factor(coeffs_i$item, levels = lvls)

  worth_plot[[i]] = 
    worth_map(mod_i, labels = traitlabels) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, 
                                     hjust = 1, 
                                     angle = 0)) +
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  if(i == 1) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(legend.position = "none")
    
  }
  
  if(i == 2) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(axis.text.y = element_blank()) +
      theme(legend.position = "none")
  }
  
  if(i == 3) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(axis.text.y = element_blank()) 
  }
  
}

p = worth_plot[[1]] + worth_plot[[2]] + worth_plot[[3]] + plot_layout(ncol = 3) 

p

ggsave("output/bean-eastafrica/worth-map-trait-performance-region.pdf",
       plot = p,
       height = 15,
       width = 40,
       units = "cm")

output$worth_region = p

# ........................................
# ........................................
# Split the data in subsets  ####
# do it by clusters
split = dat$registration_gender

group_split = table(split)

group_split

# Likelihood ratio will check whether the ranks are significantly distinct 
# from each gender
llr = lapply(R, function(x){
  likelihood_ratio(x, split = split)
})

llr = do.call("rbind", llr)

llr$trait = traitlabels

llr

write.csv(llr, 
          "output/bean-eastafrica/likelihood-ratio-gender.csv")


output$llr_gender = llr

# PCA and worth map
n_points = 10
worth_plot = list()

for (i in seq_along(group_split)) {
  
  # run the model 
  R_subset = lapply(R, function(x){
    x = x[split == names(group_split[i])]
    x = na.omit(x)
    x
  })
  
  mod_i = lapply(R_subset, PlackettLuce)
  
  # selection index
  coeffs_i = lapply(mod_i, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = n_points, log = FALSE) 
  })
  
  coeffs_i = do.call(cbind, coeffs_i)
  
  rmv = which(names(coeffs_i) == "item")[-1]
  
  coeffs_i = coeffs_i[-rmv]
  
  names(coeffs_i)[-1] = traitlabels
  
  for (w in seq_along(trait_available$trait)) {
    coeffs_i[, trait_available$trait[w]] = coeffs_i[,trait_available$trait[w]] * trait_available$w[w]
  }
  
  coeffs_i$index = rowSums(coeffs_i[2:ncol(coeffs_i)])
  
  coeffs_i$subset = names(group_split[i])
  
  coeffs_i$item = factor(coeffs_i$item, levels = lvls)
  
  worth_plot[[i]] = 
    worth_map(mod_i, labels = traitlabels) +
    labs(x = "", y = "") +
    theme(axis.text.y = element_text(vjust = 0.5, 
                                     hjust = 1, 
                                     angle = 0)) +
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(group_split[i]),
                        ", n = ",
                        as.integer(group_split[i])))
  
  if(i > 1) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(axis.text.y = element_blank())
    
  }
  
  if(i < length(group_split)) {
    worth_plot[[i]] =
      worth_plot[[i]] +
      theme(legend.position = "none")
  }
  
}

p = worth_plot[[1]] + worth_plot[[2]]

p

output$worth_gender = p

ggsave("output/bean-eastafrica/worth-map-trait-performance-gender.pdf",
       plot = p,
       height = 15,
       width = 35,
       units = "cm")



# mod = lapply(R, PlackettLuce)
# 
# for(i in seq_along(trait_unif)) {
#   p = plot(mod[[i]], ref = ref) +
#     labs(title = trait_unif[i])
# 
#   ggsave(filename = paste0("output/bean-eastafrica/log-worth-", gsub(" ", "", trait_unif[i]), ".pdf"),
#          width = 20,
#          height = 20,
#          units = "cm",
#          plot = p)
# }



nrow(as.rankings(R[[ov]]))




# Kendall tau

O = rank_tricot(data = dat, 
                items = pack_index,
                trait_list[[7]]$string,
                validate.rankings = TRUE)

R2 = R[-ov]

labs = traitlabels[-ov]

kendall = data.frame()

for(i in seq_along(R2)) {
  
  x = as.rankings(R2[[i]])
  
  n = nrow(x) / nrow(dat)
  
  if(n > 1) {
    for(j in seq_len(n)) y = rbind(O, O[1:nrow(dat),]) 
  } else {
    y = O
  }
  
  all(dim(x) == dim(y))
  
  k = 10
  n = nrow(y)
  set.seed(1852)
  folds = sample(rep(1:k, times = ceiling(n/k), length.out = n))
  
  kt = kendallTau(y, x, average = FALSE)
  
  e = NULL

  for(j in seq_len(k)) {
    m = mean(kt$kendallTau[folds != j], na.rm = TRUE)
    e = c(e, m)
  }

  kt = data.frame(kendall = e, trait = labs[i])
  
  kendall = rbind(kendall, kt)
  
}

levels = 
  kendall %>% 
  group_by(trait) %>% 
  summarise(mean = mean(kendall)) %>% 
  ungroup() %>% 
  arrange(mean) 

kendall$trait = factor(kendall$trait, levels = levels$trait)

p = 
ggplot(kendall, aes(x = kendall, y = trait, fill = "white")) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer() +
  theme_bw() +
  labs(x = "Correlation with overall preference", y = "") +
  theme(text = element_text(size = 15))

p

output$kendall = p

ggsave("output/bean-eastafrica/kendall-correlation.pdf",
       plot = p,
       height = 15,
       width = 20,
       units = "cm")


rmarkdown::render("script/mainreport.Rmd",
                  output_dir = "output/bean-eastafrica/",
                  output_format = "html_document",
                  output_file = "report.html")

