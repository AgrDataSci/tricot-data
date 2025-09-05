# This script will handle the WorldVeg data #####
# first run: 5 of September 2025
# updated: 5 of September 2025
# K de Sousa
library("ClimMobTools")
library("sf")
library("readxl")
library("jsonlite")
library("PlackettLuce")
library("gosset")
library("tidyverse")

load('raw/trial-data.rda')

xy = read.csv("docs/trial-xy.csv") 

# read file with genotype metadata
geno = read_excel('raw/variety-metadata/worldveg.xlsx')
geno$final_genotype_name = ifelse(is.na(geno$final_genotype_name), geno$genotype_name, geno$final_genotype_name)

license = "CC BY-SA 4.0"

doi = "Pending"

objective = paste("Strengthen seed systems of African vegetables and scale variety adoption so",
                  "that farmers and urban consumers benefit from increased production and",
                  "consumption of African vegetables, which are nutritious but currently underutilized.")

# filter the worlveg projects
keep = grep("@worldveg.org", projects$email)

projects = projects[keep, ]

cmdata = cmdata[keep]

# x = do.call("rbind", lapply(cmdata, function(x){do.call("rbind", x$combination$elements)}))
# x$technology_name[x$technology_name == "Amaranths"] = "Amaranth"
# x$id = paste0(x$technology_name, x$alias_name)
# x = x[!duplicated(x$id), ]

# remove eggplant piment and tomate
projects$project_name

keep = !grepl("piment|tomate|eggplant", projects$project_name)

projects = projects[keep, ]

cmdata = cmdata[keep]

cmdata

k = 1

for(k in seq_along(cmdata)) {
  
  x = cmdata[[k]]
  
  meta = exportTrialMetadata(x)
  
  rank = exportTricotRanks(x)
  
  # remove ties 
  # keep only block x traits with >1 distinct value
  rank = 
    rank %>%
    group_by(block_id, collection_moment, trait) %>%
    filter(n_distinct(value) > 1) %>%   
    ungroup()
  
  measu = exportMeasuredTraits(x)
  
  # keep only block x traits with at least one entry (no NA)
  measu = 
    measu %>%
    group_by(block_id, collection_moment, trait) %>%
    filter(!all(is.na(value))) %>%   
    ungroup()
  
  plot = as.data.frame(rbind(rank, measu))
  
  rownames(plot) = 1:nrow(plot)
  
  block = exportBlockData(x)
  
  variables = exportVariablesDescription(x, rank, measu, block)
  
  # check the crop name
  meta$crop_name = ifelse(meta$crop_name == "Amaranths", "Amaranth", meta$crop_name)
  
  meta$crop_name = tolower(meta$crop_name)
  
  # clean variety names
  for(i in seq_along(geno$genotype_name)) {
    plot$genotype_name = ifelse(geno$genotype_name[i] == plot$genotype_name &
                                  geno$crop_name[i] == meta$crop_name, 
                                geno$final_genotype_name[i],
                                plot$genotype_name)
  }
  
  
  genotypes = data.frame(genotype_name = unique(plot$genotype_name),
                         role_in_trial = NA,
                         year_release = NA,
                         market_segment = NA,
                         country_origin = NA, 
                         remarks = NA)
  
  geno = geno[!duplicated(geno$final_genotype_name), ]
  
  for(i in seq_along(genotypes$genotype_name)) {
    
    index = grep(genotypes$genotype_name[i], geno$final_genotype_name)
    
    if(length(index) == 0) next
    
    genotypes[i, c(2, 4, 6)] = geno[index, c("entry_type", "target_trait", "remarks")]
    
  }
  
  genotypes[is.na(genotypes)] = "No information provided"
  
  meta$genotypes = genotypes  
  
  meta$variables = variables
  
  meta$data_producer_institute = "World Vegetable Center"
  
  meta$license = license
  
  meta$doi = doi
  
  meta$program = "World Vegetable Center"
  
  meta$taxon = ifelse(meta$crop_name == "amaranth", "Amaranthus spp.",
                      ifelse(meta$crop_name == "okra", "Abelmoschus esculentus",
                             ifelse(meta$crop_name == "jute mallow", "Corchorus spp."), 
                             "Not provided"))
  
  meta$trial_objective = objective
  
  # PlackettLuce analysis
  rank$traitmoment = paste(rank$collection_moment, rank$trait, sep = " - ")
  
  rank$block_id = as.factor(rank$block_id)
  
  traits = unique(rank$traitmoment)
  
  R = vector(mode = "list", length = length(traits))
  
  for (i in seq_along(traits)) {
    
    dat_i = subset(rank, rank$traitmoment == traits[i])
    
    R[[i]] = rank_numeric(data = dat_i,
                          items = "genotype_name",
                          input = "value",
                          id = "block_id",
                          ascending = TRUE)
    
    # remove rows with ties 
    ties = unclass(R[[i]])
    
    ties[ties == 0] = NA
    
    ties = apply(ties, 1, function(x) any(duplicated(na.omit(x))))
    
    R[[i]] = R[[i]][!ties, ]
    
  }
  
  ref = genotypes$genotype_name[grep("check", genotypes$role_in_trial)[1]]
  
  mod = lapply(R, PlackettLuce)
  
  mod = lapply(mod, function(x) {
    x = qvcalc(x, ref = ref)$qvframe
    x = cbind(genotype_name = rownames(x), x)
    x
  })
  
  mod = do.call("rbind", mod)
  
  mod = cbind(collection_moment = rep(traits, each = ncol(R[[i]])),
              mod)
  
  
  mod$trait = gsub(".*- ", "", mod$collection_moment) 
  
  mod$collection_moment = gsub(" -.*", "", mod$collection_moment)
  
  mod = mod[union(c("collection_moment", "trait", "genotype_name"), names(mod))]
  
  rownames(mod) = 1:nrow(mod)
  
  data_export = list(metadata = meta,
                     block_data = block,
                     plot_data = plot,
                     rank_analysis = mod)
  
  filename = paste(data_export$metadata$crop_name, 
                   data_export$metadata$trial_id,
                   data_export$metadata$changelog$version,
                   sep = "-")
  
  write_json(data_export,
             path = paste0("data/", filename, ".json"),
             pretty = TRUE,
             auto_unbox = TRUE)
  

  # add coordinates to file to write the main map
  coords = data.frame(block_id = block$block_id,
                      crop_name = meta$crop_name,
                      longitude = block$longitude,
                      latitude = block$latitude)
  

  xy = rbind(xy, coords)  
  
}


write.csv(xy, "docs/trial-xy.csv")



