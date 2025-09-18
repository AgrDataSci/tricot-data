# .............................................
# This script will handle the WorldVeg data #####
# first run: July 2025
# updated: September 2025
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

available = read.csv("data/aa-available-datasets.csv")

# read file with genotype metadata
geno = read_excel('metadata/trials-metadata/worldveg/genotype-metadata.xlsx')
geno$final_genotype_name = ifelse(is.na(geno$final_genotype_name), 
                                  geno$genotype_name, 
                                  geno$final_genotype_name)

doi = "https://doi.org/10.5281/zenodo.17112492"

experimental_site = "farm"

trial_type = "on-farm"

trial_objective = "variety introduction"

unit_of_analysis = "genotype"

trial_description = paste("Strengthen seed systems of African vegetables and scale variety adoption so",
                  "that farmers and urban consumers benefit from increased production and",
                  "consumption of African vegetables, which are nutritious but currently underutilized.")


institute = "World Vegetable Center"

pi = "Sognigbé N'Danikou"

pi_email = "sognigbe.ndanikou@worldveg.org"

program = "Traditional African Vegetables"

ror_id = "https://ror.org/05dvmy761"

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

keep = !grepl("eggplant", projects$project_name)

projects = projects[keep, ]

cmdata = cmdata[keep]

cmdata

for(k in seq_along(cmdata)) {
  
  x = cmdata[[k]]
  
  meta = exportTrialMetadata(x)
  
  # add some metadata manually
  meta$identifier = doi
  
  meta$data_producer$name = institute
  
  meta$data_producer$identifier = ror_id
  
  meta$data_producer$program = program
  
  meta$data_producer$principal_investigator = pi
  
  meta$data_producer$email = pi_email
  
  meta$study$experimental_site = experimental_site
  
  meta$study$type = trial_type
  
  meta$study$objective = trial_objective
  
  meta$study$description = trial_description
  
  meta$study$unit_of_analysis = unit_of_analysis
  
  meta$crop$name = ifelse(meta$crop$name == "Amaranths", "Amaranth", meta$crop$name)
  
  meta$crop$name = gsub(" ", "", meta$crop$name)
  
  meta$crop$name = tolower(meta$crop$name)
  
  meta$crop$taxon = ifelse(meta$crop$name == "amaranth", "Amaranthus spp.",
                      ifelse(meta$crop$name == "okra", "Abelmoschus spp.",
                             ifelse(meta$crop$name == "jutemallow", "Corchorus spp.", 
                             "Not provided")))
  
  # ....................................
  # ....................................
  # get ranking data 
  rank = exportTricotRanks(x, nmin = 0.2)
  
  if (nrow(rank) == 0) next
  
  rank = rank[rank$trait != "yieldqualitative", ]
  
  # remove ties 
  # keep only block x traits with >1 distinct value
  rank = 
    rank %>%
    group_by(block_id, collection_moment, trait) %>%
    filter(n_distinct(value) > 1) %>%   
    ungroup()
  
  # ....................................
  # ....................................
  # other non-tricot traits
  measu = exportMeasuredTraits(x)
  
  # keep only block x traits with at least one entry (no NA)
  measu = 
    measu %>%
    group_by(block_id, collection_moment, trait) %>%
    filter(!all(is.na(value))) %>%   
    ungroup()
  
  # combine tricot and non-tricot traits
  plot = as.data.frame(rbind(rank, measu))
  
  rownames(plot) = 1:nrow(plot)
  
  unique(plot$trait)
  
  # ....................................
  # ....................................
  # all available non-PII block data
  block = exportBlockData(x)
  
  # ....................................
  # ....................................
  # descriptors for variables in both plot and block data 
  variables = exportVariablesDescription(x, rank, measu, block)
  
  # until data backward merging is not implemented we need to remove the 
  # 'fr' string from some trials 
  # the variable descriptions will still be in french but this will 
  # make easier to merge with other datasets
  variables$variable_name = sub("fr$", "", variables$variable_name)
  
  rank$trait = sub("fr$", "", rank$trait)
  
  measu$trait = sub("fr$", "", measu$trait)
  
  plot$trait =  sub("fr$", "", plot$trait)
  
  names(block) = sub("fr$", "", names(block))
  
  # ....................................
  # ....................................
  # clean genotype names
  for(i in seq_along(geno$genotype_name)) {
    plot$genotype_name = ifelse(geno$genotype_name[i] == plot$genotype_name &
                                  geno$crop_name[i] == meta$crop$name, 
                                geno$final_genotype_name[i],
                                plot$genotype_name)
  }
  
  
  # new table using final genotype names to be added to the metadata
  genotypes = data.frame(genotype_name = unique(plot$genotype_name),
                         role = NA,
                         release_year = NA,
                         crossing_year = NA,
                         target_trait = NA,
                         origin = NA, 
                         remarks = NA)
  
  geno = geno[!duplicated(geno$final_genotype_name), ]
  
  # get genotype information from the source table
  for(i in seq_along(genotypes$genotype_name)) {
    
    index = grep(genotypes$genotype_name[i], geno$final_genotype_name)
    
    if(length(index) == 0) next
    
    genotypes[i, 2:7] = geno[index, c("entry_type", "release_year", "crossing_year",
                                      "target_trait", "origin", "remarks")]
    
  }
  
  genotypes[is.na(genotypes)] = "No information provided"
  
  
  # add both genotype and variable metadata to the main metadata list
  meta$genotypes = genotypes  
  
  meta$variables = variables
  
  # ....................................
  # ....................................
  # PlackettLuce analysis
  rank = plot[plot$value_type == "rank", ]
  
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
  
  ref = genotypes$genotype_name[grep("check", genotypes$role)[1]]
  
  if (is.na(ref)) ref = 1
  
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
  
  # ....................................
  # ....................................
  # prepare the data to export
  data_export = list(metadata = meta,
                     block_data = block,
                     plot_data = plot,
                     rank_analysis = mod)
  
  filename = paste(data_export$metadata$crop$name, 
                   data_export$metadata$study$id,
                   #data_export$metadata$changelog$version,
                   sep = "-")
  
  write_json(data_export,
             path = paste0("data/", filename, ".json"),
             pretty = TRUE,
             auto_unbox = TRUE)
  

  # ....................................
  # ....................................
  # write up / update some summary tables to added to the README file
  # add coordinates to file to write the main map
  coords = data.frame(block_id = block$block_id,
                      crop_name = meta$crop$name,
                      longitude = block$longitude,
                      latitude = block$latitude)
  
  xy = rbind(xy, coords)  
  
  # summary table with available datasets
  avail = data.frame(study_id = meta$study$id,
                     crop_name = meta$crop$name,
                     taxon = meta$crop$taxon,
                     data_producer_institute = meta$data_producer$name,
                     country = meta$study$country,
                     start_date = meta$date$start,
                     type = meta$study$type,
                     participants = meta$participants$total,
                     filename = paste0(filename, ".json"),
                     check.names = FALSE)
  
  available = rbind(available, avail)
  
}

# remove duplicated entries
xy = xy[!duplicated(xy$block_id),]
# round up coordinates
xy[c("longitude","latitude")] = lapply(xy[c("longitude","latitude")], function(x) round(x, 2))
# overwrite the file
write.csv(xy, file = "docs/trial-xy.csv", row.names = FALSE)

# remove duplicated entries
available = available[!duplicated(available$study_id), ]

write.csv(available, file = "data/aa-available-datasets.csv", row.names = FALSE)



