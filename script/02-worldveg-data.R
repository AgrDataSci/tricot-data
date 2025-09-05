library("ClimMobTools")
library("sf")
library("readxl")
library("jsonlite")
library("PlackettLuce")
library("gosset")

load('raw/trial-data.rda')

geno = read_excel('raw/variety-metadata/avisa-crops-metadata/worldveg.xlsx')

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

for(k in seq_along(cmdata)) {
  
  x = cmdata[[k]]
  
  meta = exportTrialMetadata(x)
  
  rank = exportTricotRanks(x)
  
  measu = exportMeasuredTraits(x)
  
  plot = rbind(rank, measu)
  
  rownames(plot) = 1:nrow(plot)
  
  block = exportBlockData(x)
  
  variables = exportVariablesDescription(x, rank, measu, block)
  
  # clean variety names
  for(i in seq_along(geno$genotype_name)) {
    plot$genotype_name = ifelse(geno$genotype_name[i] == plot$genotype_name, 
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
  
  meta$program = "World Vegetable Center"
  
  meta$taxon = "Vigna unguiculata"
  
  meta$crop_name = "cowpea"
  
  # PlackettLuce analysis
  rank$traitmoment = paste(rank$collection_moment, rank$trait, sep = " - ")
  
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
    
    print(sum(ties))
    
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
             path = paste0("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/tricot-data-v1/tests/",
                           filename, 
                           ".json"),
             pretty = TRUE,
             auto_unbox = TRUE)
  
}

