# ..........................................
# ..........................................
# Read and clean potato data in Central America
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
library("jsonlite")
source("script/helper01_functions.R")

capture.output(sessioninfo::session_info(),
               file = "script/sessioninfo/02_organize_potato.txt")

#######
ids <- list.dirs("data/raw/potato", full.names = TRUE)[-1]

lt <- list()
quest <- data.frame()

for(i in seq_along(ids)) {
  
  x <- list.files(ids[i], pattern = ".json", full.names = TRUE)[1]
  
  x <- fromJSON(x)
  
  quest <- rbind(quest, x$specialfields)
  
  class(x) <- union("CM_list", class(x))
  
  x <- as.data.frame(x, pivot.wider = TRUE, tidynames = TRUE)
  
  names(x) <- make_clean_names(names(x))
  
  names(x) <- gsub("75days", "day75", names(x))
  
  lt[[i]] <- x
}

cmdata <- rowbind(lt)

names(cmdata)

quest

# remove duplicates in quest
questions <- quest[!duplicated(quest$desc), ]

# ................................
# Make list of parameters ####
questions$name[which(grepl("_pos$", questions$name))]

# look for the the pattern in cmdata as some traits were asked twice
traitpattern <- names(cmdata)[which(grepl("_pos$", names(cmdata)))]
traitpattern <- gsub("_pos$", "", traitpattern)
traitpattern <- unique(traitpattern)
traitpattern <- rev(sort(traitpattern))

traitpattern

newname <- c("Disease insect", "Bacterial wilt",
             "Plant vigour", "Disease insect",
             "Bacterial wilt", "Preference",
             "Plant future", "Overall preference",
             "Overall performance", "Dormancy",
             "Taste",   "Marketability",
             "Dormancy" , "Yield",
             "Tuber size", "Tuber appearance",
             "Taste", "Maturity",
             "Marketability")

length(traitpattern) == length(newname)

traits <- data.frame()

for(i in seq_along(traitpattern)){
  index <- traitpattern[i]
  
  pos <- which(grepl(paste0(index, "_pos"), names(cmdata)))
  neg <- which(grepl(paste0(index, "_neg"), names(cmdata)))
  
  trait_i <- strsplit(index, "_")[[1]]
  trait_i <- trait_i[length(trait_i)]
  
  assess <- strsplit(index, "_")[[1]]
  assess <- paste(assess[-c(length(assess)-1, length(assess))], collapse = "-")
  
  
  tr <- data.frame(nameString1 = names(cmdata)[pos],
                   nameString2 = names(cmdata)[neg],
                   nQst = 2,
                   name = newname[i],
                   codeQst = gsub(" ", "", ClimMobTools:::.title_case(newname[i])),
                   questionAsked1 = questions$desc[which(grepl(paste0(trait_i, "_pos"), questions$name))],
                   questionAsked2 = questions$desc[which(grepl(paste0(trait_i, "_neg"), questions$name))],
                   assessmentId = assess,
                   assessmentName = ClimMobTools:::.title_case(assess),
                   assessmentDay = "0",
                   traitOrder = "otherTraits")
  
  traits <- rbind(traits, tr)
  
}

# define the reference trait
traits[which(grepl("post_harvest1_5daysafterharvest_yield_pos", traits$nameString1)), "traitOrder"] <- "referenceTrait"
#traits[which(grepl("post_harvest3_60daysafterharvest_preference_pos", traits$nameString1)), "traitOrder"] <- "referenceTrait"



traits$assessmentName <- factor(traits$assessmentName, 
                                levels = c("Vegetative1", "Vegetative2", 
                                           "Post-harvest1", "Post-harvest2", "Post-harvest3"))

traits <- traits[order(traits$assessmentName),]

tricotVSlocal <- character()

# select the covariates
# combine with lonlat data
cmdata$id <- paste(cmdata$id, cmdata$package_project_name, sep = "-")

cmdata <- merge(cmdata, lonlat, by = "id", all.x = TRUE)


names(cmdata)[which(!grepl("_pos$|_neg$", names(cmdata)))]




covariates <- data.frame(assessmentId = "43b9cf61d6",
                         assessmentName = "Data collection",
                         codeQst = c("MLWS", "DTR"),
                         id = "",
                         nameString = c("MLWS", "DTR"),
                         name = "",
                         questionAsked = "")


covariates$name <- covariates$codeQst
covariates$questionAsked <- covariates$codeQst 

pars <- list(traits = traits, tricotVSlocal = tricotVSlocal, covariates = covariates)
rm(tr, traits, tricotVSlocal, covariates, i, traitpattern, newname, index)
