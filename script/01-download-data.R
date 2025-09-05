# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required
# load packages
library("ClimMobTools")
library("gosset")
library("janitor")

# read file with API key, this is a txt file 
# indicating the key(s) and the server from which the data should 
# be retrieved. Its a matrix with two columns, the first being the key and the second 
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

# remove projects with keyword exercise
projects$keywords = tolower(projects$keywords)
rmkeywords = c("chocolate","egg","exercise","excercice","excerise",
               "example","playground","excesise","exrcsie","exersice","test",
               'choco','exercise','trianing','exercice','1234',"training")
rmkeywords = paste(rmkeywords, collapse = "|")
keep = !grepl(rmkeywords, projects$keywords)

sum(keep)

projects$project_name = tolower(projects$project_name)
rmtitles = c("chocolate", "exercice", "consumer")
rmtitles = paste(rmtitles, collapse = "|")

keep = !grepl(rmtitles, projects$project_name) & keep

# remove users that are not providing real projects
unique(projects$user_owner)

rmusers = c("adolph1", "aikurawa", "akmue", "akmueller", "alicekamara",
            "Berta","caghogho", "jacobvanetten","student","VioletLasdun", 
            "acoto", "cquiros", "carlos28","Dorsil","fitsum", "almendra")

keep = !projects$user_owner %in% rmusers & keep

projects = projects[keep, ]

# fetch the data from climmob
cmdata = list()

for(i in seq_along(projects$project_id)) {
  print(i)
  d_i = getDataCM(projects$key[i], 
                  project = projects$project_code[i],
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

# save it 
dir.create("processing", showWarnings = FALSE)

# write.csv(dat, "processing/climmob-projects-registration-dates.csv", row.names = FALSE)

# # get the other data set
# dat = lapply(cmdata, function(x){
#   x = as.data.frame(x, 
#                     tidynames = TRUE,
#                     pivot.wider = TRUE)
#   
#   names(x) = make_clean_names(names(x))
#   
#   x
#   
# })

save(projects, questions, cmdata, file = "processing/trial-data.rda")


