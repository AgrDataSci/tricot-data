# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required
# load packages
library("ClimMobTools")
library("gosset")

# API key
key = ""

# get the list of projects from the user indicated in the API key
projects = getProjectsCM(key, server = "1000farms")

users = c("Happy")

keep = projects$user_owner %in% users

projects = projects[keep, ]

# fetch the data from climmob
cmdata = list()

for(i in seq_along(projects$project_id)) {
  print(i)
  d_i = getDataCM(key, 
                  project = projects$project_code[i],
                  userowner = projects$user_owner[i],
                  server = projects$server[i],
                  as.data.frame = FALSE)
  
  cmdata[[i]] = d_i
  
}

dat = lapply(cmdata, function(x){
  as.data.frame(x, 
                pivot.wider = TRUE,
                tidynames = TRUE)
})








