# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required
# load packages
library("ClimMobTools")
library("gosset")
library("readxl")

# read file with API key, this is a txt file 
# indicating the key(s) and the server from which the data should 
# be retrieved. Its a matrix with two coluns, the first being the key and the second 
# being the server
pj = read_excel("data/projects-climmob3-to-remove.xlsx")

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
rmtitles = c("chocolate", "exercice")
rmtitles = paste(rmtitles, collapse = "|")

keep = !grepl(rmtitles, projects$project_name) & keep

#projects = projects[keep, ]

# remove users that are not providing real projects
unique(projects$user_owner)

rmusers = c("adolph1", "aikurawa", "akmue", "akmueller", "alicekamara",
            "amavilysaglinglo", "Berta","caghogho",
            "jacobvanetten","student","VioletLasdun", 
            "acoto", "cquiros", "carlos28","Dorsil","fitsum")

keep = !projects$user_owner %in% rmusers & keep

y = projects[keep, ]


x = projects[!keep, ]

names(x)[names(x) == "project_id"] = "project_cod"

names(x)[names(x) == "coordinator"] = "project_pi"

names(x)[names(x) == "keywords"] = "project_tags"


pj$server = "1000FARMS"


z = rbind(x[,c("project_cod", "project_pi", "project_tags", "server")],
          pj[,c("project_cod", "project_pi", "project_tags", "server")])

z$id = paste(z$project_cod, z$project_pi)

keep = !duplicated(z$id)

z = z[keep, ]


write.csv(z, 'output/project-remove-climmob3.csv', row.names = F)


