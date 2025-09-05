library("ClimMobTools")

load("processing/trial-data.rda")

ids = unlist(lapply(cmdata, function(x){
  x$project$project_id
}))

dat = read.csv("data/tricot-data-long.csv")

dat = dat[dat$country == "RW", ]

table(dat$coordinator, dat$crop_name)

keep = which(ids %in% dat$trial_id)

rwanda = cmdata[keep]

# this is a huge list as it comes from climmob 
# but you can also put it as a data.frame 
# here I also reconstruct the "block_id"
rwandadf = lapply(rwanda, function(x){
  y = x$project$project_id
  x = as.data.frame(x, pivot.wider = TRUE, tidynames = FALSE)
  x$trial_id = y
  x$block_id = paste(x$package_project_name,
                     x$id, 
                     x$trial_id, sep = "-")
  x
})

block_ids = unlist(lapply(rwandadf, function(x) x$block_id))

table(block_ids %in% dat$block_id)


