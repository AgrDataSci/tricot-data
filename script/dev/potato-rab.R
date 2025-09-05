library("ClimMobTools")

key = c("3d2ac674-f8c6-4666-9436-2219d8b1dc12")

k1 = getProjectsCM(key, server = "climmob3")

k1

keep = k1$project_id %in% c("POT21B", "Pot21A")

k1 = k1[keep, ]

dat_list = list()

for(i in seq_along(k1$project_id)) {
  dat_list[[i]] = getDataCM(key = key, 
                            project =  k1$project_id[i],
                            server = "climmob3",
                            userowner = k1$user_owner[i],
                            as.data.frame = TRUE,
                            tidynames = TRUE,
                            pivot.wider = TRUE)
}

