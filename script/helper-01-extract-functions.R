
metadata
│   │   ├── version
│   │   ├── trial name
│   │   ├── trial description
│   │   ├── date YYYY-MM-DD
│   │   ├── coumtry ISO2
│   │   ├── coordinates centroid (00.000, 00.000)
│   │   ├── data producer name
│   │   ├── data producer email
│   │   ├── data producer institute
│   │   ├── program (ADCIN, RTB, BOLDER)
│   │   ├── crop name
│   │   ├── taxon
│   │   ├── trial objective
│   │   ├── N participants
│   │   ├── N men
│   │   ├── N women

x = cmdata[[1]]

get_trial_metadata = function(x){
  ClimMobTools:::.safe_extract(x, "project", "project_name")
  
  crop_name = ""
  taxon = ""
  trial_objective = ""
  nparticipants = try(length(x$data$REG_gender1), silent = TRUE)
  nmen = try(sum(x$data$REG_gender1 == 2), silent = TRUE)
  nwomen = try(sum(x$data$REG_gender1 == 1), silent = TRUE)
}











