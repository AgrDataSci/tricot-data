# .............................................
# Build the dataset index (datasets.csv) from the trial files in data/
# The index is the entry point for users and AI assistants. It lets them
# pick a trial file by crop, country, period and ranked traits without
# opening the JSON files.
# Run this after every export script, before a release.
# K de Sousa
library("jsonlite")

files = list.files("data", pattern = "\\.json$", full.names = TRUE)

collapse = function(x) {
  x = unique(x[!is.na(x) & x != ""])
  paste(sort(x), collapse = "; ")
}

index = lapply(files, function(f) {

  dat = fromJSON(f)

  meta = dat$metadata

  plots = dat$plot_data

  ranks = plots[plots$value_type == "rank", ]

  data.frame(study_id = meta$study$id,
             crop_name = meta$crop$name,
             taxon = meta$crop$taxon,
             country = meta$study$country,
             data_producer_institute = meta$data_producer$name,
             start_date = as.character(as.Date(meta$date$start)),
             end_date = as.character(as.Date(meta$date$end)),
             type = meta$study$type,
             objective = meta$study$objective,
             participants = meta$participants$total,
             n_blocks = length(unique(dat$block_data$block_id)),
             n_genotypes = length(unique(plots$genotype_name)),
             collection_moments = collapse(ranks$collection_moment),
             ranked_traits = collapse(ranks$trait),
             filename = basename(f),
             size_mb = round(file.size(f) / 1e6, 1),
             license = meta$license,
             check.names = FALSE)

})

index = do.call("rbind", index)

index = index[order(index$crop_name, index$start_date, index$study_id), ]

write.csv(index, file = "datasets.csv", row.names = FALSE, na = "")
