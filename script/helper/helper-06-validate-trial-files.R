# .............................................
# Validate every trial file in data/ against metadata/tricot-trial.schema.json
# Run before each release. Files that fail are listed with their errors.
# K de Sousa
library("jsonvalidate")

schema = "metadata/tricot-trial.schema.json"

validate = json_validator(schema, engine = "ajv")

files = list.files("data", pattern = "\\.json$", full.names = TRUE)

result = lapply(files, function(f) {

  ok = validate(f, verbose = TRUE, greedy = TRUE)

  if (ok) return(NULL)

  errors = attr(ok, "errors")

  data.frame(file = basename(f),
             path = errors$instancePath,
             error = errors$message)

})

result = do.call("rbind", result)

if (is.null(result)) {

  cat("All", length(files), "trial files are valid.\n")

} else {

  # one line per file and type of error
  result$path = gsub("/[0-9]+", "/*", result$path)

  result = unique(result)

  cat(length(unique(result$file)), "of", length(files), "trial files have errors.\n")

  print(result, row.names = FALSE)

}
