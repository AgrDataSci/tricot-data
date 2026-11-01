# .............................................
# Write .zenodo.json from metadata/project-metadata.xlsx
# Zenodo's GitHub integration reads .zenodo.json in its own deposit
# format (title, upload_type, creators, license), not in DataCite format.
# A file in the wrong format is ignored without warning, and Zenodo then
# falls back to CITATION.cff or the GitHub license.
# K de Sousa
library("readxl")
library("jsonlite")

# Read metadata and authors from Excel
file = "metadata/project-metadata.xlsx"
metadata = read_excel(file, sheet = "metadata")

trials = list.files("metadata/trials-metadata", full.names = TRUE)

authors = list()

for(i in seq_along(trials)) {
  authors[[i]] = read_excel(paste0(trials[i], "/authors-metadata.xlsx"))
}

authors = do.call("rbind", authors)

authors = authors[union(1:2, order(authors$family_name)), ]

# the same person may appear in several trials
authors = authors[!duplicated(paste(authors$given_name, authors$family_name)), ]

funders = read_excel(file, sheet = "funders")
funders = funders[!is.na(funders$funder_name), ]

communities = read_excel(file, sheet = "communities")
communities = communities[!is.na(communities$community_id), ]

# --- 1. Validate

required_fields = c("title", "version", "publication_date", "description",
                    "language", "resource_type", "license", "github_repo")
missing_fields = setdiff(required_fields, metadata$key)
if (length(missing_fields) > 0) {
  stop("Missing required metadata fields: ", paste(missing_fields, collapse = ", "))
}

orcid_ok = function(x) !is.na(x) & grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x)

if (any(!is.na(authors$orcid) & !orcid_ok(authors$orcid))) {
  cat("Warning: some ORCIDs are invalid and will be omitted:\n")
  print(authors[!is.na(authors$orcid) & !orcid_ok(authors$orcid), c("given_name", "family_name", "orcid")])
}

if (any(is.na(authors$given_name) | is.na(authors$family_name))) {
  stop("Some authors are missing given or family names.")
}

# --- 2. Build the Zenodo deposit metadata

get_meta = function(key) {
  val = metadata$value[metadata$key == key]
  if (length(val) == 0) return(NULL)
  as.character(val)
}

creators = lapply(seq_len(nrow(authors)), function(i) {
  row = authors[i, ]
  affils = unname(na.omit(unlist(row[, grepl("^affiliation_", names(row))])))
  creator = list(name = paste(row$family_name, row$given_name, sep = ", "))
  if (length(affils) > 0) creator$affiliation = paste(affils, collapse = "; ")
  if (orcid_ok(row$orcid)) creator$orcid = row$orcid
  creator
})

funding_note = paste0("Funding: ",
                      paste(paste0(funders$funder_name, ", ", funders$grant_title,
                                   " (", funders$grant_number, ")"),
                            collapse = "; "), ".")

zenodo = list(
  title = get_meta("title"),
  upload_type = get_meta("resource_type"),
  description = get_meta("description"),
  publication_date = get_meta("publication_date"),
  version = get_meta("version"),
  language = get_meta("language"),
  license = get_meta("license"),
  access_right = "open",
  keywords = as.list(strsplit(get_meta("subjects"), ";\\s*")[[1]]),
  creators = creators,
  notes = funding_note,
  related_identifiers = list(list(
    identifier = get_meta("github_repo"),
    relation = "isSupplementedBy",
    resource_type = "software"
  ))
)

if (nrow(communities) > 0) {
  zenodo$communities = lapply(communities$community_id, function(x) list(identifier = x))
}

write(toJSON(zenodo, pretty = TRUE, auto_unbox = TRUE), ".zenodo.json")
