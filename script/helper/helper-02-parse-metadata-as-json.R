# Load required packages
library("readxl")
library("jsonlite")


# Path to your Excel file
file = "metadata/project-metadata.xlsx"

# --- 1. Read sheets ---
metadata = read_excel(file, sheet = "metadata")
authors = read_excel(file, sheet = "authors")
funders = read_excel(file, sheet = "funders")
communities = read_excel(file, sheet = "communities")
dates = read_excel(file, sheet = "dates")

keep = grep("1", authors$checked_dataset)

authors = authors[keep, ]

# Check required fields in metadata
required_fields = c("title", "version", "publication_date", "publisher", "description", "language", "resource_type", "license")
missing_fields = setdiff(required_fields, metadata$key)
if (length(missing_fields) > 0) {
  cat("Missing required metadata fields:", paste(missing_fields, collapse = ", "), "\n")
} else {
  cat("All required metadata fields are present.\n")
}

# Check ORCID format
if ("orcid" %in% names(authors)) {
  orcid_invalid = authors[!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{4}$", authors$orcid), ]
  if (nrow(orcid_invalid) > 0) {
    cat("Invalid ORCID(s):\n")
    print(orcid_invalid[, c("given_name", "family_name", "orcid")])
  } else {
    cat("All ORCIDs are valid.\n")
  }
}

# Check date format
if (!is.null(dates)) {
  bad_dates = dates[!grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(dates$date)), ]
  if (nrow(bad_dates) > 0) {
    cat("Invalid date format in 'dates' sheet (expected YYYY-MM-DD):\n")
    print(bad_dates)
  } else {
    cat("All dates are in valid format.\n")
  }
}

# Check for missing author names
if (any(is.na(authors$given_name) | is.na(authors$family_name))) {
  cat("Some authors are missing given or family names.\n")
} else {
  cat("All authors have names.\n")
}

cat("Validation complete. You can now proceed to generate the JSON.\n")

# --- 2. Process metadata

# Helper function to extract value from metadata sheet
get_meta = function(key) {
  val = metadata$value[metadata$key == key]
  if (length(val) == 0) return(NULL)
  return(as.character(val))
}

# Build DataCite metadata structure
datacite = list(
  titles = list(list(title = get_meta("title"))),
  publisher = get_meta("publisher"),
  publicationYear = substr(get_meta("publication_date"), 1, 4),
  resourceType = list(
    resourceTypeGeneral = "Dataset",
    resourceType = tools::toTitleCase(get_meta("resource_type"))
  ),
  descriptions = list(list(description = get_meta("description"), descriptionType = "Abstract")),
  version = get_meta("version"),
  language = get_meta("language"),
  rightsList = list(list(
    rights = "Creative Commons Attribution 4.0 International",
    rightsUri = "https://creativecommons.org/licenses/by-sa/4.0/",
    rightsIdentifier = "cc-by-sa-4.0",
    rightsIdentifierScheme = "SPDX"
  )),
  subjects = lapply(strsplit(get_meta("subjects"), ";\\s*")[[1]], function(s) list(subject = s)),
  creators = lapply(seq_len(nrow(authors)), function(i) {
    row = authors[i, ]
    affils = unname(na.omit(unlist(row[ , grepl("^affiliation_", names(row))])))
    list(
      name = paste(row$family_name, row$given_name, sep = ", "),
      nameType = "Personal",
      givenName = row$given_name,
      familyName = row$family_name,
      affiliation = as.list(affils),
      nameIdentifiers = list(list(
        nameIdentifier = paste0("https://orcid.org/", row$orcid),
        nameIdentifierScheme = "ORCID",
        schemeUri = "https://orcid.org"
      ))
    )
  })
)

# Optional: Add dates
if (!is.null(dates)) {
  datacite$dates = lapply(seq_len(nrow(dates)), function(i) {
    row = dates[i, ]
    list(date = as.character(row$date), dateType = tools::toTitleCase(row$type))
  })
}

# Optional: Add fundingReferences
if (!is.null(funders)) {
  datacite$fundingReferences = lapply(seq_len(nrow(funders)), function(i) {
    row = funders[i, ]
    list(
      funderName = row$funder_name,
      funderIdentifier = row$funder_identifier,
      awardTitle = row$grant_title,
      awardNumber = row$grant_number
    )
  })
}

# Write to JSON
write(toJSON(datacite, pretty = TRUE, auto_unbox = TRUE), "metadata/project-metadata.json")
