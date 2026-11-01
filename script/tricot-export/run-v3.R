# .............................................
# Export all ClimMob v3 tricot trials into release schema v2.0
# Run from the root of the tricot-data repository.
#
# Inputs
#   raw/trial-data.rda                     list of CM_list objects (cmdata)
#   metadata/Harmonized_Questions_Ontologies_v20260609.xlsx
#   metadata/tricot-units-reviewed.xlsx    unit and basis sheet after review
#   config/projects.csv                    one row per trial, include = TRUE to export
#   data-v1/                               JSON files of the previous release, to
#                                          keep the published coordinates stable
# Outputs
#   data/<crop>-<trial id>.json
#   data/aa-available-datasets.csv
#   docs/trial-xy.csv
#   reports/export-report-<date>.csv
# .............................................
library("ClimMobTools")
library("gosset")
library("PlackettLuce")

for (f in list.files("script/tricot-export/R", pattern = "\\.R$", full.names = TRUE)) source(f)

release = list(
  version = "v2.0",
  notes = paste("Schema 2.0. Block renamed to site. Harmonised variable names, units and",
                "English descriptions. Country and crop added to site data. District and",
                "village names kept. Coordinates checked against country borders."),
  doi = "https://doi.org/10.5281/zenodo.17112491",
  license = "https://creativecommons.org/publicdomain/zero/1.0/",
  radius_m = 1500,
  nmin = 0.2,
  min_sites = 10)

dict = build_dictionary(
  questions_xlsx = "raw/Harmonized_Questions_Ontologies_v20260609.xlsx",
  units_xlsx = "raw/tricot-units-reviewed.xlsx")

env = new.env()
load("raw/trial-data.rda", envir = env)
trials = env$cmdata

# first run only, then fill config/projects.csv and set include = TRUE
if (!file.exists("config/projects.csv")) {
  make_config_template(trials)
  stop("config/projects.csv created. Fill it in and set include = TRUE for each trial to export.")
}

make_config_template(trials)  # adds trials that are new since the last run
config = utils::read.csv("config/projects.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

previous_xy = if (dir.exists("data-v1")) read_published_coordinates("data-v1") else NULL

report = list()
available = list()
xy_map = list()

for (x in trials) {
  pid = project_field(x, "project_id")
  cfg = project_config(config, pid)
  if (!cfg$include) next
  message("Exporting ", pid, " ", project_field(x, "project_cod"))
  r = tryCatch(run_trial(x, dict, cfg, release, previous_xy),
               error = function(e) e)
  if (inherits(r, "error")) {
    report[[pid]] = report_row(x, error = conditionMessage(r))
    next
  }
  file = write_trial(r$out, "data")
  report[[pid]] = report_row(x, r$res)
  m = r$out$metadata
  s = r$out$site_data
  available[[pid]] = data.frame(study_id = pid, crop_name = m$crop$name,
                                taxon = m$crop$taxon %||% NA, country = m$study$country_iso3,
                                data_producer_institute = m$data_producer$name %||% NA,
                                start_date = m$date$start %||% NA, participants = nrow(s),
                                filename = basename(file), stringsAsFactors = FALSE)
  xy_map[[pid]] = data.frame(site_id = s$site_id, crop_name = s$crop_name,
                             longitude = round(s$longitude, 2), latitude = round(s$latitude, 2))
}

report = rbind_fill(report)
dir.create("reports", showWarnings = FALSE)
utils::write.csv(report, file.path("reports", paste0("export-report-", Sys.Date(), ".csv")),
                 row.names = FALSE, na = "")
utils::write.csv(do.call(rbind, available), "data/aa-available-datasets.csv", row.names = FALSE, na = "")
utils::write.csv(do.call(rbind, xy_map), "docs/trial-xy.csv", row.names = FALSE, na = "")

print(table(report$status))
