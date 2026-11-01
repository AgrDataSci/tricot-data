# .............................................
# Test the v3 export on data-sample.rda (six trials) before running on all data.
# Run from the root of the tricot-data repository:
#   source("tricot-export/tests/test-sample.R")
# Every line should print PASS. Expected values were computed independently
# from the raw data with the same rules.
# .............................................
library("ClimMobTools")
library("gosset")
library("PlackettLuce")

for (f in list.files("tricot-export/R", pattern = "\\.R$", full.names = TRUE)) source(f)

dict = build_dictionary(
  questions_xlsx = "metadata/Harmonized_Questions_Ontologies_v20260609.xlsx",
  units_xlsx = "metadata/tricot-units-reviewed.xlsx")

env = new.env()
load("raw/data-sample.rda", envir = env)
trials = env$y

release = list(version = "test", notes = "test", doi = NA, license = NA,
               radius_m = 1500, nmin = 0.2, min_sites = 10)

expected = list(
  "7bec309a7ed0" = list(code = "GCTT24", sites = 249, trial_plot = 245, farm = 0, delivery = 4, missing = 0, rank_traits = 7, rank_rows = 3480),
  "c564f83b89c5" = list(code = "amara2021B", sites = 251, trial_plot = 212, farm = 0, delivery = 39, missing = 0, rank_traits = 15, rank_rows = 4782),
  "caeb64350d8f" = list(code = "Sorgnig", sites = 80, trial_plot = 80, farm = 0, delivery = 0, missing = 0, rank_traits = 11, rank_rows = 1878),
  "ef37e7107b4e" = list(code = "SGS24", sites = 290, trial_plot = 261, farm = 0, delivery = 29, missing = 0, rank_traits = 10, rank_rows = 5283),
  "18b89d9040cd" = list(code = "Pot21A", sites = 150, trial_plot = 0, farm = 150, delivery = 0, missing = 0, rank_traits = 18, rank_rows = 6888),
  "82cc41bf05f1" = list(code = "BeanTest", sites = 90, trial_plot = 65, farm = 0, delivery = 25, missing = 0, rank_traits = 8, rank_rows = 951))

check = function(label, got, want) {
  ok = isTRUE(all.equal(as.numeric(got), as.numeric(want)))
  cat(sprintf("%s  %-45s got %s, expected %s\n", if (ok) "PASS" else "FAIL", label, got, want))
}

results = list()
for (x in trials) {
  pid = project_field(x, "project_id")
  e = expected[[pid]]
  cat("\n==", e$code, "\n")
  r = run_trial(x, dict, cfg = list(), release = release)
  s = r$out$site_data
  p = r$out$plot_data
  src = r$res$report$coordinate_source
  get = function(k) if (k %in% names(src)) src[[k]] else 0
  check("sites", nrow(s), e$sites)
  check("coordinates from trial plot", get("trial plot"), e$trial_plot)
  check("coordinates from farm", get("farm"), e$farm)
  check("coordinates from delivery point", get("seed delivery point"), e$delivery)
  check("ranked traits (moment x trait)", length(unique(paste(p$collection_moment, p$trait)[p$value_type == "rank"])), e$rank_traits)
  check("rank rows", sum(p$value_type == "rank"), e$rank_rows)
  check("each ranked site has ranks 1, 2 and 3",
        all(tapply(as.integer(p$value[p$value_type == "rank"]),
                   paste(p$site_id, p$collection_moment, p$trait)[p$value_type == "rank"],
                   function(v) identical(sort(v), 1:3))), TRUE)
  check("no personal fields in site data",
        sum(grepl(PII_FIELDS, names(s), ignore.case = TRUE)), 0)
  check("published coordinates are finite",
        all(is.finite(s$longitude[!is.na(s$longitude)])), TRUE)
  check("rank analysis rows > 0", nrow(r$out$rank_analysis) > 0, TRUE)
  file = write_trial(r$out, file.path(tempdir(), "tricot-test"))
  back = jsonlite::fromJSON(file)
  check("JSON reads back with same plot rows", nrow(back$plot_data), nrow(p))
  results[[pid]] = r
}

cat("\nSite columns per trial\n")
for (pid in names(results)) cat(expected[[pid]]$code, ":", paste(names(results[[pid]]$out$site_data), collapse = ", "), "\n")

cat("\nYield checks in the Ghana cowpea trial\n")
g = results[["7bec309a7ed0"]]$out$plot_data
print(table(g$trait[g$trait == "yieldperse"], g$flag[g$trait == "yieldperse"], useNA = "ifany"))
