# tricot-export

Exports ClimMob v3 tricot trials into release schema v2.0 of the tricot-data repository.
ClimMob v2 JSON and v2 CSV come later as separate adapters feeding the same schema.

## Setup

Copy this folder to the root of the `tricot-data` repository and place the inputs.

| Path | Content |
|---|---|
| `raw/trial-data.rda` | All v3 trials as a list of `CM_list` objects named `cmdata` |
| `raw/data-sample.rda` | The six-trial sample, object `y`, used by the test |
| `metadata/Harmonized_Questions_Ontologies_v20260609.xlsx` | Almendra's harmonised question table |
| `metadata/tricot-units-reviewed.xlsx` | The unit and basis sheet after review (start from `tricot-units-draft-20261029.xlsx`) |
| `data-v1/` | The JSON files of release v1, moved out of `data/`, so sites keep their published coordinates |

Packages: ClimMobTools, gosset (1.5.1 or later, for `rank_tricot2`), PlackettLuce, qvcalc, jsonlite, readxl, sf, terra, geodata.

## Run

1. `source("tricot-export/tests/test-sample.R")` runs the six sample trials. Every check should print PASS.
2. `source("tricot-export/run-v3.R")` the first time writes `config/projects.csv` with one row per trial and stops.
3. Fill `config/projects.csv`. Set `include = TRUE` for each trial to publish and add crop, taxon, institute, ROR, PI and the paths to the partner genotype and funder sheets you already have. Empty cells fall back to what ClimMob holds.
4. Run `run-v3.R` again. It writes one JSON per trial to `data/`, plus `data/aa-available-datasets.csv`, `docs/trial-xy.csv` and `reports/export-report-<date>.csv`.
5. Read the report before releasing. It lists per trial the coordinate sources, coordinates removed for falling outside the country, rankings dropped, ages converted or removed, flagged yields and question codes not found in the dictionary.

## Schema v2.0

| Table | One row per | Main columns |
|---|---|---|
| `metadata` | trial | study, dates, bounding box, location privacy, producer, crop, participants, genotypes, funder, variables |
| `site_data` | site (formerly block) | site_id, trial_id, country_iso3, country_name, crop_name, longitude, latitude, coordinate_source, coordinate_uncertainty_m, coordinate_flag, district, village, gender, age, plot_area_m2, other site questions |
| `plot_data` | site x plot x moment x trait | site_id, plot, genotype_name, collection_moment, moment_days, trait, trait_id, trait_original, value, value_type, unit, flag |
| `rank_analysis` | moment x trait x genotype | log_worth, se, quasi_se, quasi_var, reference, n_sites |

Rankings use 1 for best and 3 for worst. Ties and "not observed" answers are left out. Yield values refer to the entire plot. `<trait>_kg_ha` rows are derived from yield and plot area and carry `flag = derived`.

## Robert's comments

| Comment | Where it is handled |
|---|---|
| Units missing | Every numeric variable needs a confirmed unit and basis in the unit sheet, otherwise it is not published |
| Yield per area | `<trait>_kg_ha` from yield and plot area, with values above 20 t/ha (80 for roots, tubers and vegetables) flagged instead of converted |
| Unclear traits | English descriptions and harmonised names from Almendra's table, with `trait_original` kept for traceability |
| Country and crop as data | `country_iso3`, `country_name` and `crop_name` in `site_data` |
| Block should be site | Renamed throughout |
| Missing coordinates | Coordinates read from the trial plot first, then the farm, then the seed delivery point, with the source recorded. District and village names are kept |
| trials-metadata in the release | Configuration lives in `config/` and `metadata/`, which stay out of the Zenodo archive |

## Privacy

Names, phone numbers, IMEI, photos, consent fields and free text are never published. The only free text kept is administrative names and local variety names. Coordinates are displaced within `radius_m` (1500 m by default), reproducibly per site, and redrawn when the displaced point leaves the country. Sites already published in v1 keep their v1 coordinates.

## Known limits

- The dictionary resolves questions by code. Codes from libraries missing in Almendra's table (for example the 2020 Rwanda potato trial) are published under their own code and description and listed in the report.
- Plot area is read from the plot size question and its unit. Free-text plot sizes are parsed only when they state a unit.
- The code has not been run in R yet. The logic was checked on the sample in Python. The test script compares the R output with those numbers.
