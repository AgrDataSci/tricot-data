# Scripts

Run every script from the project root. The export scripts read raw files from `raw/`, which is not committed.

## Export scripts

Each script exports one partner's trials to `data/` and adds their jittered coordinates to `docs/trial-xy.csv`.

| Script | Data producer | Crops |
|---|---|---|
| `01-export-worldveg-data.R` | World Vegetable Center | amaranth, chili pepper, jute mallow, okra, tomato |
| `02-export-groundnut-tanzania-data.R` | Tanzania Agricultural Research Institute | groundnut |
| `03-export-potato-nigeria.R` | International Potato Center | potato |
| `04-export-cowpea-nigeria.R` | International Institute of Tropical Agriculture | cowpea |
| `05-one-acre-fund-rwanda.R` | One Acre Fund | common bean, forages, maize, potato, sorghum, soybean, wheat |
| `06-ethiopiagrass.R` | Norwegian Institute of Bioeconomy Research | forages |

## Helpers

Run these after the export scripts, in this order, before a release.

| Script | What it does |
|---|---|
| `helper/helper-05-build-dataset-index.R` | Rebuilds `datasets.csv` from all trial files |
| `helper/helper-06-validate-trial-files.R` | Checks every trial file against `metadata/tricot-trial.schema.json` |
| `helper/helper-03-trial-map-with-available-datasets.R` | Draws `docs/trial-locations.png`, the map in the README |
| `helper/helper-01-parse-metadata-as-cff.R` | Writes `CITATION.cff` from `metadata/project-metadata.xlsx` |
| `helper/helper-02-parse-metadata-as-json.R` | Writes `.zenodo.json` from `metadata/project-metadata.xlsx` |
| `helper/helper-04-trial-map-all-data-generated.R` | Draws `docs/tricot-trials.png`, the map of all tricot trials, published or not (needs raw files) |
