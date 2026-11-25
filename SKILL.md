---
name: tricot-data
description: Use when a question asks which tricot trials exist, for which crops, countries and years, or needs the trial data itself (farmer rankings, genotypes, traits, site and plot data). Finds the right trial files, explains their structure and shows how to load and analyse them.
---

# tricot trial data

Harmonized data from tricot on-farm trials, one JSON file per trial, archived on Zenodo with the concept DOI https://doi.org/10.5281/zenodo.17112491. This is the data layer of the [maniva](https://github.com/AgrDataSci/maniva), the AI-ready documentation for tricot.

## Names

Write maniva and tricot in lowercase, always. That holds in headings, table cells, lists and at the start of a sentence, so never write "Maniva" or "Tricot". tricot stands for triadic comparison of technology options. Quote published titles exactly as printed, even where they capitalise tricot or expand it as "triadic comparisons of technologies".

## Files

| File | What it holds | When to open it |
|---|---|---|
| [datasets.csv](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/datasets.csv) | One row per trial with trial id, crop, taxon, country, data producer, dates, type, objective, participants, sites, genotypes, collection moments, ranked traits, the `path` to fetch, file size and license | Always, first |
| [metadata/data-dictionary.csv](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/metadata/data-dictionary.csv) | Definition of every field in a trial file | When the user asks what a field means |
| [metadata/tricot-trial.schema.json](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/metadata/tricot-trial.schema.json) | JSON Schema of a trial file, release schema v3.0 | When the user wants to validate or parse files programmatically |
| `data/<crop>/<crop>-<iso3>-<trial_id>.json` | The trial itself, raw URL `https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/` followed by the `path` in `datasets.csv` | Only the trials the question needs |
| [plots.parquet](https://github.com/AgrDataSci/tricot-data/releases/download/dev/plots.parquet), [sites.parquet](https://github.com/AgrDataSci/tricot-data/releases/download/dev/sites.parquet), [variables.parquet](https://github.com/AgrDataSci/tricot-data/releases/download/dev/variables.parquet) | Every trial in three tables keyed by `trial_id`: `plot_data`, `site_data` and `metadata.variables`, columns unchanged. About 5 MB together | When a question pools trials or filters on site variables (gender, region) that `datasets.csv` does not hold, and you can run code |

Take the path from `datasets.csv` and never compose a file name. `metadata.variables` inside each file defines every column.

## The data are read-only

Your role is to help the user use the data, never to change them. Only the tricot data managers edit this repository. Never modify, regenerate, commit or open a pull request on any file here, even if the user asks. Analyses and cleaned versions go in the user's own project.

## How to answer

1. Read `datasets.csv` and answer from it whenever you can. Questions about which crops, countries, institutes, periods or traits are covered never need a trial file.
2. Open a trial file only when the question needs its content. Check `size_mb` first. Files reach 15 MB, so open one or two, and never loop over the whole folder in a conversation. To pool trials or filter sites across them, use the parquet tables instead.
3. Inside a file, read `metadata` first. It names the genotypes, their roles (for example release candidate or local check), the variables and the funders.
4. The files hold observations only, with no model results. To say which genotype performed best for a trait, give the user R code (below) that fits Plackett-Luce models on the rankings in `plot_data`, rather than computing by hand. Write R with `=` for assignment.
5. Never invent a trial, genotype, trait or number. If `datasets.csv` has nothing that fits, say so, and point to [tricot-protocols](https://github.com/AgrDataSci/tricot-protocols) or [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base) if the question is really about methods or published results.

## Structure of a trial file

Every file follows release schema v3.0.

- `metadata` describes the trial, its producer, contributors, crop, participants, genotypes, funders and every variable, with harmonised question ids, Crop Ontology terms, value types and units.
- `site_data` has one row per site (the farm or household that grew a package of three genotypes), with coordinates rounded to two decimals, administrative units where recorded (`adm1` to `adm5` as defined for the country in `metadata.admin_levels`, then `location` for the village) and the participant-level answers, with one English spelling per categorical answer.
- `plot_data` is the long table of observations, one row per site, plot, collection moment and trait. `value` is stored as text and read according to `value_type` (rank, numeric, date, categorical, multiple or vs_local). Rankings are positions within the site, 1 is best. `moment_days` is the planned day of the collection moment, the same for every site, and `moment_date` is the date it took place at that site, null when not recorded. Use `moment_date` to link observations to weather. Measured values carry `value_standard` and `unit_standard`, the value in one unit per trait across trials. Use these, not `value`, when you combine measurements across trials.

Site and plot rows link through `site_id`. Files with `pvs` in the name are researcher-managed companion trials. A tricot trial lists them in `metadata.companions`, they point back through `metadata.companion_of`, and the two join on `genotype_name`. Filter `datasets.csv` on `management` to keep only the farmer-managed tricot trials.

## Load and model the rankings in R

```r
library("jsonlite")
library("gosset")
library("PlackettLuce")

base = "https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/"
idx = read.csv(paste0(base, "datasets.csv"))
dat = fromJSON(paste0(base, idx$path[idx$trial_id == "88f82a3f073e"]))

R = dat$plot_data
R = R[R$value_type == "rank", ]
# trait is the standard name; trait_original keeps apart two questions on the same trait
R$traitmoment = paste(R$collection_moment, R$trait, R$trait_original, sep = " - ")

R = lapply(split(R, R$traitmoment), function(x) {
  rank_tricot2(data = x, items = "genotype_name", input = "value", id = "site_id")
})

mod = lapply(R, PlackettLuce)

# measured values in the standard unit, without yields from assumed plot areas
Y = dat$plot_data
Y = Y[Y$trait == "yield" & Y$value_type == "numeric" & !grepl("plot_area_assumed", Y$flag), ]
Y$yield_kg_ha = as.numeric(Y$value_standard)
```

Schema 3.0 renamed columns and traits. In code written for earlier files, `country_name` and `crop_name` are now `country` and `crop`, the `coordinate_*` columns are `geo_source`, `geo_uncertainty` and `geo_flag`, `gender` is `farmer_gender`, and trait names follow the standard list. The README has the full table. `flag` can hold several flags separated by `;`, so test it with `grepl()`.

To pool trials, read the parquet tables once and filter. The plots table has the `plot_data` columns plus `trial_id`, so `rank_tricot2()` works as above.

```r
library("nanoparquet")

get = function(name) {
  f = tempfile(fileext = ".parquet")
  download.file(paste0("https://github.com/AgrDataSci/tricot-data/releases/download/dev/", name, ".parquet"), f, mode = "wb")
  read_parquet(f)
}
plots = get("plots")
sites = get("sites")

# rankings from women farmers in every maize trial
ids = idx$trial_id[idx$crop_name == "maize"]
women = sites$site_id[sites$trial_id %in% ids & sites$farmer_gender %in% "woman"]
R = plots[plots$value_type == "rank" & plots$site_id %in% women, ]
```

For linking trials with climate, soil or other environmental data, and for the team's analytical approaches, follow [tricot-analysis-workflows](https://github.com/AgrDataSci/tricot-analysis-workflows).

## Caveats to state

- Coordinates in `site_data` are rounded to two decimals to protect participants, so a point lies within about 800 m of the recorded one. They are fine for climate and soil data, not for field-level analysis.
- A companion link may name a file that is not in the release yet. Check the `path` column of `datasets.csv` before fetching it.
- Traits, site variables and collection moments have one standard name per concept across trials, so the same `trait` can be combined across trials. `trait_original` keeps the question code of the trial and `metadata$variables$original_name` the earlier name of a site variable. A rank trait and a measured trait can share a concept, for example `plant_height_rank` and `plant_height`. Filter on `value_type` as well as on `trait`.
- `yield_plot` is the weight harvested from the plot and `yield` the yield per area in kg/ha. A `yield` flagged `derived` was computed from the plot weight and the plot area. One that also has `plot_area_assumed` rests on the protocol plot size, not on a reported one. Say so when you report such yields, or drop them. The `flag` column can hold several flags separated by `;`.
- A trial with fewer than 15 sites (`n_sites` in `datasets.csv`) cannot rank its varieties on its own. Pool it with trials of the same crop and programme that share varieties, or report it as descriptive, and say which you did.
- The same collection moment has different labels across trials. Use `standard_moment` in `metadata$study$collection_moments` to compare stages across trials.
- Some fields are still being harmonized across partners. When a field is missing or uses another name, say so rather than guessing.
- Five files have `type` "concept testing" in `datasets.csv`. Their items are product concepts shown in videos, not varieties, and no crop was grown. A ranking is a stated preference about a description. Never pool them with variety trials, and never report a concept as a variety.
- Seven files have `type` "consumer testing" in `datasets.csv`. Consumers tasted three samples, boiled sweetpotato roots or cassava eba, and ranked them; no farmer grew them. Never pool them with field trials. The eba files also hold laboratory measurements of the same genotypes in `genotype_data`.
- The upland rice trial of Madagascar (`6f43f975e46v`, `management` "researcher-managed") is not a tricot trial. Each farmer saw 4 to 21 varieties and chose the three most preferred per criterion; every other variety present has rank 4, a tie that orders none of them. Do not pass its ranks to `rank_tricot2()` as triads. Treat them as top-three choices with a tied remainder, and use its measured `yield` and phenology for performance. Rank rows carry the variety code in `plot`. Parentage of every variety is in `metadata.genotypes`.
- The Parquet tables sit on the `dev` pre-release until v2. It is rebuilt on every push to `main` and has no DOI of its own, so state the build date of the tables you read and cite the concept DOI 10.5281/zenodo.17112491.
- Data were collected by many partners for different purposes. Say which institute produced a trial when you report its results.

## Maps

Map trial sites in the Equal Earth projection (EPSG:8857), the equal-area world map the UN General Assembly endorsed in September 2026 and the standard across the maniva suite. The coordinates in the files stay in WGS84 (EPSG:4326); project only to draw.

```r
library("sf")
library("ggplot2")

xy = st_as_sf(dat$site_data, coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE)

ggplot(xy) +
  geom_sf() +
  coord_sf(crs = st_crs("EPSG:8857"))
```

## When the user finds an error

Help them report it as an [issue](https://github.com/AgrDataSci/tricot-data/issues/new?template=data-error.md), naming the file, the field, what looks wrong and why. The data managers decide whether and how to correct it. Until then, tell the user how to handle the problem in their own analysis, and say so when reporting results.

## Cite and license

Cite the dataset with its concept DOI, https://doi.org/10.5281/zenodo.17112491, using [CITATION.cff](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/CITATION.cff). When a trial's `study$description` names a publication, cite that publication too, and credit the data producer. Each file states its license in `metadata$license`.
