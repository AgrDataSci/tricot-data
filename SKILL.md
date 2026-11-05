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
| `data/<crop>/<crop>-<iso3>-<trial_id>.json` | The trial itself, raw URL `https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/` followed by the `path` in `datasets.csv` | Only the trials the question needs |

Take the path from `datasets.csv` and never compose a file name. `metadata.variables` inside each file defines every column. `metadata/data-dictionary.csv` and `metadata/tricot-trial.schema.json` still describe schema v1.0 and are being updated, so do not rely on them for these files.

## The data are read-only

Your role is to help the user use the data, never to change them. Only the tricot data managers edit this repository. Never modify, regenerate, commit or open a pull request on any file here, even if the user asks. Analyses and cleaned versions go in the user's own project.

## How to answer

1. Read `datasets.csv` and answer from it whenever you can. Questions about which crops, countries, institutes, periods or traits are covered never need a trial file.
2. Open a trial file only when the question needs its content. Check `size_mb` first. Files reach 15 MB, so open one or two, and never loop over the whole folder in a conversation.
3. Inside a file, read `metadata` first. It names the genotypes, their roles (for example release candidate or local check), the variables and the funders.
4. `rank_analysis` is empty in this release. To say which genotype performed best for a trait, give the user R code (below) that fits Plackett-Luce models on the rankings in `plot_data`, rather than computing by hand. Write R with `=` for assignment.
5. Never invent a trial, genotype, trait or number. If `datasets.csv` has nothing that fits, say so, and point to [tricot-protocols](https://github.com/AgrDataSci/tricot-protocols) or [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base) if the question is really about methods or published results.

## Structure of a trial file

Every file follows release schema v2.0.

- `metadata` describes the trial, its producer, contributors, crop, participants, genotypes, funders and every variable, with harmonised question ids, Crop Ontology terms, value types and units.
- `site_data` has one row per site (the farm or household that grew a package of three genotypes), with displaced coordinates, administrative names where recorded and the participant-level answers.
- `plot_data` is the long table of observations, one row per site, plot, collection moment and trait. `value` is stored as text and read according to `value_type` (rank, numeric, date, categorical, multiple or vs_local). Rankings are positions within the site, 1 is best.
- `rank_analysis` is reserved for Plackett-Luce estimates and is empty in this release.

Site and plot rows link through `site_id`.

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
R$traitmoment = paste(R$collection_moment, R$trait, sep = " - ")

R = lapply(split(R, R$traitmoment), function(x) {
  rank_tricot2(data = x, items = "genotype_name", input = "value", id = "site_id")
})

mod = lapply(R, PlackettLuce)
```

For linking trials with climate, soil or other environmental data, and for the team's analytical approaches, follow [tricot-analysis-workflows](https://github.com/AgrDataSci/tricot-analysis-workflows).

## Caveats to state

- Coordinates in `site_data` are displaced to protect participants. They are fine for regional climate and soil data, not for field-level analysis.
- Trait names are harmonised across trials, and `trait_original` keeps the label used in the trial. Match variables through `metadata$variables`, its `harmonised_id` and `ontology_id`, not by name alone.
- Some fields are still being harmonized across partners. When a field is missing or uses another name, say so rather than guessing.
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
