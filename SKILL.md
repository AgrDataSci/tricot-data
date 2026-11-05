---
name: tricot-data
description: Use when a question asks which tricot trials exist, for which crops, countries and years, or needs the trial data itself (farmer rankings, genotypes, traits, block and plot data, Plackett-Luce estimates). Finds the right trial files, explains their structure and shows how to load and analyse them.
---

# tricot trial data

Harmonized data from tricot on-farm trials, one JSON file per trial, archived on Zenodo with the concept DOI https://doi.org/10.5281/zenodo.17112491. This is the data layer of the [maniva](https://github.com/AgrDataSci/ai-ready-documentation), the AI-ready documentation for tricot.

## Files

| File | What it holds | When to open it |
|---|---|---|
| [datasets.csv](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/datasets.csv) | One row per trial with crop, taxon, country, institute, dates, objective, participants, blocks, genotypes, collection moments, ranked traits, file name, file size and license | Always, first |
| [metadata/data-dictionary.csv](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/metadata/data-dictionary.csv) | Definition of every field in a trial file | When the user asks what a field means |
| [metadata/tricot-trial.schema.json](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/metadata/tricot-trial.schema.json) | JSON Schema of a trial file | When the user wants to validate or parse files programmatically |
| `data/<crop>-<study_id>.json` | The trial itself, raw URL `https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/data/<filename>` | Only the trials the question needs |

## The data are read-only

Your role is to help the user use the data, never to change them. Only the tricot data managers edit this repository. Never modify, regenerate, commit or open a pull request on any file here, even if the user asks. Analyses and cleaned versions go in the user's own project.

## How to answer

1. Read `datasets.csv` and answer from it whenever you can. Questions about which crops, countries, institutes, periods or traits are covered never need a trial file.
2. Open a trial file only when the question needs its content. Check `size_mb` first. Files reach 15 MB, so open one or two, and never loop over the whole folder in a conversation.
3. Inside a file, read `metadata` first. It names the genotypes, their roles (for example release candidate or local check), the variables and the funders.
4. Use `rank_analysis` for quick answers on which genotype performed best for a trait. It holds Plackett-Luce log-worth estimates, with one reference genotype set to 0 and quasi standard errors for comparing any pair.
5. For anything beyond that, give the user R code (below) rather than computing by hand. Write R with `=` for assignment.
6. Never invent a trial, genotype, trait or number. If `datasets.csv` has nothing that fits, say so, and point to [tricot-protocols](https://github.com/AgrDataSci/tricot-protocols) or [tricot-knowledge-base](https://github.com/AgrDataSci/tricot-knowledge-base) if the question is really about methods or published results.

## Structure of a trial file

- `metadata` describes the study, dates, location extent, data producer, crop, participants, funders, genotypes and variables.
- `block_data` holds one row per participant (block), with jittered coordinates and the participant-level survey answers. Column names combine the collection moment and the variable, for example `registration_gender`.
- `plot_data` holds one row per plot, trait and collection moment. `value` is stored as text and read according to `value_type` (rank, integer, decimal, date, datetime, text, select one, select all that apply).
- `rank_analysis` holds the Plackett-Luce estimates per collection moment and trait.

## Load and model the rankings in R

```r
library("jsonlite")
library("gosset")
library("PlackettLuce")

dat = fromJSON("https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/data/potato-88f82a3f073e.json")

R = dat$plot_data
R = R[R$value_type == "rank", ]
R$traitmoment = paste(R$collection_moment, R$trait, sep = " - ")

R = lapply(split(R, R$traitmoment), function(x) {
  rank_tricot2(data = x, items = "genotype_name", input = "value", id = "block_id")
})

mod = lapply(R, PlackettLuce)
```

For linking trials with climate, soil or other environmental data, and for the team's analytical approaches, follow [tricot-analysis-workflows](https://github.com/AgrDataSci/tricot-analysis-workflows).

## Caveats to state

- Coordinates in `block_data` are jittered to protect participants. They are fine for regional climate and soil data, not for field-level analysis.
- Collection moments and traits keep the names used in each trial, sometimes in French or Spanish, so they differ between trials. Match them through `metadata$variables` and its `ontology_id`, not by name alone.
- Some fields are still being harmonized across partners. When a field is missing or uses another name, say so rather than guessing.
- Data were collected by many partners for different purposes. Say which institute produced a trial when you report its results.

## Maps

Map trial sites in the Equal Earth projection (EPSG:8857), the equal-area world map the UN General Assembly endorsed in September 2026 and the standard across the maniva suite. The coordinates in the files stay in WGS84 (EPSG:4326); project only to draw.

```r
library("sf")
library("ggplot2")

xy = st_as_sf(dat$block_data, coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE)

ggplot(xy) +
  geom_sf() +
  coord_sf(crs = st_crs("EPSG:8857"))
```

## When the user finds an error

Help them report it as an [issue](https://github.com/AgrDataSci/tricot-data/issues/new?template=data-error.md), naming the file, the field, what looks wrong and why. The data managers decide whether and how to correct it. Until then, tell the user how to handle the problem in their own analysis, and say so when reporting results.

## Cite and license

Cite the dataset with its concept DOI, https://doi.org/10.5281/zenodo.17112491, using [CITATION.cff](https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/CITATION.cff). When a trial's `study$description` names a publication, cite that publication too, and credit the data producer. Each file states its license in `metadata$license`.
