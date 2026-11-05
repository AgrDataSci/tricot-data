<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17112491.svg)](https://doi.org/10.5281/zenodo.17112491)
[![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License](https://img.shields.io/badge/license-CC0%201.0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
<!-- badges: end --> 

<a href="https://github.com/AgrDataSci/maniva"><img src="docs/maniva-logo.svg" alt="Part of maniva" height="64" align="right"></a>

# Global multi-crop agricultural trial data supported by citizen science

The [triadic comparison of technology options (tricot)](https://doi.org/10.1007/s13593-023-00937-1) is a citizen science approach for testing technology options in their target environments, which has been applied to on-farm testing of crop varieties. ‘Triadic’ refers to the sets of three technology options that are compared by each participant. In the approach, participants are invited to test an anonymous set of three technologies (out of a larger number, generally between 5 to 20) randomly assigned. Between 2011 and 2025 the tricot approach was applied in more than 25 countries across Africa, Asia, Europe and Latin America with more than 30 crops.

---

## Crops and sites currently covered by the dataset

The map below shows the crops and trial sites that are currently available for public use. Each point represents a trial location, and the legend indicates the crop(s) associated with those sites. This visualization helps to quickly identify the regions and crops that are currently represented in the dataset.

![Trial map](docs/trial-locations.png)

## Find a dataset

[`datasets.csv`](datasets.csv) is the finding aid. It lists every trial, one row each, with trial id, crop, taxon, country, data producer, start and end dates, type, management, objective, participants, sites, genotypes, collection moments, ranked traits, the `path` to fetch, file size and license. Start there, filter, then open only the files you need through their `path`. Never compose a file name yourself.

The current release holds 64 trials and 18,570 sites across 14 crops in 7 countries. These are the trials of the first release, now in the harmonised format described below.

## Data structure

Each trial is one JSON file at `data/<crop>/<crop>-<iso3>-<trial_id>.json`, for example `data/potato/potato-NGA-88f82a3f073e.json`. The trial id comes from ClimMob and is the only identity of a trial. Crop and country in the name make a folder easy to scan. Every file follows release schema v2.0 and has four parts.

- **metadata** describes the trial, its producer, contributors, crop, participants, genotypes, funders and every variable in the file.
- **site_data** has one row per site, the farm or household where a package of three genotypes was grown, with displaced coordinates, administrative names (region, district, village) where recorded, planting dates and participant-level answers.
- **plot_data** is the long table of observations, one row per site, plot, collection moment and trait. Values are stored as text and read according to `value_type` (rank, numeric, date, categorical, multiple, or vs_local for the comparison with the farmer's own variety), with the `unit` where one applies.
- **rank_analysis** is reserved for Plackett-Luce estimates fitted by the data managers. It is empty in this release, and the rankings to fit them are in `plot_data`.

Site and plot rows link through `site_id`. `metadata.variables` lists every column of `site_data` and `plot_data` with its description, harmonised question id, Crop Ontology term, value type and unit. Rankings are the position of each genotype within its site, where 1 is best. Trait names are harmonised across trials, and `trait_original` keeps the label used in the trial.

``` text
├── metadata
│   ├── schema_version                      "2.0"
│   ├── changelog (version, date, notes, software)
│   ├── identifier                          concept DOI, https://doi.org/10.5281/zenodo.17112491
│   ├── license                             CC0-1.0
│   ├── study
│   │   ├── id                              trial id, the identity of the file
│   │   ├── code, title, description        ClimMob project code and the trial as described by the producer
│   │   ├── type, objective, experimental_site, unit_of_analysis
│   │   ├── country_iso2, country_iso3, country_name
│   │   ├── source_platform                 ClimMob version the trial came from
│   │   ├── collection_moments (moment, moment_days)
│   ├── date (start, end as YYYY-MM-DD)
│   ├── geo_location
│   │   ├── boundingBox (xmin, xmax, ymin, ymax)
│   ├── location_privacy (method, radius_m, admin_names)
│   ├── data_producer (name, identifier ROR, principal_investigator, email, program)
│   ├── contributors (name, orcid, affiliation, contributorType)
│   ├── crop (name, taxon)
│   ├── participants (total, women, men, not_stated)
│   ├── genotypes (genotype_name, role, release_year, crossing_year, origin, target_trait, remarks)
│   ├── funder (funderName, funderIdentifier ROR, awardTitle, awardNumber)
│   ├── variables
│   │   ├── level                           site or plot
│   │   ├── variable_name, description
│   │   ├── harmonised_id, ontology_id      harmonised question id and Crop Ontology term
│   │   ├── value_type, unit, controlled_vocabulary
├── site_data                               one row per site
│   ├── site_id                             trial id plus package number
│   ├── trial_id, country_iso3, country_name, crop_name
│   ├── longitude, latitude                 WGS84, displaced within radius_m
│   ├── adm1, adm2, adm3                    region, district, village, where recorded
│   ├── planting_date, plot_area_m2, ...    and the survey variables listed in metadata.variables
├── plot_data                               one row per site, plot, moment and trait
│   ├── site_id, plot                       plot is the package letter (A, B, C)
│   ├── genotype_name
│   ├── collection_moment, moment_days
│   ├── trait, trait_id, trait_original
│   ├── value, value_type, unit, flag
├── rank_analysis                           empty in this release
```

[`metadata/tricot-trial.schema.json`](metadata/tricot-trial.schema.json) and [`metadata/data-dictionary.csv`](metadata/data-dictionary.csv) still describe schema v1.0 and are being updated to v2.0. Until then, `metadata.variables` inside each file is the reference for its columns.

## Repository structure

``` text
├── data/           Trial files, one folder per crop, one JSON per trial
├── docs/           Data paper for Scientific Data and the README maps
├── metadata/       Schema, data dictionary, project and per-partner metadata
├── datasets.csv    Index of all trials, with the path to each file
├── SKILL.md        Instructions for AI assistants using the data
├── AGENTS.md       Short instructions for AI agents using the data
├── MAINTAINERS.md  Guide for the tricot data managers
├── NEWS.md         What each release added
```

## Use with AI assistants

This repository is the data layer of the [maniva](https://github.com/AgrDataSci/maniva), the AI-ready documentation for tricot. Share that link with an AI assistant to reach the data together with the published evidence, protocols and analytical approaches, or use [`SKILL.md`](SKILL.md) on its own.

## Using the ranking data

To analyse rankings, read the file path from `datasets.csv`, keep the `plot_data` rows where `value_type` is `rank`, convert them to rankings with `gosset::rank_tricot2()`, and fit Plackett-Luce models.

``` r
library("jsonlite")
library("gosset")
library("PlackettLuce")

base = "https://raw.githubusercontent.com/AgrDataSci/tricot-data/main/"

# Find the trial in the index
idx = read.csv(paste0(base, "datasets.csv"))
path = idx$path[idx$trial_id == "88f82a3f073e"]

# Load the trial
dat = fromJSON(paste0(base, path))

# study metadata
dat$metadata$study

# Prepare and model rankings by trait × collection moment
R = dat$plot_data
R = R[R$value_type == "rank", ]
R$traitmoment = paste(R$collection_moment, R$trait, sep = " - ")

R = split(R, R$traitmoment)
R = lapply(R, function(x) {
  rank_tricot2(
    data  = x,
    items = "genotype_name",
    input = "value",
    id    = "site_id"
  )
})

mod = lapply(R, PlackettLuce)
```
For more information, check the [gosset package documentation](https://agrdatasci.github.io/gosset/).

## Report an error

The data are maintained by the tricot data managers. If something looks wrong, [open an issue](https://github.com/AgrDataSci/tricot-data/issues/new?template=data-error.md) naming the file, the field and the problem. Corrections are made in the export scripts and published in the next release.

## Citation

The dataset is archived on [Zenodo](https://doi.org/10.5281/zenodo.17112491). This DOI always resolves to the latest version.

> The dataset is dedicated to the public domain under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). You can use it without restriction. Please cite it, and the publications named in each trial's metadata, so that the farmers, data producers and funders behind the trials get credit.

de Sousa, K., Laporte, M.-A., Abate, L., Abdulmalik, R. O., Abidin, E., Abolore, B., Achigan-Dako, E. G., Aglinglo, L. A., Aguilar, A., Angudubo, S., Arnaud, E., Assefa, T. M., Atieno, E., Ayenan, M., Barrios, M., Borman, G., Boukar, O., Brown, D., Carey, E., Chaves, N., … van Etten, J. (2025). Global multi-crop agricultural trial data supported by citizen science [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.17112491

### BibTeX (dataset)

``` bibtex
@dataset{desousa_tricot_2025,
  author       = {de Sousa, Kauê and Laporte, Marie-Angélique and Abate, Legesse and Abdulmalik, Rekiya O. and Abidin, Erna and Abolore, Bello and Achigan-Dako, Enoch G. and Aglinglo, Lys Amavi and Aguilar, Amilcar and Angudubo, Stephen and Arnaud, Elizabeth and Assefa, Teshale M. and Atieno, Elly and Ayenan, Mathieu and Barrios, Mirna and Borman, Gareth and Boukar, Ousmane and Brown, David and Carey, Edward and Chaves, Néstor and Chase, Rachel and … and van Etten, Jacob},
  title        = {Global multi-crop agricultural trial data supported by citizen science},
  year         = {2025},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.17112491},
  url          = {https://doi.org/10.5281/zenodo.17112491},
  note         = {Dataset}
}
```
