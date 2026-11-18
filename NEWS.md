# Changelog

## Unreleased

To be released with the next batch of trials.

- The release grows from 64 to 431 trials and from 18,570 to 70,650 sites, across 38 crops in 14 countries. New are 114 trials from India (Seeds for Needs, the UNEP-GEF project and the ICAR-Bioversity workplan trials of 2025, with 34 mother trials of participatory varietal selection), 220 from Ethiopia (ISSD Ethiopia and the Seeds for Needs durum wheat trials), the bean trials of Central America, three cowpea trials from Ghana and four pearl millet trials from Nigeria. The Laos forage trial gains its entry descriptions, authors and funders.
- All files move to release schema v2.1. Site coordinates are rounded to two decimals instead of displaced at random, so a published point lies within about 800 m of the recorded one and repeated downloads give the same point. `plot_data` gains `moment_date`, the date a collection moment took place at the site, and `moment_days` is defined as the planned day of the moment, counted from the start of the trial. These changes follow the review by Robert J. Hijmans.
- Researcher-managed companion trials are new in the release. Their files carry `pvs` in the name, and `metadata.companions` and `metadata.companion_of` link them to the tricot trials on the same genotypes.
- Producers, project leaders, members and funders were confirmed with the partners for the India, Ethiopia, Central America, Nigeria and Laos trials.
- The maps in `docs/` are rebuilt from the published files in the Equal Earth projection, and the data paper draft has the current map and author list.
- The 64 trials of the first release first moved to release schema v2.0, built in the data managers' processing repository. No trial or site was added, and the files cover the same 18,570 sites.
- Files now live at `data/<crop>/<crop>-<iso3>-<trial_id>.json`, one folder per crop. `datasets.csv` gives the `path` of each file, keyed by `trial_id`.
- `block_data` is renamed `site_data` and gains country, crop and administrative names (region, district, village) where recorded. `plot_data` gains `moment_days`, harmonised trait names and ids, `trait_original`, `unit` and `flag`. `metadata.variables` now gives the harmonised question id, Crop Ontology term, value type and unit of every column.
- The wheat trial is now filed under `breadwheat`, matching the crop list used across the dataset.
- `rank_analysis` is removed. The files hold observations only, and model results are left to the analyses that use them. The rankings are unchanged in `plot_data`.
- `metadata/tricot-trial.schema.json` and `metadata/data-dictionary.csv` describe the release schema, including the optional `genotype_data` table and the companion links. All files validate.
- License changed from CC BY-SA 4.0 to CC0 1.0 (public domain). The export scripts now write the CC0 license into each trial file.
- The dataset index moved from `data/aa-available-datasets.csv` to `datasets.csv`. It is rebuilt from the trial files by `script/helper/helper-05-build-dataset-index.R`, uses ISO dates, and adds end date, objective, number of blocks and genotypes, collection moments, ranked traits, file size and license.
- New `metadata/tricot-trial.schema.json` and `metadata/data-dictionary.csv` document the trial files, and `script/helper/helper-06-validate-trial-files.R` checks them.
- Trial files now carry the concept DOI, https://doi.org/10.5281/zenodo.17112491, which always resolves to the latest version.
- `.zenodo.json` is now written in Zenodo's deposit format, so Zenodo reads it on release.
- Added `SKILL.md`, `AGENTS.md` and `llms.txt`, aligning the repository with the AI-ready documentation for tricot. They guide users and AI assistants on using the data, which are read-only.
- Added `MAINTAINERS.md`, the guide for the tricot data managers, and an issue template for reporting data errors.

## v0.4 (2025-09-10)

- Last version recorded before this changelog started. Earlier versions are listed on Zenodo.
