# Changelog

## Unreleased

To be released with the next batch of trials.

- The 64 trials of the first release move to release schema v2.0, built in the data managers' processing repository. No trial or site was added, and the files cover the same 18,570 sites.
- Files now live at `data/<crop>/<crop>-<iso3>-<trial_id>.json`, one folder per crop. `datasets.csv` gives the `path` of each file, keyed by `trial_id`.
- `block_data` is renamed `site_data` and gains country, crop and administrative names (region, district, village) where recorded. `plot_data` gains `moment_days`, harmonised trait names and ids, `trait_original`, `unit` and `flag`. `metadata.variables` now gives the harmonised question id, Crop Ontology term, value type and unit of every column.
- The wheat trial is now filed under `breadwheat`, matching the crop list used across the dataset.
- `rank_analysis` is empty in schema v2.0 and will be refitted by the data managers. The rankings are unchanged in `plot_data`.
- `metadata/tricot-trial.schema.json` and `metadata/data-dictionary.csv` still describe schema v1.0 and will be updated to v2.0 before the release.
- License changed from CC BY-SA 4.0 to CC0 1.0 (public domain). The export scripts now write the CC0 license into each trial file.
- The dataset index moved from `data/aa-available-datasets.csv` to `datasets.csv`. It is rebuilt from the trial files by `script/helper/helper-05-build-dataset-index.R`, uses ISO dates, and adds end date, objective, number of blocks and genotypes, collection moments, ranked traits, file size and license.
- New `metadata/tricot-trial.schema.json` and `metadata/data-dictionary.csv` document the trial files, and `script/helper/helper-06-validate-trial-files.R` checks them.
- Trial files now carry the concept DOI, https://doi.org/10.5281/zenodo.17112491, which always resolves to the latest version.
- `.zenodo.json` is now written in Zenodo's deposit format, so Zenodo reads it on release.
- Added `SKILL.md`, `AGENTS.md` and `llms.txt`, aligning the repository with the AI-ready documentation for tricot. They guide users and AI assistants on using the data, which are read-only.
- Added `MAINTAINERS.md`, the guide for the tricot data managers, and an issue template for reporting data errors.

## v0.4 (2025-09-10)

- Last version recorded before this changelog started. Earlier versions are listed on Zenodo.
