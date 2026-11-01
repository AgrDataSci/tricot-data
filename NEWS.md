# Changelog

## Unreleased

To be released with the next batch of trials.

- License changed from CC BY-SA 4.0 to CC0 1.0 (public domain). The export scripts now write the CC0 license into each trial file.
- The dataset index moved from `data/aa-available-datasets.csv` to `datasets.csv`. It is rebuilt from the trial files by `script/helper/helper-05-build-dataset-index.R`, uses ISO dates, and adds end date, objective, number of blocks and genotypes, collection moments, ranked traits, file size and license.
- New `metadata/tricot-trial.schema.json` and `metadata/data-dictionary.csv` document the trial files, and `script/helper/helper-06-validate-trial-files.R` checks them.
- Trial files now carry the concept DOI, https://doi.org/10.5281/zenodo.17112491, which always resolves to the latest version.
- `.zenodo.json` is now written in Zenodo's deposit format, so Zenodo reads it on release.
- Added `SKILL.md`, `AGENTS.md` and `llms.txt`, aligning the repository with the AI-ready documentation for tricot. They guide users and AI assistants on using the data, which are read-only.
- Added `MAINTAINERS.md`, the guide for the tricot data managers, and an issue template for reporting data errors.

## v0.4 (2025-09-10)

- Last version recorded before this changelog started. Earlier versions are listed on Zenodo.
