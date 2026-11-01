# Instructions for AI agents

This repository holds harmonized tricot trial data, one JSON file per trial, and the R scripts that export them. It is the data layer of the [AI-ready documentation for tricot](https://github.com/AgrDataSci/ai-ready-documentation).

## Answering questions

1. Read `SKILL.md` first. Start every question from `datasets.csv`, and open trial files in `data/` only when the question needs them.
2. Use `metadata/data-dictionary.csv` for field definitions. Never guess what a field means.
3. Never invent a trial, genotype, trait or number. Cite the concept DOI, https://doi.org/10.5281/zenodo.17112491, and the publication named in each trial's metadata.

## Working on the repository

1. Never edit the files in `data/` by hand. They are written by the scripts in `script/`, from raw files that are not committed (`raw/` is ignored).
2. Run scripts from the project root, export scripts first and helpers after, as listed in `script/README.md`. Write R with `=` for assignment.
3. After any export, run `script/helper/helper-05-build-dataset-index.R` to rebuild `datasets.csv`, then `script/helper/helper-06-validate-trial-files.R` to check every file against `metadata/tricot-trial.schema.json`.
4. Edit release metadata in `metadata/project-metadata.xlsx`, then regenerate `CITATION.cff` and `.zenodo.json` with helpers 01 and 02. Never edit those two files by hand.
5. Never commit personal data. Coordinates must be jittered with `ClimMobTools::rmGeoIdentity()`, and names, phone numbers and other identifiers must stay out of `block_data`.
6. Record each release in `NEWS.md`. A GitHub release creates a new Zenodo version, so release only when a batch of data is ready.
7. Never invent a DOI, ORCID, ROR ID, license, funder or date. Ask the user, or leave the field empty and say so.
