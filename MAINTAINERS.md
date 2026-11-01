# Guide for the tricot data managers

This guide is for the tricot data managers only. Users and their AI assistants should follow `SKILL.md` and `AGENTS.md`, which treat the data as read-only. When you work on a batch with an AI agent, point it to this file explicitly.

## Exporting a batch

1. Never edit the files in `data/` by hand. They are written by the scripts in `script/`, from raw files that are not committed (`raw/` is ignored).
2. Run scripts from the project root, export scripts first and helpers after, as listed in `script/README.md`. Write R with `=` for assignment.
3. After any export, run `script/helper/helper-05-build-dataset-index.R` to rebuild `datasets.csv`, then `script/helper/helper-06-validate-trial-files.R` to check every file against `metadata/tricot-trial.schema.json`. Fix the export, not the file, until every file passes.
4. Never commit personal data. Coordinates must be jittered with `ClimMobTools::rmGeoIdentity()`, and names, phone numbers and other identifiers must stay out of `block_data`.

## Releasing

1. Edit release metadata in `metadata/project-metadata.xlsx`, then regenerate `CITATION.cff` and `.zenodo.json` with helpers 01 and 02. Never edit those two files by hand.
2. Record the release in `NEWS.md`.
3. A GitHub release creates a new Zenodo version, so release only when a batch of data is ready.
4. Never invent a DOI, ORCID, ROR ID, license, funder or date. Leave the field empty and ask the data producer.

## Handling error reports

Users report errors as issues with the `data-error` template. Check the report against the raw data, correct the export script or the partner metadata, re-export, and close the issue with a reference to the release that carries the fix.
