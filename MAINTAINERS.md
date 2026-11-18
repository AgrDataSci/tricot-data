# Guide for the tricot data managers

This guide is for the tricot data managers only. Users and their AI assistants should follow `SKILL.md` and `AGENTS.md`, which treat the data as read-only. When you work on a batch with an AI agent, point it to this file explicitly.

## Publishing a batch

1. Never edit the files in `data/` by hand. They are built in the private processing repository of the data managers, AgrDataSci/tricot-data-raw, from raw files and partner metadata that stay there.
2. A batch is a copy. Take the trial files from `data/` of the processing repository, with the same folders and names, the matching rows of its `datasets.csv`, `metadata/tricot-trial.schema.json` and `metadata/data-dictionary.csv`. Nothing is renamed on the way.
3. After the copy, check every file against `metadata/tricot-trial.schema.json` and confirm that `datasets.csv` lists exactly the files in `data/`. Fix the export in the processing repository, not the file, until every file passes.
4. Never publish personal data. Coordinates are rounded to two decimals, and names, phone numbers and other identifiers stay out of `site_data`.
5. Rebuild the maps in `docs/` from the published files, in the Equal Earth projection.

## Releasing

1. `CITATION.cff` and `.zenodo.json` are both kept and both written from `metadata/project-metadata.xlsx`. Zenodo reads `.zenodo.json` for the record of a release and then ignores `CITATION.cff`. GitHub reads `CITATION.cff` for its citation box. Never edit either file by hand.
2. Update them at every batch. Bring the `authors` sheet in line with the author list of the batch, set `version` and `publication_date` in the `metadata` sheet for a release, then run `python3 script/v2-export/citation_files.py --public <path to this repository>` from the processing repository.
3. Record the release in `NEWS.md`.
4. A GitHub release creates a new Zenodo version, so release only when a batch of data is ready.
5. Never invent a DOI, ORCID, ROR ID, license, funder or date. Leave the field empty and ask the data producer.

## Handling error reports

Users report errors as issues with the `data-error` template. Check the report against the raw data, correct the export script or the partner metadata in the processing repository, publish the corrected file, and close the issue with a reference to the release that carries the fix.
