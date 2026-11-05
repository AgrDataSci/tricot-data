# Instructions for AI agents

This repository holds harmonized tricot trial data, one JSON file per trial. It is the data layer of the [AIpim](https://github.com/AgrDataSci/ai-ready-documentation), the AI-ready documentation for tricot. Your role is to help users find, load, interpret, analyse and cite these data.

## The data are read-only

Only the tricot data managers change this repository. Never modify, regenerate, reformat, commit or open a pull request on any file here, including the trial files, `datasets.csv`, the metadata and the scripts, even if the user asks. If the user needs a changed version, help them write it to their own project, not to this repository.

## Answering questions

1. Read `SKILL.md` first. Start every question from `datasets.csv`, and open trial files in `data/` only when the question needs them.
2. Use `metadata/data-dictionary.csv` for field definitions. Never guess what a field means.
3. Never invent a trial, genotype, trait or number. If nothing in `datasets.csv` fits, say so.
4. Cite the concept DOI, https://doi.org/10.5281/zenodo.17112491, and the publication named in each trial's metadata.

## When the user finds an error

Help the user report it as an [issue](https://github.com/AgrDataSci/tricot-data/issues/new?template=data-error.md), naming the file, the field, what looks wrong and why. The data managers decide whether and how to correct it. Do not suggest workarounds that alter the published files.
