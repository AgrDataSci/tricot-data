^(a) Bioversity International, Montpellier, France. ^(b) University of Inland Norway, Hamar, Norway. ^(c) Add affiliations, City, Country.

Draft of 4 November 2026, built on the release files of AgrDataSci/tricot-data-raw as indexed on that date. Items marked XX are placeholders to resolve before submission.

# Background and summary

Fifteen years of on-farm trials with the tricot approach produced rankings from more than 100,000 farms on 42 crops in 20 countries, and until now none of it could be fetched, pooled or cited as one resource. The trials were run by 27 organisations under their own projects, funders and platform versions, so the same yield question was worded four ways in three languages, the same variety carried two spellings, farms were located by GPS in one file and by village name in another, and the producer of a trial was often recorded nowhere. Every tricot study published so far faced that wall and climbed it by hand, once, for its own subset^(1–4).

This release removes the wall. The dataset is reusable at this scale because every trial is published as one self-describing file that carries the credit of the organisation that produced it. That is the whole design, and it is what let 27 organisations agree to release 539 farmer-managed trials and 100,787 sites under CC0, with 130 researcher-managed companion trials on the same genotypes. Methods explains how heterogeneous sources became one schema without losing provenance or exposing farmers. Data Records describes the files and their coverage. Usage Notes explains how people and AI agents fetch the data and how credit flows back to the producers, which is the part of the design that made the release possible.

![Trial sites in the dataset, by crop group. Each point is one site after displacement within 1,500 m. Equal Earth projection, EPSG:8857. Administrative boundaries from Natural Earth. The authors remain neutral with regard to jurisdictional claims in maps.](trial-locations.png)

Trial sites in the dataset, by crop group. Each point is one site after displacement within 1,500 m. Equal Earth projection, EPSG:8857. Administrative boundaries from Natural Earth. The authors remain neutral with regard to jurisdictional claims in maps.

The data come from the tricot approach, proposed in 2011 for large-scale, decentralised citizen science in agriculture^(5,6). Options from a larger pool are allocated to participants in connected incomplete blocks of three. A farmer receives three coded seed lots, grows them side by side under local management and reports which performed best and worst for each trait. Because the triads overlap, the partial rankings of many farmers form one comparison network, and the relative performance of every option is estimated with Plackett-Luce models although nobody evaluates the full set^(1,7,8). Fifteen years of trials showed that these distributed observations support inference across environments¹, predict across-season performance better than station-trained genomic models⁹, extend to consumer tests and product concepts^(10,11), and aggregate into reliable estimates although each farmer’s ranking is imperfect^(12,13). The same fifteen years are why the data were scattered. Each trial was designed, run and owned by the organisation that needed the answer, a national research institute, a CGIAR centre, an NGO, a seed company or a university, under its own project and funder, on successive versions of the ClimMob platform (<https://climmob.net/>) or in partner files^(8,14). Nobody owned the whole.

Three features follow from the design and set the dataset apart from the trial data breeding programmes usually release. It was built from the bottom up, so the trust of the producers is written into the files. Each trial carries its producer with a ROR identifier, its principal investigator, its contributors and its funder, and credit travels with the data wherever they go. It is wide rather than deep. One trial of 150 farms says little on its own, while several hundred trials on 42 crops across 20 countries and 15 seasons let an analyst ask which varieties perform where, how preferences differ between women and men, how performance responds to climate and whether on-farm and on-station results agree. And it is FAIR in full¹⁵ for people and for AI agents alike. One index lists every trial with the path to fetch it, one JSON file per trial describes itself completely, and nothing requires an account, a token or a database client.

![Cumulative growth of the farmer-managed tricot trials by start year. The area gives the cumulative number of sites (participants) of all 539 trials, 100,787 in total. For 44 trials without a start date in the index the year was taken from the project code or, for 18 trials, from the source project \[XX, confirm the 18 estimated years in `metadata/questions/`\].](tricot-sites-cumulative.png)

Cumulative growth of the farmer-managed tricot trials by start year. The area gives the cumulative number of sites (participants) of all 539 trials, 100,787 in total. For 44 trials without a start date in the index the year was taken from the project code or, for 18 trials, from the source project \[XX, confirm the 18 estimated years in `metadata/questions/`\].

This release is the production version of the dataset. A pilot release in 2025 tested the format on 62 trials from six African countries. The data described here replace the pilot, with a new schema, naming rule and licence, so users should work with this release and treat the pilot as superseded. It harmonises trials collected with every version of ClimMob since 2012, together with partner files and 130 researcher-managed companion trials on the same genotypes, into one schema, one naming rule and one licence, and sets the standard for future publications of tricot data from ClimMob. It is also the first release that reaches beyond Africa, to India and Central America, where tricot began.

# Methods

## The tricot design

The unit of the design is the set of three technology options evaluated by one participant. The options are most often crop varieties and breeding lines, and sometimes food products, agronomic practices or product concepts. The pool usually holds 8 to 12 comparable options and never fewer than two. In this release the median trial compares 10 genotypes and the largest 48. Each participant receives one incomplete block of three, which keeps the entry threshold low in land, time and training. Blocks are allocated at random by ClimMob in a sequentially balanced way, so that each option appears at roughly equal frequency and every village receives a balanced set even when distribution stops early^(6,14). Packages are coded A, B and C and the identity of the options is revealed only after the data are collected, to prevent bias toward familiar names.

Participants grow the three options at the same time in adjacent plots of equal size, under their own management, which maximises external validity. At defined crop stages they record on pictorial observation cards which option is best and which is worst for each trait, for instance germination, vigour, pest and disease resistance, drought tolerance, yield, taste and marketability, and for overall preference. Many trials also compare each option with the farmer’s own local variety, and a growing share records measured traits such as plot yield or days to maturity. Field agents, usually extension officers, digitise the cards with the ODK Collect app and submit them to ClimMob, where the project owner keeps full rights over the data¹⁴. Ranks are stored as the position of each option within the site, 1 being best, and the middle rank is inferred from the best and worst answers. Rankings are analysed with Plackett-Luce models, which estimate for each option the probability of outranking the others^(7,16), and with model-based recursive partitioning to find the climatic, geographic or socio-economic covariates that change the ranking^(1,17).

## Sources of the data

The release combines four kinds of sources (Table 1). ClimMob projects were exported with their project metadata, registration survey, data collection forms and package allocation, or recovered from curated spreadsheets prepared for earlier publications^(1,2). Partner files cover trials run with the tricot design outside ClimMob, among them the national on-farm trials of Integrated Seed Sector Development Ethiopia, the NextGen Cassava trials in Nigeria and Tanzania and the consumer and concept tests run with video-based ranking tools^(11,18). Researcher-managed companion data come from breeding databases and spreadsheets of the same programmes, 96 on-station breeding trials of cassava in Nigeria and groundnut in Tanzania and 34 participatory varietal selection mother trials of bread wheat and rice in India, which evaluate the same genotypes under researcher control.

**Table 1. Sources of the trial files.**

| Source platform                                                                      | Trial files | Sites   | Years        |
|--------------------------------------------------------------------------------------|-------------|---------|--------------|
| ClimMob projects (platform exports and curated spreadsheets)                         | 526         | 89,901  | 2012 to 2026 |
| Partner spreadsheets and video-based ranking surveys (tricot design outside ClimMob) | 13          | 10,886  | 2013 to 2025 |
| Breeding database exports and PVS spreadsheets (researcher-managed companion data)   | 130         | 596     | 2011 to 2023 |
| Total                                                                                | 669         | 101,383 | 2011 to 2026 |

## From source to release file

Two objections meet any pooled on-farm dataset. The trials are not comparable, and the farmers’ data are not protected. The pipeline answers both first, then takes care of identity, direction, credit, companion data and review. It runs as one command, kept under version control in the private working repository of the data managers, and rebuilds every release file from its source, so nothing in the published files is edited by hand. Each step has a decision record that states the question, the method, the alternatives rejected and what the result does and does not support. The records are published with the data so that a reader, or an AI agent, can judge whether a choice fits their own question.

**Harmonisation of traits and variables.** Every question asked in a trial was mapped to a canonical question in a harmonised question library of 414 entries that the protocols team maintains for all tricot crops¹⁹. The published trait name is the cleaned canonical code, so a yield ranking from Nicaragua in 2014 and one from Rwanda in 2024 share one name and one identifier. The label as asked is preserved in `trait_original`, and the identifier in `trait_id`, so every harmonised name traces back to its source. Where a Crop Ontology term exists for the crop and trait, its identifier is attached to the variable definition²⁰. Labels with no canonical match keep an English name and an empty identifier, and are listed as open questions for the producer.

**Personal data.** Names, relatives’ names, phone numbers, device identifiers, photographs, enumerator identities and free text are never exported. Administrative names and local variety names are kept. Gender and age are published as categories. Coordinates are checked against the national boundary, and points outside the country, with swapped axes or a flipped sign are removed and flagged. Every site coordinate is then displaced to a random point within 1,500 m, reproducibly per site id, and redrawn if it leaves the country. Sites published in the 2025 pilot release keep their coordinates so that no site receives two independent displacements. Sites without a coordinate take the mean of other located sites in the same village or district before displacement, with the source and the spread of the pool recorded in `coordinate_source` and `coordinate_uncertainty_m`. In this release 86,180 sites carry a point and 15,192 do not, the latter mostly consumer and concept tests and early India trials that recorded no village name.

**Identifiers.** The trial identifier is the only identity of a trial. Current ClimMob projects keep their 12-character hexadecimal project id. Older trials receive the first 11 hexadecimal characters of the MD5 hash of the country code and the original project code, followed by the letter v. The id has the same length and alphabet as a current id, cannot collide with one and reproduces itself on every run, which is how duplicates across sources were found. Sites are identified as trial id plus package number, and plots as the package letter A, B or C.

**Direction of rankings.** Some legacy forms asked which option was worst in the first column and best in the second, and some traits, such as faster maturity, can be valued in either direction. A check compares, at every site, the option ranked best for a trait with the option ranked best overall. Traits farmers value agree with the overall choice well above chance, and the median agreement of a trait across all trials describes how it normally behaves. A trait that agrees elsewhere but, in one trial, is the overall worst on more than half of at least 40 sites is reversed and every affected row is flagged `direction_inverted`. Ten trait and trial pairs were reversed. The same check exposed three bread wheat trials from India in 2014 whose answers are shifted across columns and cannot be repaired without the paper forms. They are published with a `consistency_warning` in their metadata.

**Producers, contributors and funders.** Where the source recorded no producer, the principal investigator of the ClimMob project and the project code decide, with the confidence and the evidence recorded in a reviewed table. An inferred producer carries an `attribution` note in the file, so a reader tells it from a stated one. Organisations were resolved against the Research Organization Registry; 20 of the 27 producers have a ROR identifier. Contributors follow the DataCite contributor types, with the authors of the publication that first used the data as project members, the person who curated the data as data curator and the data managers as such. Funders carry the award title and number where known. 204 trials name their funder, the largest being the CGIAR Research Program on Climate Change, Agriculture and Food Security (116 trials), the Gates Foundation (49), the Global Environment Facility (22), Norad (21), BMZ (20), SDC (20) and USAID (18).

**Companion data.** Researcher-managed trials on the same genotypes are published in the same schema with `pvs` or `station` in the file name and `management = researcher-managed` in the index. Each companion file lists in `metadata.companion_of` the tricot trials it relates to, and each tricot file lists in `metadata.companions` the companion files that carry data on its genotypes, with the shared genotype names. Measurements made on genotypes outside the trial design, such as laboratory assays of a food product, go to a `genotype_data` table in the tricot file itself. The three tables share the same observation columns, so an analyst joins them on `genotype_name` to fit models with item covariates.

**Review before release.** The processed files were reviewed as a whole before this release. Variable names were cleaned of question identifiers, genotype names of crop prefixes, spellings merged within trials, taxa given one spelling per crop and placeholders set to null. Every open question that the data could answer was answered and marked as decided, 176 proposed answers were sent to the producers for confirmation and 104 questions remain open, mostly release years of varieties, author lists, plot sizes and units. Forty further trials with 8,571 sites were processed but held back pending the approval of their producers. They will enter the next version.

## Standard operating procedures

Most trials in this release predate the first published multi-crop collection of tricot standard operating procedures, pilot release v0.1 of 2026¹⁹, which holds complete protocols for 20 crops and the core question library, and the crop-specific sweetpotato SOP published in English, French, Portuguese and Spanish²¹. The harmonised question library used here is the same one those protocols are built on, so trials run under the SOPs from 2026 onwards enter the dataset without further mapping. About 80 percent of current ClimMob trials already follow an SOP, and the data structure described here is what those SOPs produce. In both directions the protocols and the data share trait definitions, identifiers and Crop Ontology terms, which is what lets on-farm rankings sit beside on-station measurements in breeding systems^(20,22).

# Data Records

The release is published on GitHub at AgrDataSci/tricot-data and archived on Zenodo under the concept DOI 10.5281/zenodo.17112491²³. The concept DOI always resolves to the latest version. The version DOI of this release, \[10.5281/zenodo.XX\], identifies the exact files described in this Data Descriptor. The GitHub release is tagged with the same version, and the Zenodo record holds the full data, never only a link. All files are released under CC0 1.0.

**Layout.** `datasets.csv` at the repository root is the finding aid, one row per trial with the trial id, crop and taxon, country, data producer, start and end dates, trial type and management, objective, numbers of participants, sites, genotypes and observation rows, the collection moments, the ranked traits, the size in megabytes, the licence and the path to fetch. Trial files sit in `data/<crop>/` and are named `<crop>-[kind-]<iso3>-<trial_id>.json`, where `kind` is `pvs` or `station` for companion data. Crop, country and year do not identify a trial, 313 of the 366 legacy files share all three with another file, so the trial id closes every name and no user composes a file name by hand. `metadata/` holds the JSON Schema, the data dictionary and the DataCite metadata of the release, `decisions/` the decision records of the pipeline, and `docs/` the map and this paper. The release is 1.2 GB of minified JSON. A single trial file is read with any JSON parser, and a user fetches the five trials they need, not the whole set.

**File structure.** Every trial file follows the same release schema with five parts (Table 2). `metadata` describes the trial, `site_data` has one row per site, `plot_data` is the long table of observations, `genotype_data` holds measurements on genotypes outside the trial design and `rank_analysis` is reserved for Plackett-Luce estimates. Site and plot rows link through `site_id`, genotype rows through `genotype_name`, and `metadata.variables` lists and describes every column of the three tables with its harmonised question id, Crop Ontology term, value type and unit. Every file states its schema version, the version and date of the release, the export script and interpreter that produced it, the concept DOI and the licence, so a file found anywhere identifies the dataset it belongs to.

**Table 2. Organisation of the trial files.**

| Component     | Record unit                                         | Principal contents                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|---------------|-----------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| metadata      | One object per trial                                | Trial id, code, title, type, objective, source platform and collection moments; start and end dates; bounding box and location privacy method; data producer with ROR id, principal investigator and programme; contributors with ORCID and DataCite type; crop and taxon; participants by gender; genotypes with role, release year and origin; funders with award; links to companion files; one definition per variable with harmonised id, ontology term, value type and unit; changelog |
| site_data     | One row per site (farm, household or station plot)  | site_id, trial id, country, crop, displaced longitude and latitude with source, uncertainty and flag, adm1 to adm3, planting and harvest dates, plot area, registration and survey variables                                                                                                                                                                                                                                                                                                 |
| plot_data     | One row per site, plot, collection moment and trait | site_id, plot (A, B, C or station plot), genotype_name, collection moment and days after planting, trait, trait_id, trait_original, value, value_type (rank, numeric, categorical, date, vs_local, multiple), unit, flag                                                                                                                                                                                                                                                                     |
| genotype_data | One row per genotype, trait and replicate           | Measurements made outside the trial design, such as laboratory assays of the product                                                                                                                                                                                                                                                                                                                                                                                                         |
| rank_analysis | One row per genotype, trait and moment              | Plackett-Luce estimates, empty until fitted by the data managers                                                                                                                                                                                                                                                                                                                                                                                                                             |

**Coverage.** The dataset holds 669 trial files from 20 countries and 42 crops, produced by 27 organisations (Figure 1, Table 3, Supplementary Table S1). The 539 farmer-managed tricot trials comprise 527 on-farm variety trials, 8 consumer tests and 4 concept tests, with 100,787 sites, each the farm or household of one participant, and 3.26 million observation rows. The 130 researcher-managed companion trials add 596 station and mother-trial sites. Across all files `plot_data` holds 3,284,321 rows, of which 2,439,450 are rankings, 509,005 numeric measurements, 99,465 categorical answers, 93,403 dates and 90,511 comparisons with the local variety, and `genotype_data` holds 7,200 rows. Participants were 30,502 women, 40,423 men and 30,362 whose gender was not recorded. Participant identifiers are specific to each trial, so counts summed across trials are not counts of unique people. The trials cover 7,511 genotype entries, 451 published trait names and 414 harmonised question identifiers, 40 of them mapped to Crop Ontology terms. Ethiopia (227 trials, 24,887 sites), India (67 trials, 21,804 sites) and Tanzania (48 trials, 15,265 sites) hold the most sites, and common bean, bread wheat, maize, groundnut and rice the most among crops. Trials started every year from 2012 to 2026. The dataset grew in two waves, the pilots and national programmes of 2013 to 2014 and the ClimMob expansion of 2017 to 2019, and has added a steady 7,500 to 10,000 sites per year since 2021 (Figure 2).

**Table 3. Summary of the dataset.**

| Indicator                                  | Value                                                             |
|--------------------------------------------|-------------------------------------------------------------------|
| Trial files                                | 669 (539 farmer-managed tricot, 130 researcher-managed companion) |
| Countries                                  | 20                                                                |
| Crops                                      | 42                                                                |
| Data-producing organisations               | 27 (20 with ROR identifier)                                       |
| Sites                                      | 101,383 (100,787 tricot, 596 companion)                           |
| Sites with coordinates                     | 86,180                                                            |
| Genotype entries                           | 7,511                                                             |
| Observation rows in plot_data              | 3,284,321 (2,439,450 ranks, 509,005 numeric)                      |
| Rows in genotype_data                      | 7,200                                                             |
| Harmonised question identifiers            | 414, with 451 published trait names                               |
| Participants by gender                     | 30,502 women, 40,423 men, 30,362 not stated                       |
| Trials with a named funder                 | 204                                                               |
| Trials held in quarantine pending approval | 40 (8,571 sites)                                                  |
| Size                                       | 1.2 GB, minified JSON                                             |
| Licence                                    | CC0 1.0                                                           |

# Technical Validation

Each step of the pipeline carries its own check, with counts, in the decision records published with the data. The checks below are the ones that hold for the release as a whole.

**Design integrity.** Every tricot site holds exactly three plots of distinct genotypes from the trial pool, and every ranking is a complete order of those three. Ties, incomplete answers and package codes used by two participants with different answers were dropped at export and counted in the export reports, so no ranking in the release is ambiguous. Legacy trials with fewer than five ranked sites were not published (391 early ClimMob projects holding 30 ranked records in all), nor were 17 Ethiopian projects shown to be copies of another project, matched package by package.

**Consistency.** After the ten documented reversals the direction check returns no further candidate on any trait of any trial. The three inconsistent India wheat trials stay in the release with a machine-readable warning rather than being dropped, because their site, management and yield data remain sound.

**Identity and privacy.** No two files map to the same path, and every file carries the same trial id in its name, its metadata and the index. Every published point lies inside its country. No value in `site_data` matches a phone number or email pattern, and no published column refers to names, phones, device identifiers or photographs.

**Schema validation.** Every file is validated against the JSON Schema of the release before it is tagged \[XX, confirm once the schema is written and the validation run\]. The index is rebuilt from the files, so its counts agree with the files by construction.

**Reproducibility.** The whole release is rebuilt by one command from the sources, and each file records the script and interpreter version that wrote it. Independent reanalysis of the published files reproduces results of earlier studies built on subsets of the same data \[XX, cite the reproduction in tricot-analysis-workflows once run, e.g. Brown et al. 2022 for the Central American beans\].

# Usage Notes

**Finding and fetching.** Start from `datasets.csv`. Filter by crop, country, year, type, management, trait or producer, then fetch the files in the `path` column from the GitHub raw URL or from the Zenodo archive. A person, a script or an AI agent needs one URL per file and no login. The repository ships a skill file (`SKILL.md`), an `AGENTS.md` and an `llms.txt` that tell an AI assistant what the data hold, how to fetch them and how to cite them. The README gives a worked example in R that reads a trial file, builds the rankings with the gosset package and fits a Plackett-Luce model with PlackettLuce^(7,16); the ClimMobTools package reads the same structure. The companion tricot-analysis-workflows repository holds the team’s curated workflows, from variety recommendation and genotype by environment analysis to the integration of trial sites with climate and soil data.

**Pooling trials.** The harmonised trait names and identifiers make pooling straightforward, but the analyst decides what to pool. Trait wording, collection moments and local varieties differ among trials, and `trait_original`, `collection_moment` and the variable descriptions carry what is needed to judge comparability. Trials with a `consistency_warning` should be read before use in trait by trait analysis. Rankings flagged `direction_inverted` were corrected and the flag is informational.

**Covariates.** Displaced coordinates are fit for linking sites to gridded climate, soil and elevation data at kilometre resolution^(1,2,24), and the climatrends and chirps R packages were written for that purpose. They are not farm locations. Planting dates let an analyst compute season-specific covariates, and the companion files let item covariates measured on station enter the model.

**Credit and citation.** CC0 places no legal obligation on users. The community norm the dataset asks for is the one GBIF (<https://www.gbif.org>) asks of its users. Cite the version DOI you used and name the concept DOI so others find the current one. Cite, alongside the dataset, the publications that first used the trials you reused, which are listed in each file’s metadata, and list the trial ids you used in your paper or supplement. Author lists grow with versions, since each Zenodo version carries its own creators, so producers added in a later batch appear from that version on.

**Governance.** Producers approve the release of their trials, and 40 processed trials wait in quarantine for that approval. Personally identifiable information never leaves the ClimMob project owner’s control. Local variety names are published because they are part of the agricultural knowledge the trials record, and the dataset makes no claim on the traditional knowledge they carry. The dataset is maintained by two data managers in the margins of other work, which is why the design puts everything into self-describing files and one index, with no database or API to keep alive.

**What comes next.** Version 3 will add the quarantined trials, the Plackett-Luce estimates in `rank_analysis`, fuller Crop Ontology coverage and the trials run under the published SOPs. Trials collected through speech-based interfaces and trials that link to station data through breeding information systems will enter the same files, since the schema already carries companion links and measured traits. The dataset is one layer of a documented ecosystem that also publishes the protocols that produce the data, the evidence built on them and the analysis workflows that use them, each with its own identifier, so that a user can move from a question to the trials behind the answer.

# Code availability

The data are archived on Zenodo under the concept DOI 10.5281/zenodo.17112491 and maintained on GitHub at https://github.com/AgrDataSci/tricot-data under CC0 1.0. The export pipeline and its decision records are in the same repository under `script/` and `decisions/` \[XX, confirm what is copied from the private working repository\]. The R packages gosset, ClimMobTools, climatrends and chirps are on CRAN under the MIT licence. ClimMob is open source at https://github.com/BioversityCostaRica \[XX, confirm URL\].

# Author contributions

\[CRediT taxonomy, to complete with the final author list.\] Conceptualization, K.d.S., J.v.E.; data curation, K.d.S., M.-A.L.; methodology, K.d.S., M.-A.L., A.C.; software, K.d.S., B.Y.; investigation and resources, all data producers; writing, original draft, R.C., A.C., K.d.S.; writing, review and editing, all authors; supervision and funding acquisition, J.v.E., K.d.S.

# Competing interests

The authors declare no competing interests.

# Acknowledgements

This work was supported by the Gates Foundation through 1000FARMS (INV-031561), by the Kingdom of Norway through the Global Crop Diversity Trust (BOLD Work Package 7, CONT-1773) and by the CGIAR Digital Transformation Accelerator (2026 Package of Deliverables). The trials themselves were funded by the CGIAR Research Program on Climate Change, Agriculture and Food Security, the Global Environment Facility, the Norwegian Agency for Development Cooperation, the German Federal Ministry for Economic Cooperation and Development, the Swiss Agency for Development and Cooperation, the United States Agency for International Development, the Research Council of Norway and the programmes named in each trial file. We thank the farmers who ran the trials, the field agents who collected the data, Robert Hijmans for his recommendations on the data structure, and Marcia Petraglia, Francesca Guzzo, Elena Zurli, Anastacia Safarian, Karol Araya, Luis Allen, Edith Odhiambo, Rachel Gitau, Esther Wambugu and Eszter Szarka for administrative support of the grants that led to this work.

# References

1. van Etten, J. *et al.* [Crop variety management for climate adaptation supported by citizen science](https://doi.org/10.1073/pnas.1813720116). *Proceedings of the National Academy of Sciences* **116**, 4194–4199 (2019).

2. Brown, D. *et al.* [Rank‐based data synthesis of common bean on‐farm trials across four central american countries](https://doi.org/10.1002/csc2.20817). *Crop Science* **62**, 2246–2266 (2022).

3. Nanyonjo, A. R. *et al.* [On‐farm evaluation of cassava clones using the triadic comparison of technology options approach](https://doi.org/10.1002/csc2.21293). *Crop Science* **64**, 2679–2697 (2024).

4. Daudi, H. *et al.* [Analysis of genotype‐by‐environment interaction, farmers’ preferences, and gender response in groundnut breeding in tanzania using triadic comparison of technologies](https://doi.org/10.1002/fes3.70228). *Food and Energy Security* **15**, (2026).

5. van Etten, J. [Crowdsourcing crop improvement in sub-saharan africa: A proposal for a scalable and inclusive approach to food security](https://doi.org/10.1111/j.1759-5436.2011.00240.x). *IDS Bulletin* **42**, 102–110 (2011).

6. van Etten, J. *et al.* [First experiences with a novel farmer citizen science approach: Crowdsourcing participatory variety selection through on-farm triadic comparisons of technologies (tricot)](https://doi.org/10.1017/s0014479716000739). *Experimental Agriculture* **55**, 275–296 (2016).

7. Turner, H. L., Etten, J. van, Firth, D. & Kosmidis, I. [Modelling rankings in R: The PlackettLuce package](https://doi.org/10.1007/s00180-020-00959-3). *Computational Statistics* **35**, 1027–1057 (2020).

8. de Sousa, K. *et al.* [The tricot approach: An agile framework for decentralized on-farm testing supported by citizen science. A retrospective](https://doi.org/10.1007/s13593-023-00937-1). *Agronomy for Sustainable Development* **44**, (2024).

9. de Sousa, K. *et al.* [Data-driven decentralized breeding increases prediction accuracy in a challenging crop production environment](https://doi.org/10.1038/s42003-021-02463-w). *Communications Biology* **4**, (2021).

10. Moyo, M. *et al.* [Consumer preference testing of boiled sweetpotato using crowdsourced citizen science in ghana and uganda](https://doi.org/10.3389/fsufs.2021.620363). *Frontiers in Sustainable Food Systems* **5**, (2021).

11. Donovan, J., Rutsaert, P., Mawia, H., Sousa, K. de & Etten, J. van. [Farmers’ preferences for the next generation of maize hybrids: Application of product concept testing in kenya and uganda](https://doi.org/10.1017/s001447972500002x). *Experimental Agriculture* **61**, (2025).

12. Steinke, J., van Etten, J. & Zelan, P. M. [The accuracy of farmer-generated data in an agricultural citizen science methodology](https://doi.org/10.1007/s13593-017-0441-y). *Agronomy for Sustainable Development* **37**, (2017).

13. van de Gevel, J., van Etten, J. & Deterding, S. [Citizen science breathes new life into participatory agricultural research. A review](https://doi.org/10.1007/s13593-020-00636-1). *Agronomy for Sustainable Development* **40**, (2020).

14. Quirós, C. *et al.* [ClimMob: Software to support experimental citizen science in agriculture](https://doi.org/10.1016/j.compag.2023.108539). *Computers and Electronics in Agriculture* **217**, 108539 (2024).

15. Wilkinson, M. D. *et al.* [The FAIR guiding principles for scientific data management and stewardship](https://doi.org/10.1038/sdata.2016.18). *Scientific Data* **3**, (2016).

16. Sousa, K. de, Etten, J. van, Brown, D. & Steinke, J. Gosset: Tools for data analysis in experimental agriculture. (2025) doi:[10.5281/zenodo.3976631](https://doi.org/10.5281/zenodo.3976631).

17. Zeileis, A., Hothorn, T. & Hornik, K. [Model-based recursive partitioning](https://doi.org/10.1198/106186008X319331). *Journal of Computational and Graphical Statistics* **17**, 492–514 (2008).

18. Olaosebikan, O. *et al.* [Drivers of consumer acceptability of cassava gari‐eba food products across cultural and environmental settings using the triadic comparison of technologies approach (tricot)](https://doi.org/10.1002/jsfa.12867). *Journal of the Science of Food and Agriculture* **104**, 4770–4781 (2023).

19. Cremaschi, A., Sousa, K. de, Laporte, M.-A. & Etten, J. van. Standard operating procedures for on-farm testing with the tricot approach. Pilot release v0.1. (2026) doi:[10.5281/zenodo.18428279](https://doi.org/10.5281/zenodo.18428279).

20. Pietragalla, J. *et al.* *Guidelines for Creating Crop-Specific Ontologies to Annotate Phenotypic Data. Version 2.1*. <https://hdl.handle.net/10568/110906> (2024).

21. XX authors. Sweetpotato on-farm testing with the tricot approach: Standard operating procedure, version 1. (2026) doi:[10.5281/zenodo.20525269](https://doi.org/10.5281/zenodo.20525269).

22. Selby, P. *et al.* [BrAPI v2: Real-world applications for data integration and collaboration in the breeding and genetics community](https://doi.org/10.1093/database/baaf048). *Database* **2025**, (2025).

23. Sousa, K. de, Laporte, M.-A., *et al.* Global multi-crop agricultural trial data supported by citizen science. (2025) doi:[10.5281/zenodo.17112491](https://doi.org/10.5281/zenodo.17112491).

24. Kang, G. *et al.* [Integrating environmental, socio-economic, and biological data in a farmer-led potato trial for enhanced varietal assessment in rwanda](https://doi.org/10.1017/s0014479725100100). *Experimental Agriculture* **61**, (2025).
