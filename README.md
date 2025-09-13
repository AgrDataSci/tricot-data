<!-- badges: start -->
[![License](https://img.shields.io/badge/license%20-%20CC%20BY%20SA%204.0%20-%20%233182bd)](https://creativecommons.org/licenses/by-sa/4.0/) 
[![Static Badge](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.17112492-blue)](https://doi.org/10.5281/zenodo.17112492)
<!-- badges: end --> 

# Global multi-crop agricultural trial data supported by citizen science

The [triadic comparison of technologies (tricot)](https://doi.org/10.1007/s13593-023-00937-1) is a citizen science approach for testing technology options in their target environments, which has been applied to on-farm testing of crop varieties. ‘Triadic’ refers to the sets of three technology options that are compared by each participant. In the approach, participants are invited to test a anonymous set of three technologies (out of a larger number, generally between 5 to 20) randomly assigned. Between 2011 and 2025 the tricot approach was applied in more than 25 countries across Africa, Asia, Europe and Latin America with more than 30 crops.

---

## Crops and sites currently covered by the dataset

The map below shows the crops and trial sites that are currently available for public use. Each point represents a trial location, and the legend indicates the crop(s) associated with those sites. This visualization helps to quickly identify the regions and crops that are currently represented in the dataset.


![Trial map](docs/trial-locations.png)

## Data structure

The dataset follows a structured format designed to ensure consistency and interoperability across trials. It is organized into four main components: **metadata**, **block data**, **plot data**, and **rank analysis**.  

- **Metadata**: Provides detailed contextual information about each trial, including identifiers, objectives, location, time frame, crops, genotypes, variables, and contributors. This ensures that each dataset is fully documented and reusable under the terms of the CC BY-SA 4.0 license.  
- **Block data**: Contains information at the block level (e.g., farmer or experimental unit), including geolocation, planting dates, and farmer demographics.  
- **Plot data**: Stores trial-level measurements for each genotype and trait, with flexible support for different data types (rank, numeric, text, date).  
- **Rank analysis**: Includes the results of statistical analyses performed on ranked data, providing estimates, standard errors, and variance components.  

The hierarchical organization allows users to track data from trial design through analysis while maintaining links between metadata, blocks, and plots.  

``` text
├── metadata
│   ├── changelog
│   │   ├── version
│   │   ├── notes
│   │   ├── software
│   │   │   ├── package
│   │   │   ├── package version
│   ├── identifier (doi)
│   ├── license
│   ├── publication year
│   ├── study
│   │   ├── id
│   │   ├── country (ISO2)
│   │   ├── title
│   │   ├── description
│   │   ├── type
│   │   ├── unit of analysis
│   │   ├── objective
│   │   ├── experimental site
│   ├── date
│   │   ├── start (YYYY-MM-DD)
│   │   ├── end (YYYY-MM-DD)
│   ├── geo location
│   │   ├── bounding box
│   │   │   ├── xmin (0.00)
│   │   │   ├── xmax (0.00)
│   │   │   ├── ymin (0.00)
│   │   │   ├── ymax (0.00)
│   ├── data producer 
│   │   ├── name
│   │   ├── identifier (ROR id)
│   │   ├── principal investigator
│   │   ├── email
│   │   ├── program (ADCIN, RTB, BOLDER)
│   ├── crop 
│   │   ├── name
│   │   ├── taxon
│   ├── participants
│   │   ├── total 
│   │   ├── men
│   │   ├── women
│   ├── genotypes
│   │   ├── genotype name
│   │   ├── role
│   │   ├── year release
│   │   ├── market segment
│   │   ├── country of origin
│   │   ├── remarks
│   ├── variables
│   │   ├── variable name
│   │   ├── description
│   │   ├── ontology id
│   │   ├── value type
│   │   ├── unit
│   │   ├── controlled vocabulary
├── block data
│   ├── block id
│   ├── longitude
│   ├── latitude
│   ├── planting date
│   ├── gender
│   ├── age
│   ├── ...
│   ├── any other data
├── plot data
│   ├── block id
│   ├── genotype name
│   ├── trait
│   ├── collection moment 
│   ├── value
│   ├── value type (rank, numeric, text, date)
├── rank analysis
│   ├── collection moment
│   ├── trait
│   ├── genotype name
│   ├── estimate
│   ├── se
│   ├── casi se
│   ├── casi var
```

## Citation

The dataset is archived and made publicly available through [Zenodo](https://doi.org/10.5281/zenodo.17112492).  

Users of this dataset are required to provide proper citation in any publications, presentations, or derivative works.

> The dataset is shared under the [CC BY-SA 4.0 license](https://creativecommons.org/licenses/by-sa/4.0/) which allows others to use, share, and adapt the material, provided appropriate credit is given and any derivative works are distributed under the same license.

de Sousa, K., Laporte, M.-A., Abate, L., Abdulmalik, R. O., Abidin, E., Abolore, B., Achigan-Dako, E. G., Aglinglo, L. A., Aguilar, A., Angudubo, S., Arnaud, E., Assefa, T. M., Atieno, E., Ayenan, M., Barrios, M., Borman, G., Boukar, O., Brown, D., Carey, E., Chaves, N., … van Etten, J. (2025). Global multi-crop agricultural trial data supported by citizen science [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.17112492


### BibTeX (dataset)

```@dataset{desousa_tricot_2025,
  author       = {de Sousa, Kauê and Laporte, Marie-Angélique and Abate, Legesse and Abdulmalik, Rekiya O. and Abidin, Erna and Abolore, Bello and Achigan-Dako, Enoch G. and Aglinglo, Lys Amavi and Aguilar, Amilcar and Angudubo, Stephen and Arnaud, Elizabeth and Assefa, Teshale M. and Atieno, Elly and Ayenan, Mathieu and Barrios, Mirna and Borman, Gareth and Boukar, Ousmane and Brown, David and Carey, Edward and Chaves, Néstor and Chase, Rachel and … and van Etten, Jacob},
  title        = {Global multi-crop agricultural trial data supported by citizen science},
  year         = {2025},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.17112492},
  url          = {https://doi.org/10.5281/zenodo.17112492},
  note         = {Dataset}
}

