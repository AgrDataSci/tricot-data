<!-- badges: start -->
[![License](https://img.shields.io/badge/license%20-%20CC%20BY%20SA%204.0%20-%20%233182bd)](https://creativecommons.org/licenses/by-sa/4.0/legalcode.en) 
<!-- badges: end --> 

# Global multi-crop agricultural trial data supported by citizen science

The [triadic comparison of technologies (tricot)](https://doi.org/10.1007/s13593-023-00937-1) is a citizen science approach for testing technology options in their target environments, which has been applied to on-farm testing of crop varieties. ‘Triadic’ refers to the sets of three technology options that are compared by each participant. In the approach, participants are invited to test a anonymous set of three technologies (out of a larger number, generally between 5 to 20) randomly assigned. Between 2011 and 2025 the tricot approach was applied in more than 25 countries in across Africa, Asia, Europe and Latin America with more than 30 crops.

---

## Crops and sites currently covered by the dataset

The map below shows the crops and trial sites that are currently available for public use. Each point represents a trial location, and the legend indicates the crop(s) associated with those sites. This visualization helps to quickly identify the regions and crops that are currently represented in the dataset.  The dataset is shared under the [CC BY-SA 4.0 license](https://creativecommons.org/licenses/by-sa/4.0/legalcode.en), which allows others to use, share, and adapt the material, provided appropriate credit is given and any derivative works are distributed under the same license.


![Trial map](docs/trial-locations.png)

## Data structure

The dataset follows a structured format designed to ensure consistency and interoperability across trials. It is organized into four main components: **metadata**, **block data**, **plot data**, and **rank analysis**.  

- **Metadata**: Provides detailed contextual information about each trial, including identifiers, objectives, location, time frame, crops, genotypes, variables, and contributors. This ensures that each dataset is fully documented and reusable under the terms of the CC BY-SA 4.0 license.  
- **Block data**: Contains information at the block level (e.g., farmer or experimental unit), including geolocation, planting dates, and farmer demographics.  
- **Plot data**: Stores trial-level measurements for each genotype and trait, with flexible support for different data types (rank, numeric, text, date).  
- **Rank analysis**: Includes the results of statistical analyses performed on ranked data, providing estimates, standard errors, and variance components.  

The hierarchical organization allows users to track data from trial design through analysis while maintaining links between metadata, blocks, and plots.  

``` text
├── Tricot data 
│   ├── metadata
│   │   ├── changelog
│   │   │   ├── version
│   │   │   ├── notes
│   │   |   ├── software
│   │   │   │   ├── package
│   │   │   │   ├── package version
│   │   ├── doi
│   │   ├── license
│   │   ├── trial id
│   │   ├── trial name
│   │   ├── trial type
│   │   ├── trial experimental site
│   │   ├── trial unit of analysis
│   │   ├── trial objective
│   │   ├── trial description
│   │   ├── trial country (ISO2)
│   │   ├── date
│   │   │   ├── date start (YYYY-MM-DD)
│   │   │   ├── date end (YYYY-MM-DD)
│   │   ├── bounding box
│   │   │   ├── xmin (0.00)
│   │   │   ├── xmax (0.00)
│   │   │   ├── ymin (0.00)
│   │   │   ├── ymax (0.00)
│   │   ├── data producer name
│   │   ├── data producer email
│   │   ├── data producer institute
│   │   ├── program (ADCIN, RTB, BOLDER)
│   │   ├── crop name
│   │   ├── taxon
│   │   ├── N participants
│   │   ├── N men
│   │   ├── N women
│   │   ├── genotypes
│   │   │   ├── genotype name
│   │   │   ├── role in trial
│   │   │   ├── year release
│   │   │   ├── market segment
│   │   │   ├── country of origin
│   │   │   ├── remarks
│   │   ├── variables
│   │   │   ├── variable name
│   │   │   ├── description
│   │   │   ├── ontology id
│   │   │   ├── value type
│   │   │   ├── unit
│   │   │   ├── controlled vocabulary
│   ├── block data
│   │   ├── block id
│   │   ├── longitude
│   │   ├── latitude
│   │   ├── planting date
│   │   ├── gender
│   │   ├── age
│   │   ├── ...
│   │   ├── any other data
│   ├── plot data
│   │   ├── block id
│   │   ├── genotype name
│   │   ├── trait
│   │   ├── collection moment 
│   │   ├── value
│   │   ├── value type (rank, numeric, text, date)
│   ├── rank analysis
│   │   ├── collection moment
│   │   ├── trait
│   │   ├── genotype name
│   │   ├── estimate
│   │   ├── se
│   │   ├── casi se
│   │   ├── casi var
```

## Citation

The dataset is archived and made publicly available through [Zenodo](https://zenodo.org/).  
Users of this dataset are required to provide proper citation in any publications, presentations, or derivative works.  
de Sousa, K., Laporte, M.-A., Achigan-Dako, E. G., Aglinglo, L. A., Ayenan, M., Boukar, O., Coulibaly, H., Cremaschi, A., Diarra, D. Y., Dolo, A., Gandhi, H., Houdegbe, A. C., Dorado-Betancourt, H., Kileo, A., Legba, E. C., Madriz, B., Malulu, D., Manners, R., Matumbo, Z., Mlaki, A., N'Danikou, S., Nabateregga, M., Nadigatla, G. R., Ojiewo, C. O., Ongom, P. O., Ouedraogo, C. O., Shango, A., Sidibe, A., Solberg, S. Ø., van Heerwaarden, J., van Zonneveld, M., & van Etten, J. (2025). **Global multi-crop agricultural trial data supported by citizen science** (Version 1) [Dataset]. Zenodo. https://doi.org/XXXX/zenodo.XXXXX  *(DOI will be added upon release)*  

### BibTeX (dataset)
```bibtex
@dataset{desousa2025tricot,
  author       = {de Sousa, Kauê and Laporte, Marie-Angélique and Achigan-Dako, Enoch G. and Aglinglo, Lys Amavi and Ayenan, Mathieu and Boukar, Ousmane and Coulibaly, Harouna and Cremaschi, Almendra and Diarra, Danfing dit Youssouf and Dolo, Aminata and Gandhi, Harish and Houdegbe, Aristide Carlos and Dorado-Betancourt, Hugo and Kileo, Aishi and Legba, Eric C. and Madriz, Brandon and Malulu, Dickson and Manners, Rhys and Matumbo, Zamira and Mlaki, Anna and N'Danikou, Sognigbé and Nabateregga, Mabel and Nadigatla, Ganga Rao V. P. R. and Ojiewo, Christopher Ochieng and Ongom, Patrick Obia and Ouedraogo, Colette Ouidyam and Shango, Abdul and Sidibe, Amadou and Solberg, Svein Øivind and van Heerwaarden, Joost and van Zonneveld, Maarten and van Etten, Jacob},
  title        = {Global multi-crop agricultural trial data supported by citizen science},
  year         = {2025},
  publisher    = {Zenodo},
  version      = {v1},
  doi          = {10.5281/zenodo.XXXXX},
  url          = {https://doi.org/10.5281/zenodo.XXXXX}
}


