<!-- badges: start -->
[![License](https://img.shields.io/badge/license%20-%20CC%20BY%20SA%204.0%20-%20%233182bd)](https://creativecommons.org/licenses/by-sa/4.0/) 
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

> The dataset is shared under the [CC BY 4.0 license](https://creativecommons.org/licenses/by/4.0/). Proper attribution is required, and derivative data/products must credit the original authors and source.


de Sousa, K., Laporte, M.-A., Abdulmalik, R. O., Abidin, E., Abolore, B., Achigan-Dako, E. G., Aglinglo, L. A., Aguilar, A., Angudubo, S., Arnaud, E., Assefa, T. M., Atieno, E., Ayenan, M., Barrios, M., Borman, G., Boukar, O., Brown, D., Carey, E., Chaves, N., Coulibaly, H., Cremaschi, A., Daudi, H., Dawud, M., Diarra, D. Y., Dolo, A., Edughaen, G., Gandhi, H., Gaya Mohammed, S., Gebrehawaryat Kidane, Y., Gomez Cerna, M., Gupta, A., Hernández, J. C., Houdegbe, A. C., Hugo Dorado-Betancourt, H., Iragaba, P., Kadege, E., Kalemera, S., Kasubiri, F., Kawuki, R., Lakew, B., Legba, E. C., Madriz, B., Madu, T. U., Malulu, D., Manners, R., Matumbo, Z., Mendes, T., Mengistu, D. K., Mlaki, A., Moyo, M., Mugo, J., N’Danikou, S., Nabateregga, M., Nadigatla, G. R. V. P., Nanyonjo, A. R., Nduwumuremyi, A., Nshimiyimana, J. C., Nyawade, S., Nyirahabimana, H., Ojiewo, C. O., Olaosebikan, O., Ongom, P. O., Onwuka, S., Ouedraogo, C. O., Rosas, J. C., Rutsaert, P., Schaap, M., Shango, A., Sharma, N., Sharma, K., Sidibe, A., Solberg, S. Ø., Ssali, R., Steinke, J., Suchini, J. G., Teeken, B., Tufan, H. A., Tuyishime, E., Ulzen, J., van Heerwaarden, J., van Zonneveld, M., & van Etten, J. (2025). **Global multi-crop agricultural trial data supported by citizen science** (Version 1) [Dataset]. Zenodo. DOI: _to be provided upon release_.


### BibTeX (dataset)

```@dataset{desousa_2025_global_multicrop_tricot,
  title        = {Global multi-crop agricultural trial data supported by citizen science},
  author       = {de Sousa, Kau\ê and Laporte, Marie-Ang\'elique and Abdulmalik, Rekiya O. and Abidin, Erna and Abolore, Bello and Achigan-Dako, Enoch G. and Aglinglo, Lys Amavi and Aguilar, Amilcar and Angudubo, Stephen and Arnaud, Elizabeth and Assefa, Teshale M. and Atieno, Elly and Ayenan, Mathieu and Barrios, Mirna and Borman, Gareth and Boukar, Ousmane and Brown, David and Carey, Edward and Chaves, N\'estor and Coulibaly, Harouna and Cremaschi, Almendra and Daudi, Happy and Dawud, Maryam and Diarra, Danfing dit Youssouf and Dolo, Aminata and Edughaen, Gospel and Gandhi, Harish and Gaya Mohammed, Sanusi and Gebrehawaryat Kidane, Yosef and Gomez Cerna, Marvin and Gupta, Arnab and Hern\'andez, Juan Carlos and Houdegbe, Aristide Carlos and Hugo Dorado-Betancourt, Hugo and Iragaba, Paula and Kadege, Edith and Kalemera, Sylvia and Kasubiri, Fadhili and Kawuki, Robert and Lakew, Basazen and Legba, Eric C. and Madriz, Brandon and Madu, Tessy Ugo and Malulu, Dickson and Manners, Rhys and Matumbo, Zamira and Mendes, Thiago and Mengistu, Dejene K. and Mlaki, Anna and Moyo, Mukani and Mugo, James and N'Danikou, Sognigb\'e and Nabateregga, Mabel and Nadigatla, Ganga Rao V. P. R. and Nanyonjo, An Ritah and Nduwumuremyi, Athanase and Nshimiyimana, Jean Claude and Nyawade, Shadrack and Nyirahabimana, Hyacinthe and Ojiewo, Christopher Ochieng and Olaosebikan, Olamide and Ongom, Patrick Obia and Onwuka, Samuel and Ouedraogo, Colette Ouidyam and Rosas, Juan Carlos and Rutsaert, Pieter and Schaap, Mirjam and Shango, Abdul and Sharma, Neeraj and Sharma, Kalpana and Sidibe, Amadou and Solberg, Svein {\O}ivind and Ssali, Reuben and Steinke, Jonathan and Suchini, Jose Gabriel and Teeken, B\'ela and Tufan, Hale Ann and Tuyishime, Elys\'e and Ulzen, Jacob and van Heerwaarden, Joost and van Zonneveld, Maarten and van Etten, Jacob},
  year         = {2025},
  publisher    = {Zenodo},
  version      = {v1},
  type         = {dataset},
  rights       = {Creative Commons Attribution 4.0 International},
  rights_url   = {https://creativecommons.org/licenses/by/4.0/},
  doi          = {10.5281/zenodo.XXXXX},
  url          = {https://doi.org/10.5281/zenodo.XXXXX}
}
