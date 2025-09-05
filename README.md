<!-- badges: start -->
[![License](https://img.shields.io/badge/license%20-%20CC%20BY%20SA%204.0%20-%20%233182bd)](https://creativecommons.org/licenses/by-sa/4.0/legalcode.en) 
<!-- badges: end --> 

## The triadic comparison of technology options data from XX on-farm experimental trials in YY countries
> Kauê de Sousa, Marie-Angélique Laporte

The triadic comparison of technologies (tricot) is a citizen science approach for testing technology options in their target environments, which has been applied to on-farm testing of crop varieties. ‘Triadic’ refers to the sets of three technology options that are compared by each participant. In the approach, participants are invited to test a anonymous set of three technologies (out of a larger number, generally between 5 to 20) randomly assigned. […] Between 2011 and 2025 the tricot approach was applied in XX countries in Central America, Sub-Saharan Africa and Asia with YY crops.

---

## Repository structure

The project follows a project-oriented layout inspired by best practices in the R community.

```text

```
---

## Data structure

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
│   │   ├── trial objective
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


---

