# Estimating the Human Health Effects of Urban Heat in LA County

2024 Caltech Summer Undergraduate Research Fellowship (SURF) project 

Mentor: Hannah Druckenmiller

## Abstract

Since the Industrial Revolution, Earth’s global climate has warmed to unprecedented levels, driven largely and unequivocally by anthropogenic activities. This warming has led to wide-ranging consequences, including increased extreme weather events and poorer public health. Extreme heat in particular has become a key concern, due to its large role in public health as well as its prevalence in urban areas due to the urban heat island effect, in which the built environment elevates local temperatures. However, while the temperature increases associated with urban heat islands are comparable to those projected by end-of-century climate change scenarios, the impacts of urban heat have received relatively less attention from the public and policymakers. We aim to address this gap by estimating the human health effects of urban heat on mortality and morbidity across LA county at the zipcode level. We gather social vulnerability scores using socio-demographic data and incorporate measures of prolonged heat exposure derived from high-resolution, near-surface air temperature data into fixed effects and polynomial regression models. Our findings are generally consistent with past work in the literature and reveal that our measures and models may be applicable for informing mitigation measures at the local level. 

## Repository overview 
This repository contains the data, code, and results for estimating the health effects of urban heat in LA county. To replicate the analysis, start by cloning this repository, removing the processed and figures folders, and then running the ```run.r``` script. 

_Be sure to follow the instructions in the ```run.r``` script (i.e., when to run the Jupyter notebooks)._ 

The sections below describe the requirements necessary for executing the analysis and the datasets used. For more information, refer to ```report.pdf```. The diagram below illustrates the structre of the repository. 

```
surf24                                # Project folder
├── data                              #   Read-only (input) data
├── processed                         #   Intermediate data
├── figures                           #   Output files (png figures)
├── scripts                           #   Code
|   ├── 00-get_avg_temps.R            #      Process LA county-level data
|   ├── ...
|   ├── 06-max_temp_diurnal_rng_bins.R
|   ├── 07-clean_zipcode_data.R       #      Collapse measures to the zipcode level     
|   ├── ...
|   ├── 09-collapse_measures.R
|   ├── 10-er_health_data.ipynb       #      Process mortality, morbidity health data
|   ├── ...
|   ├── 13-catchment_mortality.R
|   ├── 14-zipcode_pca_data.ipynb     #      Prepare data for PCA analysis
|   ├── 15-zipcode_pca.ipynb          #      Perform PCA analysis
|   ├── 16-merge_data.ipynb           #      Compile data into panel data for regression
|   ├── 17-regression_funcs.R         #      Run regression, get figures
|   ├── ...
|   ├── 19-mortality_regression.R
|   ├── process_data.py               #      Class to extract data from CSVs for PCA
|   └── run.R                         #      Master script
└── report.pdf                        #      Final SURF paper              
```

## Requirements

Python version 3.10.5 or higher, with pandas, geopandas, numpy, matplotlib, and scikit-learn libraries installed. 

R version 4.4.1 or higher, with raster, terra, tidyverse, and lfe packages installed. 

## Data

#### Temperature data 

Daily minimum and maximum near-surface air temperatures were gathered from [Zhang et al.’s](https://essd.copernicus.org/articles/14/5637/2022/essd-14-5637-2022-discussion.html) global gridded dataset, offering 1 kilometer spatial resolution from 2003 to 2020 using ground station measurements and remotely-sensed data. LA-specific data can be found at ```data\la_atmax``` and ```data\la_atmin```. 

Daily one-degree temperature bins for each year from 2003 to 2020 were collected for minimum, maximum, average, and diurnal range temperatures, as well as for a maximum-diurnal range temperature interaction. The number of heatwave days for each year was also recorded, using the [EPA’s definition of heatwaves](https://www.epa.gov/sites/default/files/2021-04/documents/heat-waves_td.pdf). The heatwave threshold for LA county was aggregated from pixel-level thresholds derived by Druckenmiller et al. in ```data\summer_85p.tif```. Each of these measures were then aggregated from the pixel level to the zipcode level. 

#### Socio-dempographic data 

Socio-demographic data was extracted from the American Community Survey's (ACS) 2022 five-year estimates. Variables were selected after an extensive literature review conducted by [Shreevastava et al.](https://agu.confex.com/agu/fm23/meetingapp.cgi/Paper/1415540), and include counts encompassing living conditions, social isolation, age, race, health, education, and transportation barriers, all of which have been found to impact responses to extreme temperatures. The table below indicates the ACS tables used to curate a zipcode-level socio-demographic dataset, where the description column explains the purpose of the columns extracted from each ACS table. As seen in ```data\pca```, each table CSV file except for the B25040 table additionally corresponds to two additional files, one that describes the column metadata and one that describes the data collection. 

| Table ID | Description                                                                                                                                                                                          |
|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| B25040   | Population using solar energy for house heating fuel.                                                                                                                                                |
| DP02     | Foreign born population                                                                                                                                                                              |
| DP03     | Unemployed population and population of outdoor workers  (workers in natural resource, construction, maintenance,  production, transportation, and material moving occupations)                      |
| DO05     | Total zipcode, race (white, Black, Asian, Native American, and Hispanic or Latino), elderly (those aged 65 and older), and children (those under 18) populations                                     |
| S1101    | Renting, live alone, and both live alone and elderly (those aged 65 and older) populations                                                                                                           |
| S1501    | Less than high school education and maximum high school education populations                                                                                                                        |
| S1701    | Population living in extreme population, or those whose income is below  50% of their poverty level.                                                                                                 |
| S2501    | Population living with many occupants and population living in overcrowded conditions, defined by the California Department of Housing and Community Development as more than 1.01 people per room.  |

#### Health data 

We curated a health dataset of hospital emergency department (ED) visits (morbidity) and death profiles (mortality) at the zipcode level, obtaining data for both variables from the California Department of Public Health (CDPH). 

The annual morbidity records were extracted from the CDPH's [Hospital Emergency Department - Characteristics by Facility](https://data.chhs.ca.gov/dataset/hospital-emergency-department-characteristics-by-facility-pivot-profile), from which hospital zipcodes, age groups, number of visits, and principal diagnosis groups from 2005 to 2020 were pulled for the final dataset. 

The mortality records were extracted from the CDPH's [Death Profiles by ZIP Code](https://data.chhs.ca.gov/dataset/death-profiles-by-zip-code) dataset, from which all-cause, all-age counts; all-cause, age-specific counts; and all-age, cause-specific counts from 2005 to 2020 were pulled for the final dataset.

For both moribidy and mortality records, missing zipcode rates were imputed using a catchment grouping approach, with each catchment group consisting of a represented zipcode and a group of non-represented zipcodes that are closest to the represented zipcode by distance between their centroids. Each zipcode in that group was assigned a new, shared rate determined by the sum of populations of all zipcodes in the group. Finally, all counts were standardized per 100,000 population using ACS zipcode population data.  


## Acknowledgments 
I'd like to thank my mentor Prof. Druckenmiller for providing support and guidance throughout the project, Dr. Shreevastava for providing access to resources, papers, and data, and Diego Ramos Aguilera for providing guidance for the PCA portion, which borrows code heavily from Aguilera. Finally, thank you to Dr. and Mrs. Harris for sponsoring this project. 

