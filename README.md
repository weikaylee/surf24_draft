# Estimating the Human Health Effects of Urban Heat in LA County

2024 Summer Undergraduate Research Fellowship project 

Mentor: Hannah Druckenmiller

## Abstract

Since the Industrial Revolution, Earth’s global climate has warmed to unprecedented levels, driven largely and unequivocally by anthropogenic activities. This warming has led to wide-ranging consequences, including increased extreme weather events and poorer public health. Extreme heat in particular has become a key concern, due to its large role in public health as well as its prevalence in urban areas due to the urban heat island effect, in which the built environment elevates local temperatures. However, while the temperature increases associated with urban heat islands are comparable to those projected by end-of-century climate change scenarios, the impacts of urban heat have received relatively less attention from the public and policymakers. We aim to address this gap by estimating the human health effects of urban heat on mortality and morbidity across LA county at the zipcode level. We gather social vulnerability scores using socio-demographic data and incorporate measures of prolonged heat exposure derived from high-resolution, near-surface air temperature data into fixed effects and polynomial regression models. Our findings are generally consistent with past work in the literature and reveal that our measures and models may be applicable for informing mitigation measures at the local level. 

## Repository overview 
This repository contains the data, code, and results for estimating the health effects of urban heat in LA county. To replicate the analysis, start by cloning this repository, removing the processed and results folders, and then running the '''run.r''' script. The sections below describe the requirements necessary for executing the analysis and the datasets used. The diagram below illustrates the struuctre of the repository. 

surf24                                # Project folder
├── data                              #   Read-only (input) data
├── processed                         #   Intermediate data
├── results                           #   Output files (png figures)
├── scripts                           #   Code
|   ├── 1_import_data.do              #     TODO update
|   ├── 2_clean_data.do
|   ├── 3_combine_data.do
|   ├── 4_analysis.do
|   ├── 5_supporting_analysis.do
|   └── 6_tables.do
└── run.r                             #   Master script

## Requirements

Python version 3.10.5 or higher, with pandas, geopandas, numpy, matplotlib, and scikit-learn libraries installed. 

R version 4.4.1 or higher, with raster, terra, tidyverse, and lfe packages installed. 

## Data

#### Temperature data 

Daily minimum and maximum near-surface air temperatures were gathered from [Zhang et al.’s] (https://essd.copernicus.org/articles/14/5637/2022/essd-14-5637-2022-discussion.html) global gridded dataset, offering 1 kilometer spatial resolution from 2003 to 2020 using ground station measurements and remotely-sensed data. LA-specific data can be found at '''data\la_atmax''' and '''data\la_atmin'''. 

Daily one-degree temperature bins for each year from 2003 to 2020 were collected for minimum, maximum, average, and diurnal range temperatures, as well as for a maximum-diurnal range temperature interaction. The number of heatwave days for each year was also recorded, using the [EPA’s] (https://www.epa.gov/sites/default/files/2021-04/documents/heat-waves_td.pdf) definition of heatwaves. The heatwave threshold for LA county was aggregated from pixel-level thresholds derived by Druckenmiller et al. in '''data\summer_85p.tif'''. Each of these measures were then aggregated from the pixel level to the zipcode level. 

#### Socio-dempographic data 

We then curated a dataset of zipcode-level socio-demographic variables using the American Community Survey’s (ACS) 2022 five-year estimates to compute social vulnerability scores. These variables includes counts encompassing living conditions, social isolation, age, race, health, education, and transportation barriers, all of which have been found to impact responses to extreme temperatures. Additional information is reported in '''data\pca\README.md'''.

TODO move the other readme stuff here?

### Health data 

Finally, we curated a health dataset of hospital emergency department (ED) visits (morbidity) and death profiles (mortality) at the zipcode level, obtaining data for both variables from the California Department of Public Health (CDPH). 

The annual morbidity records were extracted from the CDPH's [Hospital Emergency Department - Characteristics by Facility] (https://data.chhs.ca.gov/dataset/hospital-emergency-department-characteristics-by-facility-pivot-profile), from which hospital zipcodes, age groups, number of visits, and principal diagnosis groups from 2005 to 2020 were pulled for the final dataset. 

The mortality records were extracted from the CDPH's [Death Profiles by ZIP Code] (https://data.chhs.ca.gov/dataset/death-profiles-by-zip-code) dataset, from which all-cause, all-age counts; all-cause, age-specific counts; and all-age, cause-specific counts from 2005 to 2020 were pulled for the final dataset.

For both moribidy and mortality records, missing zipcode rates were imputed using a catchment grouping approach, with each catchment group consisting of a represented zipcode and a group of non-represented zipcodes that are closest to the represented zipcode by distance between their centroids. Each zipcode in that group was assigned a new, shared rate determined by the sum of populations of all zipcodes in the group. Finally, all counts were standardized per 100,000 population using ACS zipcode population data.  


## Acknowledgments 
I'd like to thank my mentor Prof. Druckenmiller for providing support and guidance throughout the project, Dr. Shreevastava for providing access to resources, papers, and data, and Diego Ramos Aguilera for providing guidance for the PCA portion, which borrows code heavily from Aguilera. Finally, thank you to Dr. and Mrs. Harris for generously support this project in honor of Dr. James J. Morgan. 

