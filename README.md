# SDM_bird_observer
Script for Assessing species distribution models performance between characterizing habitat on species and observer locations.
## Description
This repository contains code that performs the analyses in the paper. This paper assesses the species distribution model performance across habitat characterization on bird and observer locations via three radii sizes. All scripts are organized in associated folders. For example, scripts working on Landsat are stored in **GoogleEarthEngine**. 
## Content
1. **Google Earth Engine**
   - Scripts to produce Landsat yearly composite + extract median Landsat value of each observer and bird location
2. **R_scripts**
   - Scripts to filter eBird data + fit boosted regression tree models + model evaluation
3. **Shell_script**
   - Scripts to run R script through high-performance computing system
## Contributors
* [Fang-Yu (Betty) Shen](https://bettyshen.org/)
* Fiona Victoria
