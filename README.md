# Overview
This project's goal is to develop a workflow to derive species richness data from Sentinel-2 Surface Reflectance Data. To this end we used MAJA preprocessed S2 Data and species richness data from field campaigns in Lower Franconia, Germany.
We trained a Random Forest with monthly S2 composites from 2022 and 2023 and compared the prediction for different alpha-diversity parameters: Species Richness, Shannon and Simpson-Index:

![image](https://github.com/Siedrid/Grasslands_BioDiv/assets/137882767/7ab73f63-0a06-4498-88ea-53337e3f7a89)
![image](https://github.com/Siedrid/Grasslands_BioDiv/blob/master/Graphs/RF-vgl-indices_v1.png?raw=true)

## Random Forest Predictor Importance
March and April appear to be the most important months for the prediction. The most important bands are B11 and B12 as well as B4 and B5.
![image](https://github.com/Siedrid/Grasslands_BioDiv/assets/137882767/ee1f88bd-35a4-4484-b784-f7a27d4c4fe8)

To enhance the prediction we also included data on mowing frequency and DOY of first cut.

## Reflectance Trend after Cut

We need to understand better how mowing affects the reflectance, as this will affect the monthly compositing. In some bands, reflectance decreases, whereas in other bands reflectance increases:
![image](https://github.com/Siedrid/Grasslands_BioDiv/blob/master/Graphs/TrendperBandafterCut.png)
