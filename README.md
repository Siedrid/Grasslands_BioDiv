This project's goal is to develop a workflow to derive species richness data from Sentinel-2 Surface Reflectance Data. To this end we used MAJA preprocessed S2 Data and species richness data from field campaigns in Lower Franconia, Germany.
We trained a Random Forest with monthly S2 composites from 2022 and 2023 and compared the prediction for different alpha-diversity parameters: Species Richness, Shannon and Simpson-Index:

![image](https://github.com/Siedrid/Grasslands_BioDiv/assets/137882767/7ab73f63-0a06-4498-88ea-53337e3f7a89)
[!image](https://github.com/Siedrid/Grasslands_BioDiv/blob/master/Graphs/RF-vgl-indices_v1.png?raw=true)

To enhance the prediction we also included data on mowing frequency and DOY of first cut.
