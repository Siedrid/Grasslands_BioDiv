# Predict species richness across spatial raster

library(terra)
library(raster)
# stack all raster from S2_max_composites folder and name like columns in RF dataframe
hd <- "G"
comp_path <- paste0(hd ,":/Grasslands_BioDiv/Data/S2_max_composites/")
fls <- list.files(comp_path)
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")

max_comp <- list()
setwd(comp_path)
for (i in 1:length(fls)){
  date.str <- str_split(fls[i],'-')[[1]][3] %>% str_split(., '.tif')
  date.str <- date.str[[1]][1] %>% gsub("_", "-", .)
  rst <- terra::rast(fls[i])
  names(rst) <- paste0(bands, "_", date.str)
  max_comp[[i]] <- rst
}

max_comp.stack.terra <- terra::rast(max_comp)
max_comp.brick <- brick(max_comp.stack.terra) # convert terra SpatRaster to raster's brick object
max.brick <- brick(paste0(hd, ":/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp.tif"))
names(max.brick)

layer_idx <- which(names(max.brick) %in% colnames(data_frame))
max_comp.brick.flt <- max.brick[[layer_idx]]

gc()
s2_pred <- predict(max_comp.brick.flt, model = forest, na.rm = T)
writeRaster(s2_pred, "E:/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction.grd")

# write raster
writeRaster(max_comp.stack, "E:/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp.tif")
writeRaster(max_comp.stack, "E:/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp.grd")

max_comp.stack <- raster::raster("E:/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp.tif")
max_comp.stack <- terra::rast("E:/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp.tif")

s2_pred_rast <- terra::predict(max_comp.stack.terra, model = forest)
names(max_comp.stack) <- rast_names

s2_prediction <- terra::predict(max_comp.stack, model = forest, na.rm = T)

# select months as predictors 
m <- c("03.tif", "04.tif", "05.tif", "06.tif","07.tif", "08.tif", "09.tif")
fls <- get_monthly_composite(comp_path, m)

max.brick <- stack_S2_months(fls, comp_path, m, "G:/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp-nowinter2.tif")
df <- RF_predictors(data_frame, names(max.brick))

s <- 10
rf_data <- preprocess_rf_data(df, div_df, "specn")
train_index <- get_train_index(rf_data, s)
forest <- RF(rf_data, train_index, s)
print(forest)

s2_pred <- predict(max.brick, model = forest, na.rm = T)

# which months to use as predictors
max_comp <- list()
pred <- c()
setwd(comp_path)

# monthly max composite raster with na values less than 10 percent
for (i in 1:length(fls)){
  date.str <- str_split(fls[i],'-')[[1]][3] %>% str_split(., '.tif')
  date.str <- date.str[[1]][1] %>% gsub("_", "-", .)
  rst <- terra::rast(fls[i])
  if (percentage.na(rst) < 10){
    names(rst) <- paste0(bands, "_", date.str)
    max_comp[[i]] <- rst
    pred <- append(pred, date.str)
  }
}

pred <- gsub("-", ".", pred)
pred <- append(pred, c("2022.05", "2023.04", "2023.07"))
data_frame <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Reflectance_2022-23_monthly_pivot.csv"))
data_frame.90 <- RF_predictors(data_frame, pred)

s <- 10
rf_data <- preprocess_rf_data(data_frame.90, div_df, "specn")
train_index <- get_train_index(rf_data, s)
forest <- RF(rf_data, train_index, s)
print(forest)
write.RF("months with more than 90 % not nan, plus may, april, july", "specn", forest, s, csv.path)
plot.varimp(forest, version = 3)  

max_comp.stack.terra <- terra::rast(max_comp)
max_comp.brick <- brick(max_comp.stack.terra) # convert terra SpatRaster to raster's brick object
#max.brick <- brick("E:/Grasslands_BioDiv/Data/SpatialRF_Data/Monthly_Maximum_comp.tif")

names(max_comp.brick)
s2_pred <- predict(max_comp.brick, model = forest, na.rm = T)
writeRaster(s2_pred, paste0(hd, ":/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction_month90.grd"))

# mask with Copernicus Grasslands Layer
s2_pred <- rast("E:/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction_month90.grd")
grass.mask <- paste0(hd, ":/Grasslands_BioDiv/Data/Copernicus_Grassland/GRA_2018_010m_03035_V1_0.tif")
grass.mask <- rast(grass.mask)
s2_pred.terra <- rast(s2_pred)

mask <- (grass.mask != 1)
mask.proj <- project(mask, "epsg:32632")

mask.res <- resample(mask.proj, s2_pred)
mask.crp <- crop(mask.res, s2_pred)
s2_pred.mask <- terra::mask(s2_pred, mask.crp, maskvalue = 1)
plot(s2_pred.mask)
writeRaster(s2_pred.mask , "E:/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction_month90_masked.grd")
