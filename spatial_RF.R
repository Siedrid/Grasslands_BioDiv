# Predict species richness across spatial raster

library(terra)
library(raster)
library(sf)
# stack all raster from S2_max_composites folder and name like columns in RF dataframe
hd <- "E"
comp_path <- paste0(hd ,":/Grasslands_BioDiv/Data/S2_max_composites/")
fls <- list.files(comp_path)
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")

setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data"))
BT_center_coords <- get_center_coords("SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx")

# define extent of prediction map!
# create an extent file from study region
xmin <- min(BT_center_coords$X - 500, na.rm = T)
xmax <- max(BT_center_coords$X + 500, na.rm = T)
ymax <- max(BT_center_coords$Y + 500, na.rm = T)
ymin <- min(BT_center_coords$Y - 500, na.rm = T)

bb <- st_bbox(c(xmin, xmax, ymin, ymax))
ext <- raster::extent(xmin, xmax, ymin, ymax)


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
grass.mask.path <- paste0(hd, ":/Grasslands_BioDiv/Data/Copernicus_Grassland/GRA_2018_010m_03035_V1_0.tif")
grass.mask <- rast(grass.mask.path)
s2_pred.terra <- rast(s2_pred)

mask <- (grass.mask != 1)
mask.proj <- project(mask, "epsg:32632")

mask.res <- resample(mask.proj, s2_pred)
mask.crp <- crop(mask.res, s2_pred)
s2_pred.mask <- terra::mask(s2_pred, mask.crp, maskvalue = 1)
plot(s2_pred.mask)
writeRaster(s2_pred.mask , "E:/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction_month90_masked.grd")

mask.grasslands <- function(s2_pred.rast, grass.mask){
  mask <- (grass.mask != 1)
  mask.proj <- project(mask, "epsg:32632")
  
  mask.res <- resample(mask.proj, s2_pred.rast)
  mask.crp <- crop(mask.res, s2_pred.rast)
  s2_pred.mask <- terra::mask(s2_pred.rast, mask.crp, maskvalue = 1)
  return(s2_pred.mask)
}
# Prediction on Minimum Raster Composites

comp_path <- paste0(hd, ":/Grasslands_BioDiv/Data/S2_min_composites/")

indx <- "specn"
hd <- "E"
plt.path <- paste0(hd, ":/Grasslands_BioDiv/Out/Graphs/finalPlots_präsi/")

data_frame <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Reflectance_2022-23_monthly_pivot.csv"))
div_df <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices.csv"))
data_frame <- data_frame[-c(1)]
div_df <- div_df[-c(1)]
s <- 91
csv.path <- paste0(hd, ":/Grasslands_BioDiv/Out/RF_Results/RF_results-v1.csv") # path to write RF results
rf.results <- read.csv(csv.path)
data_frame.nowinter <- RF_predictors(data_frame, c("03$", "04$", "05$", "06$","2022.07$", "08$", "2022.09$"))

rf_data <- preprocess_rf_data(data_frame.nowinter, div_df, "specn")
train_index <- get_train_index(rf_data, s)
forest <- RF(rf_data, train_index, s)
print(forest)
png("E:/Grasslands_BioDiv/Out/RF_Results/nowinter_RF_spatial.png")
summarize.RF(forest, rf_data, div_df,train_index, "specn")
dev.off()
write.RF("nowinter (March to September)", "specn", forest, s, csv.path)
png("E:/Grasslands_BioDiv/Out/RF_Results/nowinter_varimp_spatial.png")
plot.varimp(forest)
dev.off()

# prepare raster stack / brick
# which months to use as predictors
max_comp <- list()
pred <- c()
setwd(comp_path)

# monthly max composite raster with na values less than 10 percent
fls <- list.files(comp_path)
m <- c("03.tif", "04.tif", "05.tif", "06.tif","2022_07.tif", "08.tif", "2022_09.tif")
fls <- get_monthly_composite(comp_path, m)

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
s2_pred <- predict(max_comp.brick, model = forest, na.rm = T)
writeRaster(s2_pred, "E:/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction_minimum_nowinter.grd")

grass.mask.path <- paste0(hd, ":/Grasslands_BioDiv/Data/Copernicus_Grassland/GRA_2018_010m_03035_V1_0.tif")
grass.mask <- rast(grass.mask.path)
s2.masked <- mask.grasslands(rast(s2_pred), grass.mask)
plot(s2.masked)
writeRaster(s2.masked, "E:/Grasslands_BioDiv/Data/SpatialRF_Data/S2_spec_prediction_minimum_nowinter_masked.grd")
