# Mahd Layer

library(raster)
library(terra)
library(sf)
library(readxl)

# get plot center coords
hd <- "G"
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data"))

BT_center_coords <- get_center_coords("SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx")
RB_center_coords <- get_center_coords("SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx", location = "RB")

# Load cut rasters
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Laura_SchnitteRMWM/"))

get_first_cut <- function(param, location, year, center_coords){
  if (year == 2023){
    rast_name <- paste0("CutDet_S2_V2_", param, "_", year, "03-", year, "08_", location, ".tif")
    
  }else{
    rast_name <- paste0("CutDet_S2_V2_", param, "_", year, "03-", year, "11_", location, ".tif")
  }
  rst <- rast(rast_name)
  param_df <- terra::extract(rst, center_coords[,2:3])
  param_df$ID <- center_coords$plot_names
  colnames(param_df) <- c("plot_names", paste0(param, "_", year))
  
  return(param_df)
}

# Mowing Frequency BT areas
for (yr in seq(2018, 2023, 1)){
  if (yr == 2018){
    df_mowfreq <- get_first_cut("MowFreq", "UPA", yr, BT_center_coords)
  }else{
    
    df1 <- get_first_cut("MowFreq", "UPA", yr, BT_center_coords)
    df_mowfreq <- merge(df1, df_mowfreq, by = "plot_names")
  }
}

# First Cut in BT areas
for (yr in seq(2018, 2023, 1)){
  if (yr == 2018){
    df_firstcut <- get_first_cut("FirstCut", "UPA", yr, BT_center_coords)
  }else{
    
    df1 <- get_first_cut("FirstCut", "UPA", yr, BT_center_coords)
    df_firstcut <- merge(df1, df_firstcut, by = "plot_names")
  }
}

df_firstcut[,2:6] <- lapply(df_firstcut[,2:6], as.numeric)
median_firstcut <- apply(df_firstcut[,2:6], 1, median, na.rm = T)

df_medfirstcut <- data.frame(plot_names = df_firstcut$plot_names,
                             medfirst_cut = median_firstcut)

# sum up mowfrequencies 2018 to 2023
rsum <- rowSums(df_mowfreq[,2:6], na.rm = T)
rsum.df <- data.frame(plot_names = df_mowfreq$plot_names, mowfreq = rsum)
