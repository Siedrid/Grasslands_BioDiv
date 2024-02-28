# Mahd Layer

library(raster)
library(terra)
library(sf)
library(readxl)

# get plot center coords
hd <- "F"
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data"))

# function to get either center coords of BT or RB/FE
get_center_coords <- function(xlsx_path, location = "BT"){
  RB_plot_names <- c("Rb_2", "Fe_3", "Fe_4_alternative", "Fe_1_alternative", "Fe_2_alternative", "RB1", "RB2", "FE1", "FE2", "FE3", "RB1_A", "RB2_A",
                     "FE1_A", "FE2_A", "FE3_A", "FE4_A")
  df <- read_excel(path = xlsx_path)
  df_coords <- df[df$Quadrant == 'A' & df$Depth != "2-7", ]
  center_coords <- df_coords[c('Plot', 'PlotCenter_x_coord', 'PlotCenter_y_coord')]
  colnames(center_coords) <- c('plot_names', 'X', 'Y')
  
  if (location == "BT"){
    center_coords <- center_coords[!center_coords$plot_names %in% RB_plot_names,]
  }else{
    center_coords <- center_coords[center_coords$plot_names %in% RB_plot_names,]
  }
  
  center_coords$X <- sub(',', '.', center_coords$X) %>%  as.numeric()
  center_coords$X <- as.numeric(center_coords$X)
  center_coords$Y <- as.numeric(center_coords$Y)
  
  return(center_coords)
}


BT_center_coords <- get_center_coords("SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx")

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
as.numeric(df_mowfreq)

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
