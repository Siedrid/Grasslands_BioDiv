# Predict species richness on a Raster Stack

# create Raster Stack


# load acquisitions from one month
setwd("E:/Grasslands_BioDiv/Data/Field_Data")
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
satpath <- "E:/Grasslands_BioDiv/Data/SatData/"
wb_path <- "E:/Grasslands_BioDiv/Data/S2_Reflectances/reflectances_plot_center-v1.xlsx"
y_str = "2022"
m = 12

comp_path <- "E:/Grasslands_BioDiv/Data/S2_max_composites/"
maximize_fct <- function(x){
  max(x,na.rm = T)
}

study_area <- get_study_area()
for (y_str in c("2022", "2023")){
  for (m in c(1:12)){
    m_str <- sprintf("%02d", m)
    if (file.exists(paste0(comp_path,"S2-MaxComp-", y_str, "_", m_str, ".tif"))){
      message(paste0("File ","S2-MaxComp-", y_str, "_", m_str, ".tif", " already exists"))
    }else{
      acquisitions <- get_acquisitions(y_str, m_str, satpath)
      b = 1
      band_lst <- list()
      for (band in bands){
        message(paste("Processing band", band))
        band = "B2"
        monthly_band_stack <- list()
        for (i in 1:length(acquisitions)){
          acq <- acquisitions[i]
          mask_path <- get_masks(acq)
          date <- as.Date(str_split(str_split(acq, '_')[[1]][3], '-')[[1]][1], "%Y%m%d")
          message(paste("Processing date", date))
          
          if (band %in% c("B2", "B3", "B4", "B8")){
            m <- load_mask(10, mask_path)
            b2 <- load_band(band, acq, m)
            bd <- b2
          }else{
            m <- load_mask(20, mask_path)
            b11 <- load_band(band, acq, m)
            b.resampled <- resample(b11, b2, method = 'bilinear')
            bd <- b.resampled
          }
          monthly_band_stack[[i]] <- bd
        }
        # maximize stack
        message("Calculate Maximum Band Composite")
        max.bd.st <- rast(monthly_band_stack) %>% maximize_fct()
        # add to list of maximum stacks
        band_lst[[b]] <- max.bd.st
        
        b <- b+1
        gc()
      }
      # write to hard drive
      message("Write...")
      monthly_stack <- rast(band_lst)
      names(monthly_stack) <- bands
      writeRaster(monthly_stack, paste0(comp_path,"S2-MaxComp-", y_str, "_", m_str, ".tif"), overwrite = F)
      gc()
    }
  }
}
        
