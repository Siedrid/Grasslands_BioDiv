# Predict species richness on a Raster Stack

# create Raster Stack

hd <- "E"
# load acquisitions from one month
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data"))
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
satpath <- paste0(hd, ":/Grasslands_BioDiv/Data/SatData/")
wb_path <- paste0(hd, ":/Grasslands_BioDiv/Data/S2_Reflectances/reflectances_plot_center-v1.xlsx")
y_str = "2022"
m = 7

comp_path <- paste0(hd, ":/Grasslands_BioDiv/Data/S2_min_composites/")

comp_fct <- function(x, comp.method){
  if (comp.method == "max"){
    comp = max(x,na.rm = T)
  }
  if (comp.method == "min"){
    comp = min(x, na.rm = T)
  }
  return(comp)
}
y_str = "2022"
m_str = "09"
study_area <- get_study_area()
for (y_str in c("2022", "2023")){
  for (m in c(3:9)){
    m_str <- sprintf("%02d", m)
    if (file.exists(paste0(comp_path,"S2-MaxComp-", y_str, "_", m_str, ".tif"))){
      message(paste0("File ","S2-MaxComp-", y_str, "_", m_str, ".tif", " already exists"))
    }else{
      acquisitions <- get_acquisitions(y_str, m_str, satpath)
      b = 1
      band_lst <- list()
      for (band in bands){
        message(paste("Processing band", band))
        monthly_band_stack <- list()
        for (i in 1:length(acquisitions)){
          acq <- acquisitions[i]
          mask_path <- get_masks(acq)
          date <- as.Date(str_split(str_split(acq, '_')[[1]][3], '-')[[1]][1], "%Y%m%d")
          message(paste("Processing date", date))
          
          if (band %in% c("B2", "B3", "B4", "B8")){
            m <- load_mask(10, mask_path)
            b2 <- load_band(band, acq, m)
            b2.crp <- crop(b2, ext)
            bd <- b2.crp
          }else{
            m <- load_mask(20, mask_path)
            b11 <- load_band(band, acq, m)
            b11.crp <- crop(b11, ext)
            b.resampled <- resample(b11.crp, b2.crp, method = 'bilinear')
            bd <- b.resampled
          }
          monthly_band_stack[[i]] <- bd
        }
        # maximize stack
        message("Calculate Maximum Band Composite")
        max.bd.st <- rast(monthly_band_stack) %>% comp_fct(., "min")
        # add to list of maximum stacks
        band_lst[[b]] <- max.bd.st
        
        b <- b+1
        gc()
      }
      # write to hard drive
      message("Write...")
      monthly_stack <- rast(band_lst)
      names(monthly_stack) <- bands
      writeRaster(monthly_stack, paste0(comp_path,"S2-MinComp-", y_str, "_", m_str, ".tif"), overwrite = F)
      gc()
    }
  }
}
        
