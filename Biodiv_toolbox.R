# All Functions for Biodiversity Random Forest Implementation

# get Reflectances ----
hd <- "G"
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data"))
library(readxl)
library(openxlsx)
library(vegan)
library(sf)
library(ggplot2)
library(terra)
library(stringr)
library(dplyr)
library(tidyr)
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12") # Sentinel-2 Bands

get_study_area <- function(){
  
  df <- read_excel(path = "SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx")
  df_coords <- df[df$Quadrant == 'A' & df$Depth != "2-7", ]
  center_coords <- df_coords[c('Plot', 'PlotCenter_x_coord', 'PlotCenter_y_coord')]
  colnames(center_coords) <- c('Plot', 'X', 'Y')
  center_coords$X <- as.numeric(center_coords$X)
  center_coords$Y <- as.numeric(center_coords$Y)
  
  # create an extent file from study region
  xmin <- min(center_coords$X, na.rm = T) - 500
  xmax <- max(center_coords$X, na.rm = T) + 500
  ymax <- max(center_coords$Y, na.rm = T) + 500
  ymin <- min(center_coords$Y, na.rm = T) - 500
  
  study_area <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin))
  return(study_area)
}


to_wb <- function(path, date, df){
  wb <- loadWorkbook(path)
  
  if (toString(date) %in% getSheetNames(path)){
    writeData(wb, sheet=date, df)
    saveWorkbook(wb, path, overwrite = T)
  }else{
    addWorksheet(wb, date)
    writeData(wb, sheet=date, df)
    saveWorkbook(wb, path, overwrite = T)
  }
}

crop_band2plots <- function(band, poly_lst){
  rp <- lapply(1:nrow(poly_lst), function(x)
    terra::mask(crop(band, poly_lst[x,]), poly_lst[x,]))
  
  met <- lapply(1:length(rp), function(x)
    median(rp[[x]][,,1], na.rm = TRUE))
  metric.v <- unlist(met)
  return(metric.v)
}

get_acquisitions <- function(y, m, wd){
  month_path <- paste0(wd, y, '/', m) # directory with all aquisitons from one month
  acquisitions <- list.files(month_path, full.names = T)
  return(acquisitions) #paste month path in the front
}

get_masks <- function(acq){
  mask_path <- paste0(acq, '/MASKS/')
  mask_lst <- list.files(mask_path, pattern = '.tif', full.names = T)
  idx <- grep('MG2', mask_lst) # get index of file with MG2 in file name
  
  mask_path <- c(mask_lst[idx[1]],mask_lst[idx[2]]) # M10, M20
  return(mask_path)
}

load_mask <- function(res, mask_path){
  if (res == 10){
    mask10 <- terra::rast(mask_path[1])
    mask_crp <- terra::crop(mask10, study_area) # crop mask to extent
    m <- mask_crp > 1 # all values greater 1 are set to TRUE and masked in the next step
  }
  if (res == 20){
    mask20 <- terra::rast(mask_path[2])
    mask_crp <- terra::crop(mask20, study_area) # crop mask to extent
    m <- mask_crp > 1 # all values greater 1 are set to TRUE and masked in the next step
  }
  return(m)
}

load_band <- function(band, acq, m){
  scal_factor <- 10000
  band_lst <- list.files(acq, full.names = T)
  band_lab <- paste0("FRE_", band, ".tif$")
  band_path <- band_lst[grep(band_lab, band_lst)]
  
  if (band %in% c("B2", "B3", "B4", "B8")){
    rast <- crop(terra::rast(band_path), study_area)/scal_factor
    
    rast_m <- terra::mask(rast, m, maskvalue = TRUE)
    
  }else{
    rast <- crop(terra::rast(band_path), study_area)/scal_factor
    
    rast_m <- mask(rast, m, maskvalue = TRUE)
  }
  return(rast_m)
}

# Preprocessing Predictor and Response Variables ----

# update plot names
plot_names_2022 <- function(spec_df){
  plot_names <- c("Obernschreez_68_Nord", "Obernschreez_68_Süd", "Obernschreez_726_West", "Obernschreez_726_Ost","Obernschreez_734", 
                  "Nördlicher_Deuterweg_1", "Nördlicher_Deuterweg_2","Nördlicher_Deuterweg_3","Nördlicher_Deuterweg_4","Nördlicher_Deuterweg_5","Nördlicher_Deuterweg_6",
                  "Südlich_Deuterweg_1","Südlich_Deuterweg_2","Südlich_Deuterweg_3","Südlich_Deuterweg_4",
                  "Südlich_Deuterweg_5","Südlich_Deuterweg_6","Südlich_Deuterweg_7",
                  "Südlich_Bauhof_1", "Südlich_Bauhof_2","Südlich_Bauhof_3","Südlich_Bauhof_4",
                  "Gubitzmoos_1", "Gubitzmoos_2",
                  "Deuter",
                  "Schobertsberg_1", "Schobertsberg_2","Schobertsberg_3","Schobertsberg_4","Schobertsberg_5")
  spec_df$plot_names <- plot_names
  return(spec_df)
}


get_diversity <- function(spec_path, yr){
  # Function for 2022 and 2023 Data (different formats are considered)
  
  if (yr == 2022){
    spec_2022 <- read.csv(spec_path, header = T, sep = ';')
    spec_2022 <- plot_names_2022(spec_2022)
    
    colnames(spec_2022)[3] <- "Total_cover"
    spec_cols <- grep('\\.', colnames(spec_2022))
    spec_2022[is.na(spec_2022)] <- 0
    
    shannon_div <- diversity(as.matrix(spec_2022[spec_cols]), index = "shannon")
    simpson_div <- diversity(as.matrix(spec_2022[spec_cols]), index = "simpson")
    spec <- specnumber(as.matrix(spec_2022[spec_cols]))
    
    div_df <- data.frame(plot_names = spec_2022$plot_names)
    div_df$shannon <- shannon_div
    div_df$simpson <- simpson_div
    div_df$specn <- spec
  }
  if (yr == 2023){
    plotIDs <- excel_sheets(spec_path)
    plotIDs <- plotIDs[-grep("Tabelle", plotIDs)] # in case there is still an empty table in the excel wb
    
    div_df <- data.frame(plot_names = plotIDs)
    shannon <- c()
    simpson <- c()
    n <- c()
    i = 1
    for (id in plotIDs){
      spec <- read_excel(path = spec_path, sheet = id) # sheet=
      colnames(spec) <- c("Species", "Cover") 
      spec[grep("<1", spec$Cover),2] <- "0.5"
      
      spec$Cover <- as.numeric(spec$Cover)
      spec <- na.omit(spec)
      
      shannon[i] <- diversity(spec$Cover, index = "shannon")
      simpson[i] <- diversity(spec$Cover, index = "simpson")
      n[i] <- length(spec$Cover)
      i <- i + 1
    }
    div_df$shannon <- shannon
    div_df$simpson <- simpson
    div_df$specn <- n
    
    div_df$plot_names[grep("734", div_df$plot_names)] <- "734-1"
  }
  return(div_df)
}


wb_to_df <- function(wb_path){
  
  # load Workbook
  wb <- loadWorkbook(wb_path)
  dates <- as.Date(getSheetNames(wb_path), "%Y-%m-%d")
  sort_dates <- sort(dates)
  
  # initialize array
  df1 <- read.xlsx(wb, sheet = toString(sort_dates[1]))
  df1 <- df1[-2]
  df1$dat <- rep(sort_dates[1], nrow(df1))
  df2 <- read.xlsx(wb, sheet = toString(sort_dates[2]))
  df2 <- df2[-2]
  df2$dat <- rep(sort_dates[2], nrow(df2))
  
  a <- rbind(df1, df2)
  
  # stack Sentinel retrieved reflectances in increasing date order
  for (d in 3:length(sort_dates)){
    df1 <- read.xlsx(wb, sheet=toString(sort_dates[d]))
    df1 <- df1[-2]
    df1$dat <- rep(sort_dates[d], nrow(df1))
    
    a <- rbind(a, df1)
  }
  return(a)
}

interpolate.ts <- function(df, plot.column){
  # rename plot column
  colnames(df)[colnames(df) == plot.column] <- "plot_names"
  int.ts <- df %>% group_by(plot_names) %>% 
    mutate_at(bands, ~ zoo::na.approx(., na.rm = FALSE))
  return(int.ts)
}

comp_max <- function(df, date.column, stat){
  # rename date column
  colnames(df)[colnames(df) == date.column] <- "dat"
  # Calculate maximum reflectance per band, plot and month
  comp_df <- df %>%
    group_by(plot_names, month = format(dat, "%Y.%m")) %>% 
    summarize_if(is.numeric, stat, na.rm = TRUE)  
  return(comp_df)
}

# calculate maximum and minimum composite depending on bands
comp_max.bands <- function(df, date.column, max_bands, min_bands){
  # rename date column
  colnames(df)[colnames(df) == date.column] <- "dat"
  # Calculate maximum and minimum reflectance per band, plot and month
  max_df <- df %>%
    group_by(plot_names, month = format(dat, "%Y.%m")) %>% # punkt statt - 
    summarize(across(all_of(max_bands), \(x) max(x, na.rm = TRUE)),
              across(all_of(min_bands),\(x) min(x, na.rm = TRUE)))  
  return(max_df)
}

# return maximum days since last cut per month
comp_max_days_since <- function(df, date.column){
  # rename date column
  colnames(df)[colnames(df) == date.column] <- "dat"
  df.sub<- subset(df, days_since != 255) %>% subset(., days_since != 254) %>% subset(., !is.na(days_since))
  # Calculate maximum reflectance per band, plot and month
  max_df <- df.sub %>%
    group_by(plot_names, month = format(dat, "%Y-%m")) %>% 
    slice(which.max(days_since))  
  
  # remove masked rows (255 or 254 --> before start of season)
  return(max_df)
}

# Pivot the data, needed for RF
pivot.df <- function(df, days_since_cut = F){
  df <- ungroup(df)
    if (days_since_cut){
      max_df_piv <- df  %>% 
        mutate(variable = month) %>%
        select(-month) %>%
        pivot_wider(names_from = variable, values_from = c(B11,B12,B2,B3,B4,B5,B6,B7,B8, B8A, days_since))
    }else{
      max_df_piv <- df %>%
        mutate(variable = month) %>%
        select(-month) %>%
        pivot_wider(names_from = variable, values_from = c(B11,B12,B2,B3,B4,B5,B6,B7,B8, B8A))
    }
  return(max_df_piv)
}

# Biodiversity RF ----

library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggpubr)
library(readxl)
library(openxlsx)
library(vegan)
library(sf)
library(terra)
library(stringr)

preprocess_rf_data <- function(data_frame, div_df, biodiv_index){
  idx <- which(colnames(div_df) == biodiv_index)
  rf_data <- div_df[c(1,idx)] %>% merge(.,data_frame, by= "plot_names")  
  # Change the name of the biodiv column
  rf_data <- rename(rf_data, biodiv = biodiv_index)
  
  rownames(rf_data) <- rf_data$plot_names
  rf_data <- rf_data[-1]
  
  return(rf_data)
}

get_train_index <- function(rf_data, s){
  set.seed(s)
  train_index <- createDataPartition(y=rf_data$biodiv, p=0.7, list = FALSE)
  return(train_index)
}
RF <- function(rf_data, train_index, s){
  train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
  
  training_set <- rf_data[train_index,]
  
  set.seed(s)
  forest <- train(biodiv~., data = training_set, method = 'rf', trControl = train_control, tuneLength = 7)
  return(forest)
}

summarize.RF <- function(forest, rf_data, div_df, train_index, biodiv_index){
  # Visualization of model prediction
  train.res <- forest$trainingData['.outcome']
  train.res$predicted <- unname(forest$finalModel$predicted)
  train.res$traintest <- rep("train", nrow(train.res))
  # Testing with test set
  testing_set <- rf_data[-train_index,]
  
  test <- predict(object=forest, newdata=testing_set)
  test_df <- merge(div_df[c('plot_names', biodiv_index)], data.frame(plot_names = names(test),predicted = test), by = "plot_names")
  test_df <- rename(test_df, .outcome = biodiv_index)
  test_df$traintest <- rep("test", nrow(test_df))
  test_df <- tibble::column_to_rownames(test_df, var="plot_names")
  test_df <- rbind(train.res, test_df)
  R2 <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$Rsquared,3)
  
  # get x and y range
  limx <- c(min(test_df$.outcome), max(test_df$.outcome))
  
  model <- lm(predicted ~ .outcome, data = test_df)
  r2_test <- summary(model)$r.squared
  # text position
  text.pos <- quantile(test_df$.outcome, 0.1) %>% unname()
  
  lab <- paste("R² =", round(R2,3), "\n", "n =", nrow(train.res), "\n", 
               "R² =", round(r2_test,3), "\n", "n =", length(test))
  rf_plot <- ggplot(data = test_df, aes(x=.outcome, y=predicted, col=traintest, label = rownames(test_df)))+
    geom_point()+
    geom_text(check_overlap = T, nudge_y = 1)+
    geom_abline(slope = 1)+
    xlim(limx)+
    ylim(limx)+
    annotate("text", x= limx[1], y=(limx[2]*(1-0.04)), label = lab, hjust = 0)+
    ggtitle(paste("Random Forest Result:", biodiv_index))
  
  return(rf_plot)
}

RF_predictors <- function(data_frame, m_lst){
  idx.lst <- c()
  for (m in m_lst){
    idx <- grep(m, colnames(data_frame))
    idx.lst <- append(idx.lst, idx)
  }
  data_frame.spring <- data_frame[idx.lst]
  data_frame.spring["plot_names"] <- data_frame$plot_names
  return(data_frame.spring)
}

# get months composite, from folder, specify months to be used in raster prediction
get_monthly_composite <- function(path, ms){
  idx.lst <- c()
  for (m in ms){
    idx <- grep(m, list.files(path))
    idx.lst <- append(idx.lst, idx)
  }
  fls <- list.files(path)[idx.lst]
  return(fls)
}

# stack input rasters to be usable as input for spatial RF
stack_S2_months <- function(fls, path, ms, filename){
  max_comp <- list()
  setwd(path)
  for (i in 1:length(fls)){
    date.str <- str_split(fls[i],'-')[[1]][3] %>% str_split(., '.tif')
    date.str <- date.str[[1]][1] %>% gsub("_", "-", .)
    rst <- terra::rast(fls[i])
    names(rst) <- paste0(bands, "_", date.str)
    max_comp[[i]] <- rst
  }
  max_comp.stack.terra <- terra::rast(max_comp)
  writeRaster(max_comp.stack.terra, filename)
  #max_comp.brick <- brick(max_comp.stack.terra) # convert terra SpatRaster to raster's brick object
  max.brick <- brick(filename)
  
  return(max.brick)
}
# write RF results to csv
write.RF <- function(pred, idx, forest, s, csv.path){
  
  if (!file.exists(csv.path)){ # create new csv if non existent
    df <- data.frame(matrix(ncol = 9))
    colnames(df) <- c("Predictors", "Resp_var", "seed", "R2", "RMSE", "stdev", "var1", "var2", "var3")
    write.csv(df, csv.path, row.names = F)
  }else{
    df <- read.csv(csv.path) # open exisiting csv
  }
  RFImp <- varImp(forest, scale = F)
  imp <- RFImp$importance
  r2 <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$Rsquared,3)
  rmse <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$RMSE,3)
  sd <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$RsquaredSD,3)
  
  vars <- rownames(imp[order(-rowSums(imp)),, drop = F])[1:3] # first three most important variables
  if (is.na(df$Predictors[1])){
    df[1,] <- c(pred, idx, s, r2, rmse, sd, vars)
  }else{
    df[nrow(df)+1,] <- c(pred, idx, s, r2, rmse, sd, vars)
  }
  write.csv(df, csv.path, row.names = F)
  return(df)
}

# plot varimportance from caret package in ggplot
plot.varimp.caret <- function(imp.df){
  
  imp.df$variable <- rownames(imp.df)
  imp.df <- imp.df[order(-imp.df$Overall), ]
  
  top_20 <- imp.df[1:15, ]
  top_20$variable <- factor(top_20$variable, levels = rev(unique(top_20$variable)))
  
  ImpPlot <- ggplot(top_20, aes(x = variable, y = Overall)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "Variable", y = "Importance") +
    coord_flip()
  
  return(ImpPlot)
}
  
# aggregate according to band and month/year 
plot.varimp <- function(forest){
  
  imp.df <- varImp(forest, scale = F)$importance
  
  imp.sep <- imp.df %>% rownames_to_column(var = "band.date") %>% 
    separate(band.date, into = c("Band", "Year", "Month"), sep = "_|\\.")
  
  top20 <- plot.varimp.caret(imp.df)
  
  band.plot <- imp.sep %>% group_by(Band) %>% 
    ggplot(aes(x=Band, y=Overall))+
    geom_bar(stat = "identity", show.legend = F)
  
  year.plot <- imp.sep %>% group_by(Year) %>% 
    ggplot(aes(x=Year, y=Overall))+
    geom_bar(stat = "identity", show.legend = F)
  
  month.plot <- imp.sep %>% group_by(Month) %>% 
    ggplot(aes(x=Month, y=Overall))+
    geom_bar(stat = "identity", show.legend = F)
  
  gg <- ggarrange(band.plot, year.plot,
                  top20, month.plot, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))
  return(gg)
} 

# General functions ----

fix.plotnames <- function(nms){
  fixed_nms <- gsub("Ã¶", "oe", nms)
  fixed_nms <- gsub("ö", "oe",fixed_nms)
  
  fixed_nms <- gsub("Ã¼", "ue", fixed_nms)
  fixed_nms <- gsub("ü", "ue", fixed_nms)
  
  return(fixed_nms)
}

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

# get Date from DOY

doy2date <- function(doy, yr){
  dt <- as.Date(doy, origin = paste0(yr, "-01-01"))
  return(dt)
}
