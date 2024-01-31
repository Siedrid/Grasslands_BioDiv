# All Functions for Biodiversity Random Forest Implementation

# get Reflectances ----

setwd("E:/Grasslands_BioDiv/Data/Field_Data")
library(readxl)
library(openxlsx)
library(vegan)
library(sf)
library(ggplot2)
library(terra)
library(stringr)
library(dplyr)


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

summarize.RF <- function(forest, div_df, train_index, biodiv_index){
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
  rf_plot <- ggplot()+
    geom_point(data = test_df, aes(x=.outcome, y=predicted, col=traintest))+
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


# write RF results to csv
write.RF <- function(pred, idx, forest, s, csv.path){
  
  if (!file.exists(csv.path)){ # create new csv if non existent
    df <- data.frame(matrix(ncol = 9))
    colnames(df) <- c("Predictors", "Resp_var", "seed", "R2", "RMSE", "stdev", "var1", "var2", "var3")
    write.csv(df, csv.path, row.names = F)
  }else{
    df <- read.csv(csv.path) # open exisiting csv
  }
  
  r2 <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$Rsquared,3)
  rmse <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$RMSE,3)
  sd <- round(forest$results[forest$results$mtry == as.numeric(forest$bestTune),]$RsquaredSD,3)
  
  vars <- rownames(RFImp$importance)[1:3] # first three most important variables
  if (is.na(df$Predictors[1])){
    df[1,] <- c(pred, idx, s, r2, rmse, sd, vars)
  }else{
    df[nrow(df)+1,] <- c(pred, idx, s, r2, rmse, sd, vars)
  }
  write.csv(df, csv.path, row.names = F)
  return(df)
}
