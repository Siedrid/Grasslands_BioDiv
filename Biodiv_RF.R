# Random Forest 

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
library(tidyr)


indx <- "specn"
data_frame <- read.csv("E:/Grasslands_BioDiv/Data/Field_Data/Reflectance_2022-23_monthly_pivot.csv")
div_df <- read.csv("E:/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices.csv")
data_frame <- data_frame[-c(1)]
div_df <- div_df[-c(1)]
s <- 10
csv.path <- "E:/Grasslands_BioDiv/Out/RF_Results/RF_results-v1.csv" # path to write RF results
rf.results <- read.csv(csv.path)

for (s in c(1:5)){
  # keep only spring and summer months
  
  # spring
  pred = "spring"
  m.spring <- c("03$", "04$", "05$", "06$")
  
  data_frame.spring <- RF_predictors(data_frame, m.spring)
  rf_data <- preprocess_rf_data(data_frame.spring, div_df, indx)
  train_index <- get_train_index(rf_data, s)
  forest <- RF(rf_data, train_index, s)
  print(forest)
  
  summarize.RF(forest, div_df, train_index, "specn")
  RFImp <- varImp(forest, scale = F)
  plot(RFImp, top = 20)
  
  write.RF(pred, indx, forest, s, csv.path)
  
  # summer
  pred = "summer"
  m.summer <- c("07$", "08$", "09$", "10$")
  
  data_frame.summer <- RF_predictors(data_frame, m.summer)
  rf_data <- preprocess_rf_data(data_frame.summer, div_df, indx)
  train_index.sum <- get_train_index(rf_data.sum, s)
  forest.sum <- RF(rf_data.sum, train_index.sum, s)
  print(forest.sum)
  
  summarize.RF(forest.sum, div_df, train_index.sum, "specn")
  
  # Variable Importance
  RFImp <- varImp(forest.sum, scale = F)
  plot(RFImp, top = 20)
  
  write.RF(pred, indx, forest.sum, s, csv.path)
}

# RF no winter predictors ----
s <- 7
data_frame.nowinter <- RF_predictors(data_frame, c("03$", "04$", "05$", "06$","07$", "08$", "09$", "10$"))
rf_data <- preprocess_rf_data(data_frame.nowinter, div_df, "specn")
train_index <- get_train_index(rf_data, s)
forest <- RF(rf_data, train_index, s)
print(forest)
summarize.RF(forest, div_df,train_index, "specn")
write.RF("nowinter", "specn", forest, s, csv.path)
plot.varimp(forest, write = F, 1)
 
# RF Ammer ----

# only April as predictor
s <- 5
data_frame.april <- RF_predictors(data_frame, "2022.04$")
colnames(data_frame.april) <- gsub("2022", "2019", colnames(data_frame.april))
rf_data <- preprocess_rf_data(data_frame.april, div_df, "shannon")
train_index <- get_train_index(rf_data, s)
forest.april <- RF(rf_data, train_index, s)
print(forest.april)
summarize.RF(forest.april, div_df, train_index, "shannon")
write.RF("2022.04", "shannon", forest.april, s, csv.path)

# predict for Ammer Data
ammer.prediction <- predict(forest.april, newdata = data.frame(max_df_piv)) %>% data.frame()
ammer.prediction$plot_names <- max_df_piv$plot_names
merge(ammer.prediction, ammer.divdf, by = "plot_names")

RFImp$importance # data frame

png("E:/Grasslands_BioDiv/Out/Graphs/RF/Variable_Importance.png")
plot(RFImp, top = 20)
dev.off()

# write RF results to csv
csv.path <- "E:/Grasslands_BioDiv/Out/RF_Results/RF_results-v1.csv"
write.RF("spring", forest, s, csv.path)

biodiv_indx <- c("specn", "shannon", "simpson")

for (indx in biodiv_indx){
  rf_data <- preprocess_rf_data(data_frame, div_df, indx)
  train_index <- get_train_index(rf_data, s)
  forest <- RF(rf_data, train_index, s)
  print(forest)
  assign(indx, summarize.RF(forest, div_df, train_index, indx))
}
forest
png("E:/Grasslands_BioDiv/Out/Graphs/RF/RF-vgl-indices_v1.png",width = 8, height = 10, units = "in", res = 1200)
ggarrange(specn,
          ggarrange(shannon, simpson, ncol = 2, labels = c("B", "C")),
          nrow=2,
          labels = 'A')
dev.off()

# complete Ammer Data 13.02.----
# only complete months as predictor
s <- 5 
predictors <- gsub("-", ".", m.RF) %>% gsub("2019", "2022", .) %>% gsub("2020", "2023", .)
data_frame.april <- RF_predictors(data_frame, predictors)
colnames(data_frame.april) <- gsub("2022", "2019", colnames(data_frame.april)) %>% gsub("2023", "2020", .)
rf_data <- preprocess_rf_data(data_frame.april, div_df, "shannon")
train_index <- get_train_index(rf_data, s)
forest.april <- RF(rf_data, train_index, s)
print(forest.april)
summarize.RF(forest.april, div_df, train_index, "specn")
write.RF("complete Ammer months", "specn", forest.april, s, csv.path)

# predict for Ammer Data
ammer.divdf <- read.csv("E:/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices_Ammer.csv")
ammer.prediction <- predict(forest.april, newdata = data.frame(max_df_piv)) %>% data.frame()
ammer.prediction$plot_names <- max_df_piv$plot_names
pred.df <- merge(ammer.prediction, pred.df, by = "plot_names")

colnames(pred.df)[2] <- "shannon.prediction"

# plot results
limx <- c(min(pred.df$specn), max(pred.df$specn))
spec <- ggplot(data = pred.df, aes(x = specn, y = specn.prediction))+
  geom_point()+
  geom_abline(slope = 1)+
  xlim(limx)+
  ylim(limx)
  
limx <- c(min(pred.df$shannon), max(pred.df$shannon)+0.5)
shannon <- ggplot(data = pred.df, aes(x = shannon, y = shannon.prediction))+
  geom_point()+
  geom_abline(slope = 1)+
  xlim(limx)+
  ylim(limx)
png("E:/Grasslands_BioDiv/Out/RF_Results/Ammer_RF_all_months.png")
ggarrange(spec, shannon, nrow = 2)
dev.off()

# Franken with Days Since Last Cut ----

df <- read.csv("E:/Grasslands_BioDiv/Data/Field_Data/DaySinceLastCut/SUSALPS_BT_2022-23_Refl_Plus_DaysSince.csv")
df <- df[, 3:ncol(df)]
int.ts <- interpolate.ts(df, plot.column = "plot_names")
int.ts$dat <- as.Date(int.ts$dat, "%Y-%m-%d")
int.ts <- na.omit(int.ts)

last.cut.df <- comp_max_days_since(int.ts, date.column = "dat")
m <- unique(last.cut.df$month)
#df.season <- last.cut.df[last.cut.df$month %in% m[1:16],] # filter months before start of season
df <- last.cut.df %>% select(-dat, -date_DOY)

df.piv <- pivot.df(df, days_since = T)
df.piv <- df.piv[-grep("alternative",df.piv$plot_names),]
df.season <- RF_predictors(df.piv, c("04", "05", "06", "07", "08", "09", "10"))

# April May 2023 fehlen bei vielen sites, durch days since verloren gegangen?

