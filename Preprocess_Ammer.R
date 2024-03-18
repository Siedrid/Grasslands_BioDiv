# Preprocessing Ammer Data
library(dplyr)
library(tidyr)
library(vegan)
hd <- "G"
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Ammer/"))

ammer.df <- read.csv("SUSALPS-RS_samplingData_plotLevel_bothDepths_2019-2021_20230301_plus_S2-Maja-5days_VI_days-cut_clean_20230717.csv", sep = ';')
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12") # Sentinel-2 Bands

ammer.df.bands <- ammer.df[c("plot", "sampling_date", bands)]
unique(ammer.df.bands$sampling_date)

int.ts <- interpolate.ts(ammer.df.bands, "plot")
int.ts$sampling_date <- as.Date(int.ts$sampling_date, "%Y-%m-%d")

int.ts <- na.omit(int.ts)

# group by month
max_df <- comp_max(int.ts, date.column = "sampling_date")

# use only May reflectances from 2019
df.april <- max_df[max_df$month == "2019-04",]
max_df %>% group_by(month) %>% 
  ggplot(aes(x=month))+
  geom_bar(stat = "count")

# apply scale factor 10000
df.april.scaled <- df.april %>% mutate(across(all_of(bands), ~. /10000))

# Pivot data
max_df_piv <- pivot.df(df.april.scaled)


# species data ----
ammer.div <- readxl::read_excel(path = "SUSALPS-RS_2020_SpeciesInventory_UBT-DE_all sites.xlsx")
ammer.div$plot <- as.factor(ammer.div$plot)
ammer.div <- ammer.div %>% fill(plot, .direction = "down")

plot_names <- c("FE1", "FE2", "FE3", "FE4", "RB1", "RB2", "MW", "GL", "MN", "UF" )

idx <- grep("^(?!.*18)|&", ammer.div$plot, perl = TRUE) # indices to keep
ammer.div19 <- ammer.div[idx,c("plot", "cover %")]
colnames(ammer.div19) <- c("plot_names", "Cover")
ammer.div19$Cover <- as.numeric(ammer.div19$Cover)

ammer.divdf <- ammer.div19 %>% group_by(plot_names) %>% 
  summarize(shannon = vegan::diversity(Cover, index = "shannon"),
            simpson = vegan::diversity(Cover, index = "simpson"),
            specn = vegan::specnumber(Cover))

ammer.divdf$plot_names <- c("RB1", "FE4", "FE1", "FE2", "FE3", "RB2", "GL", "MN", "MW", "UF")
write.csv(ammer.divdf, "E:/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices_Ammer.csv", row.names = F)  

# complete data 13.02. ----
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Satellite_CenterPixel_TimeSeries_2019-20/"))
fls <- list.files(pattern = ".csv")

for (i in 1:length(fls)){
  
  plot <- fls[i] %>% strsplit(., "_") %>% unlist() %>% strsplit(., ".csv") 
  plot <- plot[[4]][1]
  year <- fls[i] %>% strsplit(., "_") %>% unlist()
  year <- year[3]
  df <- read.csv(fls[i])
  
  df[df == -9999 | df == -10000] <- NA
  df$plot_names <- rep(plot, nrow(df))
  if (i == 1){
    df1 <- df
  }else{
    df1 <- rbind(df, df1)
  }
}
df <- na.omit(df1)
df$X <- as.Date(df$X, "%Y-%m-%d")
max_df <- comp_max(df, date.column = "X", max)

# check if all plots have all months
result <- max_df %>%
  group_by(month) %>%
  summarize(Num_Months = n_distinct(plot_names))

m.RF <- result[result$Num_Months == 13,]$month # months usable for RF
# use only 03-09 for RF, 2019 missing for GL plot

max_df %>% group_by(month) %>% 
  ggplot(aes(x = month))+
  geom_bar(stat = "count")
  
max_df.flt <- max_df[max_df$month %in% m.RF,]
# apply scale factor 10000
df.scaled <- max_df.flt %>% mutate(across(all_of(bands), ~. /10000))

# Pivot data
max_df_piv <- pivot.df(df.scaled)
colnames(max_df_piv) <- gsub("-", ".", colnames(max_df_piv))
write.csv(max_df_piv, "G:/Grasslands_BioDiv/Data/Field_Data/Ammer/Reflectance_2019-20_monthly_pivot.csv", row.names = F)

# merge Ammer and BT data
data_frame.ammer <- read.csv("G:/Grasslands_BioDiv/Data/Field_Data/Ammer/Reflectance_2019-20_monthly_pivot.csv")
data_frame <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Reflectance_2022-23_monthly_pivot.csv"))
div_df <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices.csv"))
div_df <- div_df[-c(1)]
ammer_div_df <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices_Ammer.csv"))
# Diversity indices only for 10 plots - right?

biodiv <- "specn"
ammer.plots <- ammer_div_df$plot_names
rf_data_ammer <- preprocess_rf_data(data_frame.ammer, ammer_div_df, biodiv)

predictors <- colnames(data_frame.ammer) %>% gsub("2019", "2022", .) %>% gsub("2020", "2023", .)
data_frame.ammermonths <- RF_predictors(data_frame, predictors)
rf_data <- preprocess_rf_data(data_frame.ammermonths, div_df, biodiv)
BT.plots <- rownames(rf_data)

colnames(rf_data) <- gsub("2022", "2019", colnames(rf_data)) %>% gsub("2023", "2020", .)
max_df_piv.flt <- rf_data_ammer %>% select(names(rf_data))

ammer.BT.df <- rbind(rf_data, max_df_piv.flt)
div_df_merged <- rbind(div_df, ammer_div_df) # merge Ammer and BT diversity df

# option to set percentage of Ammer and BT data in training set
s <- 123
train.ammer <- sample(ammer.plots, 5)
train.BT <- sample(BT.plots, 45)

train.names <- append(train.ammer, train.BT)

train_index <- which(rownames(ammer.BT.df) %in% train.names)
forest <- RF(ammer.BT.df, train_index, s)
print(forest)
rf_plot <- summarize.RF(forest, ammer.BT.df, div_df_merged,train_index,biodiv)
rf_plot
ggsave(plot = rf_plot, paste0(hd, ":/Grasslands_BioDiv/Out/RF_Results/AmmerandBTdataTrainingSetRF-s123.png"))
write.RF("BT and Ammer data in Training Set", biodiv, forest, s, csv.path)
varimp <- plot.varimp(forest, version = 3)                                                                    
ggsave(plot = varimp, paste0(hd, ":/Grasslands_BioDiv/Out/RF_Results/AmmerandBTdataTrainingSet_variableImp-s123.png"))


# 