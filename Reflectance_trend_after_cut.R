library(sp)
library(stringr)
library(openxlsx)
library(abind)
library(dplyr)
library(zoo)
library(reshape2)
library(tidyr)
library(vegan)
library(readxl)
library(ggplot2)

bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")

refl_df <- read.csv("E:/Grasslands_BioDiv/Data/S2_Reflectances/Reflectance_2022-23_grouped.csv")
df.long <- pivot_longer(refl_df, cols = bands, names_to = "variable", values_to = "reflectance")

# First Cut in BT areas
for (yr in seq(2018, 2023, 1)){
  if (yr == 2018){
    df_firstcut <- get_first_cut("FirstCut", "UPA", yr, BT_center_coords)
  }else{
    
    df1 <- get_first_cut("FirstCut", "UPA", yr, BT_center_coords)
    df_firstcut <- merge(df1, df_firstcut, by = "plot_names")
  }
}

plot = "88-1"
plots <- BT_center_coords$plot_names[-13] # remove obernschreez west
df.incr <- data.frame(matrix(nrow = 59, ncol = 10))
colnames(df.incr) <- bands
rownames(df.incr) <- plots
for (plot in plots){
  doys <- df_firstcut[df_firstcut$plot_names == plot, c("FirstCut_2022", "FirstCut_2023")] %>% unlist() %>% unname()
  
  #lapply(doys, FUN = doy2date, yr = c(2022, 2023))
  doy2022 <- doy2date(doys[1], yr = 2022)
  doy2023 <- doy2date(doys[2], yr = 2023)
  
  df.long$dat <- as.Date(df.long$dat)
  df.long <- na.omit(df.long)
  df.long[df.long$plot_names == plot, ] %>%
    ggplot(aes(x = dat, y = reflectance, color = variable))+
    geom_line()+
    geom_point()+
    geom_vline(xintercept = doy2022)+
    geom_vline(xintercept = doy2023)+
    ggtitle(paste0("Reflectance TS Plot ", plot))
  # ggsave(paste0("E:/Grasslands_BioDiv/Out/Graphs/Refl_TS-perPlot/", plot, "-TS.png"))
  # 
  # decreasing or increasing reflectances after cut
  if (!is.na(doy2022)){
    for (band in bands){
    
      df.plot.band <- df.long[df.long$plot_names == plot & df.long$variable == band,]
      acq_idx_after_cut <- which(df.plot.band$dat > doy2022)[1]
      refl <- df.plot.band$reflectance[acq_idx_after_cut : (acq_idx_after_cut +3)]  
      
      incr <- all(refl == cummax(refl))
      decr <- all(refl == cummin(refl))
      
      if (incr == T){
        df.incr[plot, band] <- 1
      }
      if (decr == T){
        df.incr[plot, band] <- -1
      }
    }
  }
  
}
write.csv(df.incr, "E:/Grasslands_BioDiv/Out/Increasing-decreasing_Refl_after_cut-2022.csv", row.names = F)
df_incr2023 <- df.incr

# Plot Trends per Band ----

# rownames lost
refl_trend2022 <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Out/Increasing-decreasing_Refl_after_cut-2022.csv"))
refl_trend2023 <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Out/Increasing-decreasing_Refl_after_cut-2023.csv"))
refl_trend2022$year <- rep(2022, nrow(refl_trend2022))
refl_trend2023$year <- rep(2023, nrow(refl_trend2023))

refl_trend.long <- pivot_longer(refl_trend2022, cols = bands, names_to = "bands", values_to = "trend")
refl_trend.long$trend[is.na(refl_trend.long$trend)] <- "NA"

trend2022 <- ggplot(refl_trend.long, aes(x= factor(bands), fill = factor(trend)))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("-1" = "blue", "1" = "red", "NA" = "grey"),
                    na.value = "grey", labels = c("Decreasing", "Increasing", "No Trend"))+
  labs(x="Band", y = "Count", fill = "Trend")+
  ggtitle("Trends after Mowing Event per Band")+
  theme_minimal()

refl_trend.long <- pivot_longer(refl_trend2023, cols = bands, names_to = "bands", values_to = "trend")
refl_trend.long$trend[is.na(refl_trend.long$trend)] <- "NA"

trend2023 <- ggplot(refl_trend.long, aes(x= factor(bands), fill = factor(trend)))+
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c("-1" = "blue", "1" = "red", "NA" = "grey"),
                    na.value = "grey", labels = c("Decreasing", "Increasing", "No Trend"))+
  labs(x="Band", y = "Count", fill = "Trend")+
  ggtitle("Trends after Mowing Event per Band")+
  theme_minimal()

png("G:/Grasslands_BioDiv/Out/Graphs/TrendperBandafterCut.png",width = 10, height = 8, units = "in", res = 1200)
ggarrange(trend2022, trend2023, ncol = 2, labels = c("2022", "2023"), common.legend = T, hjust = 0.05)
dev.off()
