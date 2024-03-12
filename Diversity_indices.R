# Read Field Data

setwd("E:/Grasslands_BioDiv/Data/Field_Data")
library(readxl)
#library(openxlsx)
library(vegan)
library(sf)
library(ggplot2)

'''
spec_path = "Vegetationskartierungen Hummelgau 2023.xlsx"
plotIDs <- excel_sheets(spec_path)
plotIDs <- plotIDs[1:length(plotIDs)-1]
'''

df <- read_excel(path = "SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx")
df_coords <- df[df$Quadrant == 'A' & df$Depth != "2-7", ]
center_coords <- df_coords[c('Plot', 'PlotCenter_x_coord', 'PlotCenter_y_coord')]
colnames(center_coords) <- c('plot_names', 'X', 'Y')
center_coords$X <- sub(',', '.', center_coords$X) %>%  as.numeric()
center_coords$X <- as.numeric(center_coords$X)
center_coords$Y <- as.numeric(center_coords$Y)

div_df <- read.csv("E:/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices.csv", header = T)
div_df_coords <- merge(div_df, center_coords, by = "plot_names") %>% drop_na()
div_df_coords <- div_df_coords[c('plot_names', 'shannon', 'simpson', 'specn', 'X.y', 'Y')]
colnames(div_df_coords) <- c('plot_names', 'shannon', 'simpson', 'specn', 'X', 'Y')
div.sf <- st_as_sf(div_df_coords, coords = c("X", "Y"), crs = st_crs(25832)) # check if correct epsg code

st_write(div.sf, "Shapes/Biodiv-indices_2022-23.gpkg", driver = "GPKG")

# Centre Coordinates from Biomass df

df_A <- df[df$Subplot_ID %in% plotIDs & df$Quadrant == 'A',]
plotInfo <- df_A[c("Subplot_ID", "PlotCenter_x_coord", "PlotCenter_y_coord", "SamplingDate")]
div_merged <- merge(plotInfo, div_df, by.x = "Subplot_ID", by.y = "plotID") 
div_merged$PlotCenter_x_coord <- as.numeric(gsub(",", ".", div_merged$PlotCenter_x_coord))
div_merged$PlotCenter_y_coord <- as.numeric(gsub(",", ".", div_merged$PlotCenter_y_coord))



# Plotting
# Histogramms 
a <- ggplot(data = div_df, aes(x= specn))+
  geom_histogram(fill = "#BEC45C",color = "black")+
  ggtitle("Species Number")
b <- ggplot(data = div_df, aes(x=shannon))+
  geom_histogram(fill = "#BEC45C",color = "black")+
  ggtitle("Shannon Index")
d <- ggplot(data = div_df, aes(x=simpson))+
  geom_histogram(fill = "#BEC45C",color = "black")+
  ggtitle("Simpson Index")

png("E:/Grasslands_BioDiv/Out/Graphs/Stats/Biodiv_indices_stats.png",width = 10, height = 7, units = "in", res = 1200)
ggarrange(a,b,d,
          labels = c("A","B", "C"),
          nrow=1)
dev.off()

# spatial plotting
ggplot(data = div.sf)+
  geom_sf(aes(color = shannon))

st_write(div.sf, "Shapes/Hummelgau_Biodiv_2023.gpkg", driver = "GPKG")

# create gpkg from Ammer species data ----
hd <- "G"
setwd(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data"))
RB_center_coords <- get_center_coords("SUSALPS_samplingData_BT-RB-FE_2022-2023.xlsx", location = "RB")
Ammer_coords <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Ammer/Plotcenters_merge.csv"))
ammer_div_df <- read.csv(paste0(hd, ":/Grasslands_BioDiv/Data/Field_Data/Biodiv-indices_Ammer.csv"))

plot_names <- strsplit(Ammer_coords$layer, "_") %>% lapply(., `[[`,1) %>% unlist()
Ammer_coords$plot_names <- plot_names
ammer.merged <- merge(Ammer_coords, ammer_div_df, by = "plot_names")[c('plot_names', 'Y', 'X', 'shannon', 'simpson','specn')]
ammer.sf <- st_as_sf(ammer.merged, coords = c("X", "Y"), crs = st_crs(25832))
st_write(ammer.sf, "Shapes/Ammer-Biodiv.gpkg", driver = "GPKG")
