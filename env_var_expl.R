library(dismo)
library(ENMeval)
library(maxnet)
library(ENMTools)
library(maptools)
library(rgeos)
library(sp)
library(spdplyr)
library(raster)
library(rasterVis)
library(climateStability)
library(SpatialTools)
library(corrplot)
library(spatialEco)
library(MIAmaxent)
library(spocc)
library(sf)
?`ENMeval-package`


set.seed(48)

crs <- CRS("+init=epsg:4329")
crs_new <-
  CRS(
    "+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )

UG <- data.frame(
  x = c(36, 14, 14, 36),
  y = c(17, 17, 9, 9)
)

UG_sp <- SpatialPolygons(list(Polygons(list(Polygon(
  UG
)), ID = 1)))

# Loading data ------------------------------------------------------------

path_base <-
  "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/Environmental_Variables_Ind"


LSM_float_path <- paste0(path_base, "/LSM_float")
LSM_float_list <-list.files(path = LSM_float_path, pattern = ".tif$", full.names = TRUE)
LSM_float_rlist <- lapply(LSM_float_list, raster)
names(LSM_float_rlist) <- c("CoV", "Entropy", "Evenness", "Homogeneity")

LSM_movingW <- raster(paste0(path_base, "/LSM_cat/moving_window_44.tif"), extent = extent)
names(LSM_movingW) <- "Variance"

weather_path <- paste0(path_base, "/Weather")
weather_list <- list.files(path=weather_path, pattern = ".tif$", full.names = TRUE)
weather_rlist <- lapply(weather_list, raster)
names(weather_rlist) <- c("prec_10", "prec_11", "prec_12", "prec_01", "prec_02", "prec_03",
                          "tmax_10", "tmax_11", "tmax_12", "tmax_01", "tmax_02", "tmax_03",
                          "tmin_10", "tmin_11", "tmin_12", "tmin_01", "tmin_02", "tmin_03")

elev <- raster(paste0(path_base, "/elevation/elev_UG.tif"))
names(elev) <- "elevation"

settl_dist <- raster(paste0(path_base, "/Settlements/Settlements_distance.tif"))
names(settl_dist) <- "dist2settlements"

water_dist <- raster(paste0(path_base, "/Water_new/dist2water.tif"))
names(water_dist) <- "dist2water"

LandCov_path <- paste0(path_base, "/LandCover_float")
LandCov_list <- list.files(path=LandCov_path, pattern = ".tif$", full.names = TRUE)
LandCov_rlist <- lapply(LandCov_list, raster)
names(LandCov_rlist) <- c("tree_cover","shrub_cover","herb_cover","cultivated_cover")

NDVI_w_path <- paste0(path_base, "/NDVI_weather")
NDVI_w_list <- list.files(path=NDVI_w_path, pattern = ".tif$", full.names = TRUE)
NDVI_w_rlist <- lapply(NDVI_w_list, raster)
names(NDVI_w_rlist) <- c("NDVI_10", "NDVI_11", "NDVI_12", "NDVI_01", "NDVI_02","NDVI_03")


# norm to [0;1] -----------------------------------------------------------


LSM_f_rlist_norm <- lapply(LSM_float_rlist, rescale0to1)
LSM_f_stack_norm <- stack(LSM_f_rlist_norm)

LSM_movingW_norm <- rescale0to1(LSM_movingW)

weather_rlist_norm <- lapply(weather_rlist, rescale0to1)
weather_stack_norm <- stack(weather_rlist_norm)

elev_norm <- rescale0to1(elev)

settl_dist_norm <- rescale0to1(settl_dist)

water_dist_norm <- rescale0to1(water_dist)

LandCover_rlist_norm <- lapply(LandCov_rlist, rescale0to1)
LandCover_stack_norm <- stack(LandCover_rlist_norm)

NDVI_w_rlist_norm <- lapply(NDVI_w_rlist, rescale0to1)
NDVI_w_stack_norm <- stack(NDVI_w_rlist_norm)

all_env_l <- list(LSM_f_stack_norm, LSM_movingW_norm, weather_stack_norm,
                  elev_norm, settl_dist_norm, water_dist_norm, LandCover_stack_norm,
                  NDVI_w_stack_norm)


# combine all to one raster stack -----------------------------------------

#Auf gröbere Auflösung der Wetterdaten resamplen, bilinear für kontinuirl. Daten empfohlen

all_env_r_norm <- lapply(all_env_l, raster::resample, weather_rlist[[1]], "bilinear")

pred_norm <- stack(all_env_r_norm)

#Prediktor-Sets (nur für Korrelationsanalyse, da mit prec-1)

pred_10_norm <- pred_norm[[c(2,4,5,6,12,18,24:30,31)]]
pred_11_norm <- pred_norm[[c(2,4,5,6, 7,13,19,24:30,32)]]
pred_12_norm <- pred_norm[[c(2,4,5,7, 8,14,20,24:30,33)]]
pred_01_norm <- pred_norm[[c(2,4,5,8, 9,15,21,24:30,34)]]
pred_02_norm <- pred_norm[[c(2,4,5,9, 10,16,22,24:30,35)]]
pred_03_norm <- pred_norm[[c(2,4,5,10, 11,17,23,24:30,36)]]

pred_sets <- list(pred_10_norm, pred_11_norm, pred_12_norm, pred_01_norm, pred_02_norm, pred_03_norm)


#Korrelation der LSM in Originalauflösung vorbereiten

LSM_movingW_res <- raster::resample(LSM_movingW_norm, LSM_f_stack_norm, "bilinear")
LSM_set <- stack(LSM_f_stack_norm, LSM_movingW_res)


# test env layers on collinearity ------------------------------------------

#Korrelation der LSM in Originalauflösung berechnen

LSM_corr <- raster.cor.matrix(LSM_set, method = "pearson")
LSM_corr_matrix <- data.matrix(LSM_corr)

png("C:/1_Bachelorarbeit/Statistik/Diagramme/LSM_orig_corr.png", 
    width = 8, height = 8, units = "cm", res = 1200)

LSM_corr_plot <- corrplot(LSM_corr_matrix, method = "color", 
                          tl.pos='lt', addCoefasPercent=F, 
                          tl.col = "black", tl.srt=45, addCoef.col = "black")

dev.off()

#Korrelation der Umweltvariablen berechnen (Individuenmodell)

pred_sets_cor <- lapply(pred_sets, raster.cor.matrix, method = "pearson") 

pred_sets_matrix <- lapply(pred_sets_cor, data.matrix) #calculate the correlation matrix for all predictor sets

for(i in seq_along(pred_sets_matrix)) {
  png(paste("C:/1_Bachelorarbeit/Statistik/Diagramme/cmt", i, ".png"), 
      width = 14, height = 14, units = "cm", res = 1200)
  corrplot <- corrplot(pred_sets_matrix[[i]], method = "color", 
                       tl.pos='lt', tl.cex=0.6,number.cex=0.5, addCoefasPercent=F, 
                       tl.col = "black", tl.srt=45, addCoef.col = "black")
  dev.off()
}

#remove the highly correlated env variables (r>0.7)


# Beispielkarten erstellen ------------------------------------------------

pred_02_prepplot <- pred_norm[[c(2,4,5,26,25,9,10,16,22,35,27,28,29,30,24)]]
p.strip <- list(cex=0.9)

png("C:/1_Bachelorarbeit/Statistik/Diagramme/maps_feb.png",res = 1600, width = 15, height = 15, units = "cm")

maps_feb <- levelplot(pred_02_prepplot, par.settings = viridisTheme, at = seq(0,1, length  = 40),
                      main = list("Umweltvariablen - Februar", size = 9), par.strip.text=p.strip, 
                      colorkey = list(cex = 0.9), between = list(x=0.5, y=0.5))
maps_feb

dev.off()


