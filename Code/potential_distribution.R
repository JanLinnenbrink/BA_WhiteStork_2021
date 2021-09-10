# set path ---------------------------------------------------

#set path to folder (only path that needs to be specified manually)
path <- "C:/HSM_Ciconia_ciconia_data/"

#set path to known distribution map
path_known_distr <- paste0(path, "environmental_data/BirdLife_distribution_map/")

#set path to input variables
path_base_pop <- paste0(path, "environmental_data/Environmental_Variables_Pop")

#set path to output directory
dir.create(paste0(path, "results/"))
path_out<- paste0(path, "results/")

#create output folder for the population models
dir.create(paste0(path_out, "potential_distribution"))
path_out_pop <- paste0(path_out, "potential_distribution/")


# load packages -----------------------------------------------------------


options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)

library(gdata)
library(plyr)
library(tidyverse)
library(ENMeval)
library(rmaxent)
library(spdplyr)
library(dismo)
library(ggsci)
library(egg)
library(reshape)
library(amt)
library(move)
library(rgdal)
library(rmaxent)
library(lubridate)
library(maptools)
library(sf)
library(sp)
library(dplyr)
library(spatialEco)
library(PBSmapping)
library(tidyverse)
library(raster)
library(ecoinfo)
library(scales)
library(spocc)
library(mapr)
library(ggpubr)
library(climateStability)
library(rasterVis)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(corrplot)

crs <- CRS("+init=epsg:4329")

set.seed(28)


# define study area -------------------------------------------------------

UG <- data.frame(
  x = c(36, 14, 14, 36),
  y = c(17, 17, 9, 9))

UG_sp <- SpatialPolygons(list(Polygons(list(Polygon(UG)), ID = 1)))

crs(UG_sp) <- crs

UG_sf <- st_as_sf(UG_sp)

# prepare environmental variables -----------------------------------------

# load environmental variables
elev <- raster(paste0(path_base_pop, "/elevation/elev_UG.tif"))
names(elev) <- "elevation"
elev_norm <- rescale0to1(elev)

LSM_float_path <- paste0(path_base_pop, "/LSM_float")
LSM_float_list <-list.files(path = LSM_float_path, pattern = ".tif$", full.names = TRUE)
LSM_float_rlist <- lapply(LSM_float_list[c(2,4)], raster) %>% lapply(crop,elev)
names(LSM_float_rlist) <- c("Entropy", "Homogeneity")
LSM_float_norm <- lapply(LSM_float_rlist, rescale0to1) 
LSM_float_stack <- stack(LSM_float_rlist)

LSM_movingW <- raster(paste0(path_base_pop, "/LSM_cat/moving_window_44.tif")) %>% crop(elev)
names(LSM_movingW) <- "Variance"
LSM_movingW_norm <- rescale0to1(LSM_movingW)

settl_dist <- raster(paste0(path_base_pop, "/Settlements/Settlements_distance.tif")) %>% crop(elev)
names(settl_dist) <- "dist2settlements"
settl_dist_norm <- rescale0to1(settl_dist)

water_dist <- raster(paste0(path_base_pop, "/Water_new/dist2water.tif")) %>% crop(elev)
names(water_dist) <- "dist2water"
water_dist_norm <- rescale0to1(water_dist)

LandCov_path <- paste0(path_base_pop, "/LandCover_float")
LandCov_list <- list.files(path=LandCov_path, pattern = ".tif$", full.names = TRUE)
LandCov_rlist <- lapply(LandCov_list, raster) %>% lapply(crop,elev)
names(LandCov_rlist) <- c("tree_cover","shrub_cover","herb_cover","cultivated_cover")
LandCov_norm <- lapply(LandCov_rlist, rescale0to1)
LandCov_stack <- stack(LandCov_rlist)

prec_path <- paste0(path_base_pop, "/prec")
prec_list <- list.files(path=prec_path, pattern = ".tif$", full.names = TRUE)
prec_rlist <- lapply(prec_list, raster) %>% lapply(crop, elev)
names(prec_rlist) <- c("prec_01", "prec_02", "prec_10", "prec_11", "prec_12")
prec_norm <- lapply(prec_rlist, rescale0to1)
prec_stack <- stack(prec_rlist)

bio16 <- raster(paste0(path_base_pop, "/bio16/bio16.tif")) %>% crop(elev)
bio16_norm <- rescale0to1(bio16)

tmax_path <- paste0(path_base_pop, "/tmax")
tmax_list <- list.files(path=tmax_path, pattern = ".tif$", full.names = TRUE)
tmax_rlist <- lapply(tmax_list, raster) %>% lapply(crop,elev)
names(tmax_rlist) <- c("tmax_01", "tmax_02", "tmax_10", "tmax_11", "tmax_12")
tmax_norm <- lapply(tmax_rlist, rescale0to1)
tmax_stack <- stack(tmax_rlist)

NDVI_c_path <- paste0(path_base_pop, "/NDVI")
NDVI_c_list <- list.files(path=NDVI_c_path, pattern = ".tif$", full.names = TRUE)
NDVI_c_rlist <- lapply(NDVI_c_list, raster) %>% lapply(crop,elev)
names(NDVI_c_rlist) <- c("NDVIc_01", "NDVIc_02", "NDVIc_10", "NDVIc_11", "NDVIc_12")
NDVI_c_norm <- lapply(NDVI_c_rlist, rescale0to1)
NDVI_c_stack <- stack(NDVI_c_rlist)

preds_pop_rlist <- list(prec_rlist, tmax_rlist, NDVI_c_rlist,
                        LandCov_rlist, settl_dist, elev, 
                        LSM_float_rlist, LSM_movingW)

preds_pop_stack <- stack(prec_stack, tmax_stack, NDVI_c_stack, LandCov_stack,bio16,
                         settl_dist, water_dist, elev, LSM_float_stack, LSM_movingW)

saveRDS(preds_pop_stack, paste0(path_out_pop, "pred_pop"))

# norm models and correlation matrix 
preds_norm <- stack(stack(LSM_movingW_norm,stack(LSM_float_norm), bio16_norm,
                          settl_dist_norm, water_dist_norm),elev_norm, stack(LandCov_norm),
                    stack(prec_norm),stack(tmax_norm), stack(NDVI_c_norm))

names(preds_norm)

pred_10_norm <- preds_norm[[c(1:11, 14, 19, 24)]]
pred_11_norm <- preds_norm[[c(1:11, 15, 20, 22)]]
pred_12_norm <- preds_norm[[c(1:11, 16, 21, 26)]]
pred_01_norm <- preds_norm[[c(1:11, 12, 17, 22)]]
pred_02_norm <- preds_norm[[c(1:11, 13, 18, 23)]]
names(pred_02_norm)

pred_sets <- list(pred_10_norm, pred_11_norm, pred_12_norm, pred_01_norm, pred_02_norm)

pred_sets_cor <- lapply(pred_sets, ENMTools::raster.cor.matrix, method = "pearson")

pred_sets_matrix <- lapply(pred_sets_cor, data.matrix) #calculate the correlation matrix for all predictor sets

dir.create(paste0(path_out_pop, "corr_matrix/"))
path_cor_matrix <- paste0(path_out_pop, "corr_matrix/")
  
for(i in seq_along(pred_sets_matrix)) {
  png(paste0(path_cor_matrix, "cmt_", i, ".png"), 
      width = 14, height = 14, units = "cm", res = 1200)
  corrplot <- corrplot(pred_sets_matrix[[i]], method = "color", 
                       tl.pos='lt', tl.cex=0.6,number.cex=0.5, addCoefasPercent=F, 
                       tl.col = "black", tl.srt=45, addCoef.col = "black")
  dev.off()
}

#split environmental variables before modeling
env <- readRDS(paste0(path_out_pop, "pred_pop"))

env10_ndvi <- env %>% dropLayer(c("prec_11", "prec_12", "prec_01", "prec_02",
                                  "tmax_11", "tmax_12", "tmax_01", "tmax_02",
                                  "NDVIc_11", "NDVIc_12", "NDVIc_01", "NDVIc_02", "bio16"))
env11_ndvi <- env %>% dropLayer(c("prec_10", "prec_12", "prec_01", "prec_02",
                                  "tmax_10", "tmax_12", "tmax_01", "tmax_02",
                                  "NDVIc_10", "NDVIc_12", "NDVIc_01", "NDVIc_02", "bio16"))
env12_ndvi <- env %>% dropLayer(c("prec_11", "prec_10", "prec_01", "prec_02",
                                  "tmax_11", "tmax_10", "tmax_01", "tmax_02",
                                  "NDVIc_11", "NDVIc_10", "NDVIc_01", "NDVIc_02", "bio16"))
env01_ndvi <- env %>% dropLayer(c("prec_11", "prec_12", "prec_10", "prec_02",
                                  "tmax_11", "tmax_12", "tmax_10", "tmax_02",
                                  "NDVIc_11", "NDVIc_12", "NDVIc_10", "NDVIc_02", "bio16"))
env02_ndvi <- env %>% dropLayer(c("prec_11", "prec_12", "prec_01", "prec_10",
                                  "tmax_11", "tmax_12", "tmax_01", "tmax_10",
                                  "NDVIc_11", "NDVIc_12", "NDVIc_01", "NDVIc_10", "bio16"))

env_ndvi_list <- list(env10_ndvi,env11_ndvi,env12_ndvi,env01_ndvi,env02_ndvi)
rm(env10_ndvi,env11_ndvi,env12_ndvi,env01_ndvi,env02_ndvi)


env10_climate <- env %>% dropLayer(c("prec_11", "prec_12", "prec_01", "prec_02",
                                     "tmax_11", "tmax_12", "tmax_01", "tmax_02",
                                     "NDVIc_11", "NDVIc_10", "NDVIc_12", "NDVIc_01", 
                                     "NDVIc_02"))
env11_climate <- env %>% dropLayer(c("prec_10", "prec_12", "prec_01", "prec_02",
                                     "tmax_10", "tmax_12", "tmax_01", "tmax_02",
                                     "NDVIc_10", "NDVIc_11", "NDVIc_12", "NDVIc_01",
                                     "NDVIc_02"))
env12_climate <- env %>% dropLayer(c("prec_11", "prec_10", "prec_01", "prec_02",
                                     "tmax_11", "tmax_10", "tmax_01", "tmax_02",
                                     "NDVIc_11", "NDVIc_10", "NDVIc_12", "NDVIc_01", "NDVIc_02"))
env01_climate <- env %>% dropLayer(c("prec_11", "prec_12", "prec_10", "prec_02",
                                     "tmax_11", "tmax_12", "tmax_10", "tmax_02",
                                     "NDVIc_11", "NDVIc_12", "NDVIc_10", "NDVIc_01", "NDVIc_02"))
env02_climate <- env %>% dropLayer(c("prec_11", "prec_12", "prec_01", "prec_10",
                                     "tmax_11", "tmax_12", "tmax_01", "tmax_10",
                                     "NDVIc_11", "NDVIc_12", "NDVIc_01", "NDVIc_10", "NDVIc_02"))

env_climate_list <- list(env10_climate,env11_climate,env12_climate,env01_climate,env02_climate)
rm(env10_climate,env11_climate,env12_climate,env01_climate,env02_climate)

saveRDS(env_climate_list, paste0(path_out_pop, "env_climate_list"))
saveRDS(env_ndvi_list, paste0(path_out_pop, "env_ndvi_list"))

env_ndvi_list <- readRDS(paste0(path_out_pop, "env_ndvi_list"))
env_climate_list <- readRDS(paste0(path_out_pop, "env_climate_list"))

# download and filter occurrence data ---------------------------------------------

#set movebank login
login <- movebankLogin(username = "JanL", password = "abXv68_c")

#define columns which are needed for further analysis
keep_cols <- c( "timestamp","location.long","location.lat","ind_name", "month", "year")

thin_distance <- 1000 # meters
thin_time <- 120

threshold_min_n <- 40 # Minimal occurrence data/individual/month

t1 <- strptime("20001001000000", format = "%Y%m%d%H%M%S", tz = 'UTC')
t32 <- strptime("20160228000000", format = "%Y%m%d%H%M%S", tz = 'UTC')

# Download movebank data, only keep Argos data with estimate error < 1km
mpiab_gps <- getMovebankLocationData(
  study = "MPIAB Argos white stork tracking (1991-2018)",
  login = login,
  timestamp_start = t1,
  timestamp_end = t32,
  removeDuplicatedTimestamps = TRUE,
  sensorID = 653 #gps
)

mg_d <- as.data.frame(mpiab_gps)
n.storks <- length(unique(mg_d$individual.id))
ind_name <- paste0("MPg", 1:n.storks)
mg_d$ind_name <- factor(mg_d$individual.id, labels = ind_name)
mg_d <- mg_d[, (names(mg_d) %in% keep_cols)]

mg_d$year <- year(mg_d$timestamp)
mg_d$month <- month(mg_d$timestamp)

mg_d <- mg_d %>% group_by(year) %>% filter(month >= 10 | month <=2)


mpiab_argos <- getMovebankLocationData(
  study = "MPIAB Argos white stork tracking (1991-2018)",
  login = login,
  timestamp_start = t1,
  timestamp_end = t32,
  removeDuplicatedTimestamps = TRUE,
  sensorID = 82798 #argos-doppler-shift
)

ma_d <- as.data.frame(mpiab_argos)
n.storks <- length(unique(ma_d$individual.id))
ind_name <- paste0("MPa", 1:n.storks)
ma_d$ind_name <- factor(ma_d$individual.id, labels = ind_name)

ma_d$year <- year(ma_d$timestamp)
ma_d$month <- month(ma_d$timestamp)

ma_d <- ma_d %>% group_by(year) %>% filter(month >= 10 | month <=2)

ma_d <- ma_d[ma_d$argos.lc %in% c(3, 2, 1), ] # estimate error <= 1.5km

ma_d <- ma_d[, (names(ma_d) %in% keep_cols)]

# Merge all TD into one data.frame
TD <- rbind(mg_d, ma_d)

TD_wo_outl <- TD %>% subset(location.long>-50 & location.long <50 & location.lat>-40)

#plot TD
world <- ne_countries(scale = "medium", returnclass = "sf")

theme_set(theme_bw())
p <- ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-25, 60), ylim = c(-40, 75), expand = FALSE) +
  geom_point(data=TD_wo_outl, mapping=aes(x=location.long, y=location.lat), shape=16, alpha =0.4) +
  xlab("Längengrad") + ylab("Breitengrad")

ggsave(paste0(path_out, "flight_map.png"), plot = p)

# Clip tracking data to study area
known_distr <- readOGR(paste0(path_known_distr, "Ciconia_ciconia.shp"))

b <- env[[23]] %>% mask(known_distr) #elev as background raster
crs(TD) <- crs

td_ug <- raster::extract(x=b, y=TD)
TD$vals <- td_ug
TD <- TD[!is.na(TD$vals),]


# Remove all locations from night except one
TD_amt <-
  make_track(
    TD,
    .x = location.long,
    .y = location.lat,
    .t = timestamp,
    ind.id = ind_name,
    month = month,
    year=year,
    crs = crs
  )

TD_track <- time_of_day(TD_amt)

TD_track$day <- substr(x = TD_track$t_, start = 0, stop = 10)

TD_night <- TD_track %>%
  filter(tod_ == "night") %>%
  group_by(ind.id, day, year) %>%
  slice(1)

TD_day <- TD_track %>%
  filter(tod_ == "day")

TD_track <- full_join(TD_day, TD_night) %>%
  select(-day)

# temporal thinning
TD_track$groupsTime <- cut(TD_track$t_, breaks = "90 mins")

TD_track_tempFilter <- TD_track %>%
  group_by(groupsTime, ind.id) %>%
  filter(row_number() == 1)

TD_track_tempFilter$tod_ <- NULL
TD_track_tempFilter$groupsTime <- NULL

n_temporalthinning <- TD_track_tempFilter %>%
  group_by(ind.id, month) %>%
  dplyr::summarise(Freq = n())
colnames(n_temporalthinning) <- c("ind_id", "month", "Freq")

# Spatially filter the location data to 1 km
names(TD_track_tempFilter) <- c("lng", "lat", "time", "ind_id", "month", "year")

TD_g <- TD_track_tempFilter %>% split(list(TD_track_tempFilter$ind_id,
                                           TD_track_tempFilter$month, 
                                           TD_track_tempFilter$year),
                                      drop = TRUE)

spatialFilter <- function(df, dist) {
  coordinates(df) <- ~ lng + lat
  crs(df) <- crs
  df_tr <- spTransform(df, crs_new)
  TD_sT2 <- remove.near(df_tr, dist = dist)
}

TD_SpT <- lapply(TD_g, spatialFilter, dist = thin_distance)

TD_SpT_SPDF <- do.call(rbind.SpatialPointsDataFrame, TD_SpT) %>% spTransform(crs) 

#impose threshold of 40 to remove individuals with less data points (e.g. through migration)
TD_threshold <-
  TD_SpT_SPDF %>%
  as.data.frame() %>%
  group_by(ind_id, month, year) %>%
  filter(n() >= threshold_min_n)

#remove not existing levels of ind_id
TD_threshold$ind_id <- factor(TD_threshold$ind_id)

n_spatiotemporalthinning <-
  TD_threshold %>%
  as.data.frame() %>%
  group_by(ind_id, month, year) %>%
  dplyr::summarise(Freq = n())

# Subsample the location data by the minimum number of location data for each individual 
n_min <- min(n_spatiotemporalthinning[, 4])

TD_samples <-
  TD_threshold %>%
  as.data.frame() %>%
  select(-year) %>% 
  group_by(ind_id, month) %>%
  sample_n(n_min) %>% 
  as.data.frame()
TD_samples$ind_id <- factor(TD_samples$ind_id)

coordinates(TD_samples) <- ~ lng + lat
crs(TD_samples) <- crs

saveRDS(TD_samples, paste0(path_out_pop, "occs_pot_ug"))


# prepare occurrence and background points --------------------------------------------------

dir.create(paste0(path_out_pop, "occurence_points"))
path_occs <- paste0(path_out_pop, "occurence_points/")

occ_sp <- readRDS(paste0(path_out_pop, "occs_pot_ug"))
occ_sp10 <- occ_sp[occ_sp$month==10,]
occ_sp11 <- occ_sp[occ_sp$month==11,]
occ_sp12 <- occ_sp[occ_sp$month==12,]
occ_sp01 <- occ_sp[occ_sp$month==01,]
occ_sp02 <- occ_sp[occ_sp$month==02,]
occ_sp_list <- list(occ_sp10, occ_sp11, occ_sp12, occ_sp01, occ_sp02)

occ_l <- lapply(occ_sp_list, as.data.frame) %>% lapply(select, c("lng", "lat"))

saveRDS(occ_l, paste0(path_out_pop, "occ_l"))

occ_l_sf <- list()

for(i in seq_along(occ_sp_list)) {
  occ_l_sf[[i]] <- st_as_sf(occ_sp_list[[i]], coords = c("lng", "lat"))
  st_write(occ_l_sf[[i]], paste0(path_occs, "occs_pop_", i, ".shp"))
}

#create background points
bg <- dismo::randomPoints(b, n = 10000) %>% as.data.frame()

bg10_ndvi <- cbind(bg, raster::extract(env_ndvi_list[[1]] , bg)) 
colnames(bg10_ndvi ) [1:2] <- c("lng", "lat")
bg11_ndvi  <- cbind(bg, raster::extract(env_ndvi_list[[2]] , bg))
colnames(bg11_ndvi ) [1:2] <- c("lng", "lat")
bg12_ndvi  <- cbind(bg, raster::extract(env_ndvi_list[[3]] , bg))
colnames(bg12_ndvi) [1:2] <- c("lng", "lat")
bg01_ndvi  <- cbind(bg, raster::extract(env_ndvi_list[[4]] , bg))
colnames(bg01_ndvi ) [1:2] <- c("lng", "lat")
bg02_ndvi  <- cbind(bg, raster::extract(env_ndvi_list[[5]] , bg))
colnames(bg02_ndvi ) [1:2] <- c("lng", "lat")

bg_ndvi_list <- list(bg10_ndvi, bg11_ndvi, bg12_ndvi, bg01_ndvi, bg02_ndvi)
rm(bg10_ndvi, bg11_ndvi, bg12_ndvi, bg01_ndvi, bg02_ndvi)

bg10_climate <- cbind(bg, raster::extract(env_climate_list[[1]] , bg)) 
colnames(bg10_climate ) [1:2] <- c("lng", "lat")
bg11_climate  <- cbind(bg, raster::extract(env_climate_list[[2]] , bg))
colnames(bg11_climate ) [1:2] <- c("lng", "lat")
bg12_climate  <- cbind(bg, raster::extract(env_climate_list[[3]] , bg))
colnames(bg12_climate) [1:2] <- c("lng", "lat")
bg01_climate  <- cbind(bg, raster::extract(env_climate_list[[4]] , bg))
colnames(bg01_climate ) [1:2] <- c("lng", "lat")
bg02_climate  <- cbind(bg, raster::extract(env_climate_list[[5]] , bg))
colnames(bg02_climate ) [1:2] <- c("lng", "lat")

bg_climate_list <- list(bg10_climate, bg11_climate, bg12_climate, bg01_climate, bg02_climate)
rm(bg10_climate, bg11_climate, bg12_climate, bg01_climate, bg02_climate, bg)

occ_ndvi_list <- list()
occ_climate_list <- list()

for (i in seq_along(occ_l)) {
  occ_ndvi_list[[i]] <- cbind(occ_l[[i]], raster::extract(env_ndvi_list[[i]], occ_l[[i]]))
  occ_climate_list[[i]] <- cbind(occ_l[[i]], raster::extract(env_climate_list[[i]], occ_l[[i]]))
}
rm(b, known_distr, occ_sp_list)

saveRDS(occ_ndvi_list, paste0(path_out_pop, "occ_ndvi_list"))
saveRDS(occ_climate_list, paste0(path_out_pop,"occ_climate_list"))
saveRDS(bg_ndvi_list, paste0(path_out_pop,"bg_ndvi_list"))
saveRDS(bg_climate_list, paste0(path_out_pop,"bg_climate_list"))


# model calibration -----------------------------------------------------------------
rm(list=setdiff(ls(), c("occ_ndvi_list", "bg_ndvi_list",
                      "occ_climate_list", "bg_climate_list",
                      "env_ndvi_list")))
get_maxent()

fc <- c("L", "LQ",  "LQP", "H")
rm <- seq(1, 4, 0.5)

tune.args <- list(fc = fc, rm = rm)

eval_models_climate <- list()
eval_models_ndvi <- list()


for(i in seq_along(occ_ndvi_list)) {
  eval_models_ndvi[[i]] <- ENMevaluate(occs = occ_ndvi_list[[i]], bg = bg_ndvi_list[[i]][,1:15],
                                          algorithm = "maxent.jar", tune.args = tune.args, 
                                          partitions = "block", updateProgress = TRUE,
                                          numCores = NULL)
}

for(i in seq_along(occ_climate_list)) {
  eval_models_climate[[i]] <- ENMevaluate(occs = occ_climate_list[[i]], bg = bg_climate_list[[i]][,1:15],
                                       algorithm = "maxent.jar", tune.args = tune.args, 
                                       partitions = "block", updateProgress = TRUE,
                                       numCores = NULL)
}

saveRDS(eval_models_ndvi, paste0(path_out_pop, "eval_models_ndvi"))
saveRDS(eval_models_climate, paste0(path_out_pop,"eval_models_climate"))


# visualize the comparison of settings ------------------------------------

dir.create(path = paste0(path_out_pop, "eval_plots"))
path_eval <- paste0(path_out_pop, "eval_plots/")

theme_set(new = theme_bw())

AUC_diff_ndvi <- list()
AUC_test_ndvi <- list()
OR_10_ndvi <- list()
AICc_ndvi <- list()

for(i in seq_along(eval_models_ndvi)) {
  AUC_diff_ndvi[[i]] <- ggplot(eval_models_ndvi[[i]]@results, 
                            aes(x=rm, y=auc.diff.avg, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("AUC.diff") +
    theme_set(theme_bw()) +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(1,1),
          legend.position = c(1,1),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  
  AUC_test_ndvi[[i]] <- ggplot(eval_models_ndvi[[i]]@results, 
                            aes(x=rm, y=auc.val.avg, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("AUC.test") +
    theme_set(theme_bw()) +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(0,0),
          legend.position = c(0,0),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  
  OR_10_ndvi[[i]] <- ggplot(eval_models_ndvi[[i]]@results,
                         aes(x=rm, y=or.10p.avg, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("OR.10") +
    theme_set(theme_bw()) +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(1,1),
          legend.position = c(1,1),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  
  AICc_ndvi[[i]] <- ggplot(eval_models_ndvi[[i]]@results, 
                        aes(x=rm, y=AICc, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("AICc") +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(1,0),
          legend.position = c(1,0),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  plot_list_ndvi <- list(AUC_diff_ndvi[[i]], AUC_test_ndvi[[i]], OR_10_ndvi[[i]], AICc_ndvi[[i]])
}

g_b_ndvi <- list()

for(i in seq_along(AUC_diff_ndvi)) {
  g_b_ndvi[[i]] <- ggarrange(AUC_diff_ndvi[[i]], AUC_test_ndvi[[i]], 
                             OR_10_ndvi[[i]], AICc_ndvi[[i]],
                             legend="none")
}

for(i in seq_along(g_b_ndvi)) {
  ggsave(paste0(path_eval, "NDVI_eval_new", i, ".svg"), plot = g_b_ndvi[[i]],
         width = 150, height = 95, units = "mm")
}

#same for climate-models
AUC_diff_climate <- list()
AUC_test_climate <- list()
OR_10_climate <- list()
AICc_climate <- list()

for(i in seq_along(eval_models_climate)) {
  AUC_diff_climate[[i]] <- ggplot(eval_models_climate[[i]]@results, 
                               aes(x=rm, y=auc.diff.avg, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("AUC.diff") +
    theme_set(theme_bw()) +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(1,1),
          legend.position = c(1,1),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  
  AUC_test_climate[[i]] <- ggplot(eval_models_climate[[i]]@results, 
                               aes(x=rm, y=auc.val.avg, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("AUC.test") +
    theme_set(theme_bw()) +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(0,0),
          legend.position = c(0,0),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  
  OR_10_climate[[i]] <- ggplot(eval_models_climate[[i]]@results,
                            aes(x=rm, y=or.10p.avg, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("OR.10") +
    theme_set(theme_bw()) +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(1,1),
          legend.position = c(1,1),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  
  AICc_climate[[i]] <- ggplot(eval_models_climate[[i]]@results, 
                           aes(x=rm, y=AICc, group=fc, colour=fc))+
    geom_line() +
    geom_point() +
    scale_color_npg()+
    guides(colour=guide_legend(ncol=2))+
    xlab("Regularization multiplier") +
    ylab("AICc") +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.justification=c(1,0),
          legend.position = c(1,0),
          legend.key.size = unit(1, "line"),
          legend.text = element_text(size = 6),
          panel.grid = element_blank())
  plot_list_climate <- list(AUC_diff_climate[[i]], AUC_test_climate[[i]], OR_10_climate[[i]], AICc_climate[[i]])
}

g_b_climate <- list()

for(i in seq_along(AUC_diff_climate)) {
  g_b_climate[[i]] <- ggarrange(AUC_diff_climate[[i]], AUC_test_climate[[i]],
                                OR_10_climate[[i]], AICc_climate[[i]],
                                legend="none")
}

for(i in seq_along(g_b_climate)) {
  ggsave(paste0(path_eval, "climate_eval_new", i, ".svg"), plot = g_b_climate[[i]],
         width = 150, height = 95, units = "mm")
}


# select models  ----------------------------------------------

settings_ndvi10 <- "fc.LQ_rm.1"
settings_ndvi11 <- "fc.LQ_rm.4" 
settings_ndvi12 <- "fc.H_rm.3" 
settings_ndvi01 <- "fc.LQ_rm.4"
settings_ndvi02 <- "fc.LQ_rm.3"

settings_l_ndvi <- list(settings_ndvi10, settings_ndvi11, settings_ndvi12,
                           settings_ndvi01, settings_ndvi02)

settings_climate10 <- "fc.LQ_rm.3"
settings_climate11 <- "fc.LQ_rm.1"
settings_climate12 <- "fc.LQ_rm.1"
settings_climate01 <- "fc.LQ_rm.2"
settings_climate02 <- "fc.LQ_rm.4"

settings_l_climate <- list(settings_climate10, settings_climate11, settings_climate12,
                           settings_climate01, settings_climate02)

models_ndvi <- list()

for(i in seq_along(eval_models_ndvi)) {
  models_ndvi[[i]] <- eval_models_ndvi[[i]]@models [[settings_l_ndvi[[i]]]]
}

models_climate <- list()

for(i in seq_along(eval_models_climate)) {
  models_climate[[i]] <- eval_models_climate[[i]]@models [[settings_l_climate[[i]]]]
}


# get test metrics --------------------------------------------------------

test_ndvi <- list()
test_climate <- list()

for(i in seq_along(eval_models_ndvi)) {
  test_ndvi[[i]] <- eval_models_ndvi[[i]]@results[which(eval_models_ndvi[[i]]@results$tune.args == settings_l_ndvi[[i]]),] 
  test_ndvi[[i]]$model <- "ndvi"
  test_ndvi[[i]]$number <- i
}


for(i in seq_along(eval_models_climate)) {
  test_climate[[i]] <- eval_models_climate[[i]]@results[which(eval_models_climate[[i]]@results$tune.args == settings_l_climate[[i]]),] 
  test_climate[[i]]$model <- "climate"
  test_climate[[i]]$number <- i
}

test_ndvi_df <- do.call(rbind, test_ndvi)
test_clim_df <- do.call(rbind, test_climate)

test_both <- rbind(test_clim_df, test_ndvi_df)

month_labs <- c("Oktober", "November", "Dezember", "Januar", "Februar")

theme_set(theme_bw())
p_auc_val <- ggplot(test_both, aes(x = number, fill = model)) +
  geom_bar(aes(y = auc.val.avg), stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Monat",
                   limits = factor(c(1, 2, 3, 4, 5)),
                   labels = factor(month_labs))

p_auc_diff <- ggplot(test_both, aes(x = number, fill = model)) +
  geom_bar(aes(y = auc.diff.avg), stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Monat",
                   limits = factor(c(1, 2, 3, 4, 5)),
                   labels = factor(month_labs))

p_or10 <- ggplot(test_both, aes(x = number, fill = model)) +
  geom_bar(aes(y = or.10p.avg), stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Monat",
                   limits = factor(c(1, 2, 3, 4, 5)),
                   labels = factor(month_labs))

p_aicc <- ggplot(test_both, aes(x = number, fill = model)) +
  geom_bar(aes(y = AICc), stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Monat",
                   limits = factor(c(1, 2, 3, 4, 5)),
                   labels = factor(month_labs))

ggarrange(p_auc_val, p_auc_diff, p_or10, p_aicc, legend=FALSE)

# response curves ---------------------------------------------------------

dir.create(path = paste0(path_out_pop, "response_curves/"))
path_rc <- paste0(path_out_pop, "response_curves/")

rc_ndvi <- list()

for(i in seq_along(models_ndvi)) {
  svg(paste0(path_rc, "rc_ndvi", i, ".svg"), width=10)
  par(mfrow = c(4,4))
  rc_ndvi[[i]] <- dismo::response(models_ndvi[[i]], expand = 0, at=NULL)
  dev.off()
}

rc_climate <- list()

for(i in seq_along(models_climate)) {
  svg(paste0(path_rc, "rc_climate", i, ".svg"), width=10)
  par(mfrow = c(4,4))
  rc_climate[[i]] <- dismo::response(models_climate[[i]], expand = 0, at=NULL)
  dev.off()
}

# variable importance -----------------------------------------------------
dir.create(path = paste0(path_out_pop, "variable_importance/"))
path_vimp <- paste0(path_out_pop, "variable_importance/")

v_imp_ndvi <- list()
vimp_plot_ndvi <- list()
v_resh <- list()

theme_set(theme_bw())


for(i in seq_along(eval_models_ndvi)){
  v_imp_ndvi[[i]] <- eval_models_ndvi[[i]]@variable.importance[[settings_l_ndvi[[i]]]]%>%
    melt(id.vars = "variable") 
  colnames(v_imp_ndvi[[i]]) <- c("env", "var", "value")
  v_imp_ndvi[[i]] <- aggregate(. ~ env, v_imp_ndvi[[i]][-2], mean)
  v_imp_ndvi[[i]]$number <- i
  v_resh[[i]] <- v_imp_ndvi[[i]] %>% pivot_wider(names_from = c("env", "number"),
                                                 values_from = value)
  names(v_resh[[i]]) <- c("Entropy", "Homogeneity", "NDVI_c", "Variance",
                              "cultivated_cover", "dist2settlements",
                              "dist2water", "elevation", "herb_cover",
                              "prec_c", "shrub_cover", "tmax_c", "tree_cover")
  
  write.xlsx(v_imp_ndvi[[i]], paste0(path_vimp, "vimp_ndvi", i, ".xlsx"), 
             col.names = TRUE, row.names = FALSE, append = FALSE)
}

v_imp_ndvi_df <- do.call(rbind, v_resh)
v_imp_ndvi_df$month <- c("Oktober", "November", "Dezember", "Januar", "Februar")

vimp_ndvi_long <- v_imp_ndvi_df %>%  pivot_longer(cols=c(1:13))

ord <- c("cultivated_cover", "herb_cover", "dist2settlements", "NDVI_c",
         "elevation", "dist2water", "tree_cover", "tmax_c", "Variance",
         "Entropy", "prec_c", "shrub_cover", "Homogeneity")
ordm <- c("Oktober", "November", "Dezember", "Januar", "Februar")

vimp_ndvi_des <-vimp_ndvi_long %>% mutate(month=factor(month, ordm)) %>% 
  mutate(name=factor(name, ord))
         
theme_set(theme_bw())
imp_plot <- ggplot(vimp_ndvi_des) +
  geom_bar(aes(x=name, y=value), stat = "identity", width = 0.5) +
  facet_wrap(vars(month), ncol = 1) +
  xlab("Umweltvariable") +
  ylab("Wichtigkeit (%)") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave(plot = imp_plot, filename = paste0(path_vimp, "vimp_plot.png"), width = 20,
        height = 25, units = "cm")

v_imp_climate <- list()
vimp_plot_climate <- list()


for(i in seq_along(eval_models_climate)){
  v_imp_climate[[i]] <- eval_models_climate[[i]]@variable.importance[[settings_l_climate[[i]]]]%>%
    melt(id.vars = "variable")
  colnames(v_imp_climate[[i]]) <- c("env", "var", "value")
  v_imp_climate[[i]] <- aggregate(. ~ env, v_imp_climate[[i]][-2], mean)
  v_imp_climate[[i]]$number <- i
  
  write.xlsx(v_imp_climate[[i]], paste0(path_vimp, "vimp_climate", i, ".xlsx"), 
             col.names = TRUE, row.names = FALSE, append = FALSE)
  
  vimp_plot_climate[[i]] <- ggplot(v_imp_climate[[i]], aes(y = reorder(env, value), x = value)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, fill="darkblue") +
    xlab("Prozent") +
    theme(legend.title = element_blank(), 
          legend.position = c(0.8, 0.2),
          axis.title.y = element_blank())
  ggsave(paste0(path_vimp, "vimp_climate", i, ".tiff"), plot = vimp_plot_climate[[i]])
}


# predictions --------------------------------------------------------
dir.create(path = paste0(path_out_pop, "prediction_maps"))
path_preds <- paste0(path_out_pop, "prediction_maps/")

preds_ndvi <- list()
preds_climate <- list()

env_climate_list <- readRDS(paste0(path_out_pop, "env_climate_list"))
env_ndvi_list <- readRDS(paste0(path_out_pop, "env_ndvi_list"))

for(i in seq_along(models_ndvi)) {
  preds_ndvi[[i]] <- dismo::predict(object=models_ndvi[[i]], x=env_ndvi_list[[i]], type = 'cloglog', 
                        filename = paste0(path_preds, "preds_ndvi", i, ".tiff"))
}

for(i in seq_along(models_climate)) {
  preds_climate[[i]] <- dismo::predict(models_climate[[i]], env_climate_list[[i]], type = 'cloglog', 
                        filename = paste0(path_preds, "preds_climate", i, ".tiff"),
                        overwrite = TRUE)
}

saveRDS(preds_ndvi, paste0(path_preds,"preds_ndvi"))
saveRDS(preds_climate, paste0(path_preds, "preds_climate"))

preds_ndvi_stack <- stack(preds_ndvi)
levelplot(preds_ndvi_stack, par.settings="BuRdTheme")

# density of suitability scores at presence and background locations --------

monthl <- list("Oktober", "November", "Dezember", "Januar", "Februar")
monthc <- c("Oktober", "November", "Dezember", "Januar", "Februar")

values_ndvi <- list()
values_climate <- list()

for(i in seq_along(occ_ndvi_list)) {
  occ_ndvi_list[[i]]$type <- "Vorkommenspunkt"
  bg_ndvi_list[[i]]$type <- "Hintergerundpunkt"
  values_ndvi[[i]] <- rbind(occ_ndvi_list[[i]], bg_ndvi_list[[i]]) %>% as.data.frame()
  values_ndvi[[i]]$suitability <- raster::extract(preds_ndvi[[i]], values_ndvi[[i]][,1:2])
  values_ndvi[[i]]$month <- monthl[[i]]
  
  occ_climate_list[[i]]$type <- "Vorkommenspunkt"
  bg_climate_list[[i]]$type <- "Hintergerundpunkt"
  values_climate[[i]] <- rbind(occ_climate_list[[i]], bg_climate_list[[i]]) %>% as.data.frame()
  values_climate[[i]]$suitability <- raster::extract(preds_climate[[i]], values_climate[[i]][,1:2])
  values_climate[[i]]$month <- monthl[[i]]
}

suit_ndvi_df <- ldply(values_ndvi, data.frame) %>% select(c("type", "suitability", "month")) %>% 
  mutate(month =  factor(month, levels = monthc)) %>% arrange(month)  

suit_climate_df <- ldply(values_climate, data.frame) %>% select(c("type", "suitability", "month")) %>% 
  mutate(month =  factor(month, levels = monthc)) %>% arrange(month)  


#Plot density of occurence/bg points against suitability
dir.create(path = paste0(path_out_pop, "suitability_density"))
path_suit_density <- paste0(path_out_pop, "suitability_density/")
  
theme_set(theme_bw())
suit_density_ndvi <- ggplot() +
  geom_density(data = suit_ndvi_df, aes(fill = type, x = suitability), alpha = 0.5) +
  scale_y_continuous(limits = c(0,5)) +
  xlab("suitability index") +
  facet_wrap(vars(month)) +
  xlab("Eignungsindex") +
  ylab("Dichte") +
  labs(fill = "Datentyp") +
  theme(legend.position = c(0.8, 0.2))

suit_density_climate <- ggplot() +
  geom_density(data = suit_climate_df, aes(fill = type, x = suitability), alpha = 0.5) +
  scale_y_continuous(limits = c(0,5)) +
  xlab("suitability index") +
  facet_wrap(vars(month)) +
  xlab("Eignungsindex") +
  ylab("Dichte") +
  labs(fill = "Datentyp") +
  theme(legend.position = c(0.8, 0.2))

ggsave(suit_density_ndvi, filename = paste0(path_suit_density, "suit_dens_ndvi.png"))
ggsave(suit_density_climate, filename = paste0(path_suit_density, "suit_dens_climate.png"))


# null-models --------------------------------------------------------------

settings_climate_nulls <- list(data.frame(fc="LQ", rm=3),data.frame(fc="LQ", rm=1),
                               data.frame(fc="LQ", rm=1),data.frame(fc="LQ", rm=2),
                               data.frame(fc="LQ", rm=4)) 

settings_ndvi_nulls <- list(data.frame(fc="LQ", rm=1),data.frame(fc="LQ", rm=4),
                            data.frame(fc="H", rm=3),data.frame(fc="LQ", rm=4),
                            data.frame(fc="LQ", rm=3))


null_models_l_climate <- mapply(ENMnulls, eval_models_climate, mod.settings = settings_climate_nulls, no.iter = 50)
null_models_l_ndvi <- mapply(ENMnulls, eval_models_ndvi, mod.settings = settings_ndvi_nulls, no.iter = 50)

null_res_climate <- lapply(null_models_l_climate,null.emp.results)
null_res_ndvi <- lapply(null_models_l_ndvi, null.emp.results)

saveRDS(null_res_climate, paste0(path_out_pop, "null_res_climate"))
saveRDS(null_res_ndvi, paste0(path_out_pop, "null_res_ndvi"))

readRDS( paste0(path_out_pop, "null_res_ndvi"))

# binarize  population models---------------------------------------------------------------
dir.create(path = paste0(path_out_pop, "binarized_prediction_maps"))
path_bin <- paste0(path_out_pop, "binarized_prediction_maps/")

pred_vals_ndvi <- list()
pred_vals_climate <- list()
n10_ndvi <- list()
n10_climate <- list()
or_10_thr_ndvi <- list()
or_10_thr_climate <- list()
th10_ndvi <- list()
th10_climate <- list()

for(i in seq_along(preds_ndvi)){
  pred_vals_ndvi[[i]] <- raster::extract(preds_ndvi[[i]], occ_l[[i]])
  n10_ndvi[[i]] <- ceiling(length(pred_vals_ndvi[[i]]) * 0.1)
  or_10_thr_ndvi[[i]] <-  preds_ndvi[[i]] > sort(pred_vals_ndvi[[i]])[n10_ndvi[[i]]]
  th10_ndvi[[i]] <- sort(pred_vals_ndvi[[i]])[n10_ndvi[[i]]] #threshold
  bin_ndvi <- stack(or_10_thr_ndvi)
  writeRaster(or_10_thr_ndvi[[i]], paste0(path_bin, "bin_ndvi", i, ".tif"), overwrite=TRUE)
}


for(i in seq_along(preds_climate)){
  pred_vals_climate[[i]] <- raster::extract(preds_climate[[i]], occ_l[[i]])
  n10_climate[[i]] <- ceiling(length(pred_vals_climate[[i]]) * 0.1)
  or_10_thr_climate[[i]] <-  preds_climate[[i]] > sort(pred_vals_climate[[i]])[n10_climate[[i]]] 
  th10_climate[[i]] <- sort(pred_vals_climate[[i]])[n10_climate[[i]]] 
  bin_climate <- stack(or_10_thr_climate)
  writeRaster(or_10_thr_climate[[i]], paste0(path_bin, "bin_clim", i, ".tif"), overwrite=TRUE)
}

saveRDS(bin_climate, paste0(path_bin, "bin_climate"))
saveRDS(bin_ndvi, paste0(path_bin, "bin_ndvi"))

# load/calculate future climate data -----------------------

tmax85_full_path <- paste0(path_base_pop, "/RCP85/tmax_full")
tmax85_full_list <-list.files(path = tmax85_full_path, pattern = ".tif$", full.names = TRUE)
tmax85_full_rlist <- lapply(tmax85_full_list, raster) %>% lapply(crop,elev)
names(tmax85_full_rlist) <- c("tmax85_01", "tmax85_10", "tmax85_11", "tmax85_12", 
                              "tmax85_02", "tmax85_03", "tmax85_04", "tmax85_05",
                              "tmax85_06", "tmax85_07", "tmax85_08", "tmax85_09")

tmax85_full_stack <- stack(tmax85_full_rlist[["tmax85_01"]], tmax85_full_rlist[["tmax85_02"]], tmax85_full_rlist[["tmax85_03"]],
                           tmax85_full_rlist[["tmax85_04"]], tmax85_full_rlist[["tmax85_05"]], tmax85_full_rlist[["tmax85_06"]],
                           tmax85_full_rlist[["tmax85_07"]], tmax85_full_rlist[["tmax85_08"]], tmax85_full_rlist[["tmax85_09"]],
                           tmax85_full_rlist[["tmax85_10"]], tmax85_full_rlist[["tmax85_11"]], tmax85_full_rlist[["tmax85_12"]])

tmin85_full_path <- paste0(path_base_pop, "/RCP85/tmin_full")
tmin85_full_list <-list.files(path = tmin85_full_path, pattern = ".tif$", full.names = TRUE)
tmin85_full_rlist <- lapply(tmin85_full_list, raster) %>% lapply(crop,elev)
names(tmin85_full_rlist) <- c("tmin85_01", "tmin85_10", "tmin85_11", "tmin85_12", 
                              "tmin85_02", "tmin85_03", "tmin85_04", "tmin85_05",
                              "tmin85_06", "tmin85_07", "tmin85_08", "tmin85_09")

tmin85_full_stack <- stack(tmin85_full_rlist[["tmin85_01"]], tmin85_full_rlist[["tmin85_02"]], tmin85_full_rlist[["tmin85_03"]],
                           tmin85_full_rlist[["tmin85_04"]], tmin85_full_rlist[["tmin85_05"]], tmin85_full_rlist[["tmin85_06"]],
                           tmin85_full_rlist[["tmin85_07"]], tmin85_full_rlist[["tmin85_08"]], tmin85_full_rlist[["tmin85_09"]],
                           tmin85_full_rlist[["tmin85_10"]], tmin85_full_rlist[["tmin85_11"]], tmin85_full_rlist[["tmin85_12"]])

prec85_full_path <- paste0(path_base_pop, "/RCP85/prec_full")
prec85_full_list <-list.files(path = prec85_full_path, pattern = ".tif$", full.names = TRUE)
prec85_full_rlist <- lapply(prec85_full_list, raster) %>% lapply(crop,elev)
names(prec85_full_rlist) <- c("prec85_01", "prec85_10", "prec85_11", "prec85_12", 
                              "prec85_02", "prec85_03", "prec85_04", "prec85_05",
                              "prec85_06", "prec85_07", "prec85_08", "prec85_09")

prec85_full_stack <- stack(prec85_full_rlist[["prec85_01"]], prec85_full_rlist[["prec85_02"]], prec85_full_rlist[["prec85_03"]],
                           prec85_full_rlist[["prec85_04"]], prec85_full_rlist[["prec85_05"]], prec85_full_rlist[["prec85_06"]],
                           prec85_full_rlist[["prec85_07"]], prec85_full_rlist[["prec85_08"]], prec85_full_rlist[["prec85_09"]],
                           prec85_full_rlist[["prec85_10"]], prec85_full_rlist[["prec85_11"]], prec85_full_rlist[["prec85_12"]])


tmax45_full_path <- paste0(path_base_pop, "/RCP45/tmax_full")
tmax45_full_list <-list.files(path = tmax45_full_path, pattern = ".tif$", full.names = TRUE)
tmax45_full_rlist <- lapply(tmax45_full_list, raster) %>% lapply(crop,elev)
names(tmax45_full_rlist) <- c("tmax45_01", "tmax45_10", "tmax45_11", "tmax45_12", 
                              "tmax45_02", "tmax45_03", "tmax45_04", "tmax45_05",
                              "tmax45_06", "tmax45_07", "tmax45_08", "tmax45_09")

tmax45_full_stack <- stack(tmax45_full_rlist[["tmax45_01"]], tmax45_full_rlist[["tmax45_02"]], tmax45_full_rlist[["tmax45_03"]],
                           tmax45_full_rlist[["tmax45_04"]], tmax45_full_rlist[["tmax45_05"]], tmax45_full_rlist[["tmax45_06"]],
                           tmax45_full_rlist[["tmax45_07"]], tmax45_full_rlist[["tmax45_08"]], tmax45_full_rlist[["tmax45_09"]],
                           tmax45_full_rlist[["tmax45_10"]], tmax45_full_rlist[["tmax45_11"]], tmax45_full_rlist[["tmax45_12"]])

tmin45_full_path <- paste0(path_base_pop, "/RCP45/tmin_full")
tmin45_full_list <-list.files(path = tmin45_full_path, pattern = ".tif$", full.names = TRUE)
tmin45_full_rlist <- lapply(tmin45_full_list, raster) %>% lapply(crop,elev)
names(tmin45_full_rlist) <- c("tmin45_01", "tmin45_10", "tmin45_11", "tmin45_12", 
                              "tmin45_02", "tmin45_03", "tmin45_04", "tmin45_05",
                              "tmin45_06", "tmin45_07", "tmin45_08", "tmin45_09")

tmin45_full_stack <- stack(tmin45_full_rlist[["tmin45_01"]], tmin45_full_rlist[["tmin45_02"]], tmin45_full_rlist[["tmin45_03"]],
                           tmin45_full_rlist[["tmin45_04"]], tmin45_full_rlist[["tmin45_05"]], tmin45_full_rlist[["tmin45_06"]],
                           tmin45_full_rlist[["tmin45_07"]], tmin45_full_rlist[["tmin45_08"]], tmin45_full_rlist[["tmin45_09"]],
                           tmin45_full_rlist[["tmin45_10"]], tmin45_full_rlist[["tmin45_11"]], tmin45_full_rlist[["tmin45_12"]])

prec45_full_path <- paste0(path_base_pop, "/RCP45/prec_full")
prec45_full_list <-list.files(path = prec45_full_path, pattern = ".tif$", full.names = TRUE)
prec45_full_rlist <- lapply(prec45_full_list, raster) %>% lapply(crop,elev)
names(prec45_full_rlist) <- c("prec45_01", "prec45_10", "prec45_11", "prec45_12", 
                              "prec45_02", "prec45_03", "prec45_04", "prec45_05",
                              "prec45_06", "prec45_07", "prec45_08", "prec45_09")

prec45_full_stack <- stack(prec45_full_rlist[["prec45_01"]], prec45_full_rlist[["prec45_02"]], prec45_full_rlist[["prec45_03"]],
                           prec45_full_rlist[["prec45_04"]], prec45_full_rlist[["prec45_05"]], prec45_full_rlist[["prec45_06"]],
                           prec45_full_rlist[["prec45_07"]], prec45_full_rlist[["prec45_08"]], prec45_full_rlist[["prec45_09"]],
                           prec45_full_rlist[["prec45_10"]], prec45_full_rlist[["prec45_11"]], prec45_full_rlist[["prec45_12"]])



#calculate bio16
bio16_85 <- biovars(prec = prec85_full_stack, tmin = tmin85_full_stack, tmax = tmax85_full_stack)
bio16_85 <- bio16_85[["bio16"]]

bio16_45 <- biovars(prec = prec45_full_stack, tmin = tmin45_full_stack, tmax = tmax45_full_stack)
bio16_45 <- bio16_45[["bio16"]]

env45 <- stack(prec45_full_rlist[["prec45_10"]], prec45_full_rlist[["prec45_11"]],
               prec45_full_rlist[["prec45_12"]], prec45_full_rlist[["prec45_01"]],
               prec45_full_rlist[["prec45_02"]], tmax45_full_rlist[["tmax45_10"]],
               tmax45_full_rlist[["tmax45_11"]], tmax45_full_rlist[["tmax45_12"]],
               tmax45_full_rlist[["tmax45_01"]], tmax45_full_rlist[["tmax45_02"]],
               bio16_45)

env85 <- stack(prec85_full_rlist[["prec85_10"]], prec85_full_rlist[["prec85_11"]],
               prec85_full_rlist[["prec85_12"]], prec85_full_rlist[["prec85_01"]],
               prec85_full_rlist[["prec85_02"]], tmax85_full_rlist[["tmax85_10"]],
               tmax85_full_rlist[["tmax85_11"]], tmax85_full_rlist[["tmax85_12"]],
               tmax85_full_rlist[["tmax85_01"]], tmax85_full_rlist[["tmax85_02"]],
               bio16_85)

saveRDS(env45, paste0(path_out_pop, "env45"))
saveRDS(env85, paste0(path_out_pop,"env85"))


# combine future climate data with current data --------

env <- readRDS(paste0(path_out_pop, "env_climate_list"))
env45 <- readRDS(paste0(path_out_pop,"env45"))
env85 <- readRDS(paste0(path_out_pop,"env85"))

env45_10 <- stack(env45$hg45pr5010_UG, env45$hg45tx5010_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env45_10) <- names(env_climate_list[[1]])


env45_11 <- stack(env45$hg45pr5011_UG, env45$hg45tx5011_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env45_11) <- names(env_climate_list[[2]])


env45_12 <- stack(env45$hg45pr5012_UG, env45$hg45tx5012_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env45_12) <- names(env_climate_list[[3]])

env45_01 <- stack(env45$hg45pr501_UG, env45$hg45tx501_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env45_01) <- names(env_climate_list[[4]])

env45_02 <- stack(env45$hg45pr502_UG, env45$hg45tx502_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env45_02) <- names(env_climate_list[[5]])


env85_10 <- stack(env85$hg85pr5010_UG, env85$hg85tx5010_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env85_10) <- names(env_climate_list[[1]])


env85_11 <- stack(env85$hg85pr5011_UG, env85$hg85tx5011_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env85_11) <- names(env_climate_list[[2]])


env85_12 <- stack(env85$hg85pr5012_UG, env85$hg85tx5012_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env85_12) <- names(env_climate_list[[3]])

env85_01 <- stack(env85$hg85pr501_UG, env85$hg85tx501_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env85_01) <- names(env_climate_list[[4]])

env85_02 <- stack(env85$hg85pr502_UG, env85$hg85tx502_UG, env$tree_cover, 
                  env$shrub_cover, env$herb_cover, env$cultivated_cover, env$bio16,
                  env$dist2settlements, env$dist2water, env$elevation, env$Entropy,
                  env$Homogeneity, env$Variance)
names(env85_02) <- names(env_climate_list[[5]])


env45_list <- list(env45_10, env45_11, env45_12, env45_01, env45_02) 
env85_list <- list(env85_10, env85_11, env85_12, env85_01, env85_02)

saveRDS(env45_list, paste0(path_out_pop,"env45_list"))
saveRDS(env85_list, paste0(path_out_pop,"env85_list"))


# calculate MESS-maps -----------------------------------------------------

mess_45 <- list()
mess_85 <- list()
ref_points45 <- list()
ref_points85 <- list()

ref_points45 <- mapply(raster::extract, x=env_climate_list, y=occ_l)
ref_points85 <- mapply(raster::extract, x=env_climate_list, y=occ_l)

mess_45 <- mapply(mess, env45_list, ref_points85)
mess_85 <- mapply(mess, env85_list, ref_points85)

names(mess_45) <- c("Oktober", "November", "Dezember", "Januar", "Februar")
names(mess_85) <- c("Oktober", "November", "Dezember", "Januar", "Februar")

saveRDS(mess_45, paste0(path_out_pop,"mess_45"))
saveRDS(mess_85, paste0(path_out_pop,"mess_85"))


#Define color ramp

meanRad <- cellStats(mess_85[[1]], 'mean')
mess_85[[1]] <- mess_85[[1]] - meanRad
## Modify the palette with your colors
divPal <- brewer.pal(n=9, 'PuOr')
divPal[5] <- "#FFFFFF"
divTheme <- rasterTheme(region=divPal)

rng <- range(mess_85[[1]][])
## Number of desired intervals
nInt <- 15
## Increment corresponding to the range and nInt
inc0 <- diff(rng)/nInt
## Number of intervals from the negative extreme to zero
n0 <- floor(abs(rng[1])/inc0)
## Update the increment adding 1/2 to position zero in the center of an interval
inc <- abs(rng[1])/(n0 + 1/2)
## Number of intervals from zero to the positive extreme
n1 <- ceiling((rng[2]/inc - 1/2) + 1)
## Collection of breaks
breaks <- seq(rng[1], by=inc, length= n0 + 1 + n1)
## Midpoints computed with the median of each interval
idx <- findInterval(mess_85[[1]][], breaks, rightmost.closed=TRUE)
mids <- tapply(mess_85[[1]][], idx, median)
## Maximum of the absolute value both limits
mx <- max(abs(breaks))
## Interpolating function that maps colors with [0, 1]
## rgb(divRamp(0.5), maxColorValue=255) gives "#FFFFFF" (white)
break2pal <- function(x, mx, pal){
  ## x = mx gives y = 1
  ## x = 0 gives y = 0.5
  y <- 1/2*(x/mx + 1)
  rgb(pal(y), maxColorValue=255)
}
divRamp <- colorRamp(divPal)
## Diverging palette where white is associated with the interval
## containing the zero
pal <- break2pal(mids, mx, divRamp)

#Plot MESS maps
dir.create(path = paste0(path_out_pop, "mess_maps"))
path_mess <- paste0(path_out_pop, "mess_maps/")

png(paste0(path_mess, "MESS_85.png"), res = 1000, 
    width = 18, height = 12, units = "cm")
print(levelplot(stack(mess_85), par.settings = rasterTheme(region=pal), at=breaks))
dev.off()

plot(mess_85)

# predict the suitability to future climate -------------------------------------------
dir.create(path = paste0(path_out_pop, "prediction_maps_future"))
path_preds_future <- paste0(path_out_pop, "prediction_maps_future/")

env45_list <- readRDS(paste0(path_out_pop,"env45_list"))
env85_list <- readRDS(paste0(path_out_pop,"env85_list"))

rcp_45_preds <- list()
rcp_85_preds <- list()

for(i in seq_along(env45_list)){
  rcp_45_preds[[i]] <- predict(models_climate[[i]], env45_list[[i]], type = 'cloglog',
                          filename = paste0(path_preds_future, "preds_45_", i, ".tiff"), overwrite=TRUE)
}

for(i in seq_along(env85_list)){
  rcp_85_preds[[i]] <- predict(models_climate[[i]], env85_list[[i]], type = 'cloglog',
                               filename = paste0(path_preds_future, "preds_85_", i, ".tiff"), overwrite=TRUE)
}

saveRDS(rcp_45_preds, paste0(path_preds_future, "rcp_45_preds_neu"))
saveRDS(rcp_85_preds, paste0(path_preds_future, "rcp_85_preds_neu"))


# binarize the climatic projection maps-------------------------------------------------------------------------
dir.create(path = paste0(path_out_pop, "binarized_prediction_maps_future"))
path_rcp_recl <- paste0(path_out_pop, "binarized_prediction_maps_future/")

mat <- list()
recl_matrix <- list()
rcp45_recl <- list()
rcp85_recl <- list()


for(i in seq_along(th10_climate)) {
  mat[[i]] <- c(0, th10_climate[[i]], 0, th10_climate[[i]], 1, 1)
  recl_matrix[[i]] <- matrix(mat[[i]], ncol=3, byrow=TRUE)
  rcp45_recl[[i]] <- reclassify(rcp_45_preds[[i]], recl_matrix[[i]])
  rcp45_recl_stack <- stack(rcp45_recl)
  writeRaster(rcp45_recl[[i]], overwrite = TRUE,
              paste0(path_rcp_recl, "preds45_recl_", i, ".tif"))
}

for(i in seq_along(th10_climate)) {
  mat[[i]] <- c(0, th10_climate[[i]], 0, th10_climate[[i]], 1, 1)
  recl_matrix[[i]] <- matrix(mat[[i]], ncol=3, byrow=TRUE)
  rcp85_recl[[i]] <- reclassify(rcp_85_preds[[i]], recl_matrix[[i]])
  rcp85_recl_stack <- stack(rcp85_recl)
  writeRaster(rcp85_recl[[i]], overwrite = TRUE,
              paste0(path_rcp_recl, "preds85_recl_", i, ".tif"))
}


# analyze the future changes in habitat suitability ----------------------------------------------
dir.create(path = paste0(path_out_pop, "difference_current_future"))
path_diff <- paste0(path_out_pop, "difference_current_future/")

rcp45_dif <- (10*bin_climate)+rcp45_recl_stack #0=ungeeignet, 11=geeignet, 10=Verlust, 1=Gewinn,  
rcp45_dif_list <- unstack(rcp45_dif)

for (i in seq_along(rcp45_dif_list)) {
  writeRaster(rcp45_dif_list[[i]], overwrite = TRUE,
              paste0(path_diff, "diff_45", i, ".tif"))
}

rcp85_dif <- (10*bin_climate)+rcp85_recl_stack
rcp85_dif_list <- unstack(rcp85_dif)
for (i in seq_along(rcp45_dif_list)) {
  writeRaster(rcp85_dif_list[[i]], paste0(path_diff, "diff_85", i, ".tif"))
}

current_freq <- freq(bin_climate)
current_freq_dat <- lapply(current_freq, as.data.frame)

rcp45_freq <- freq(rcp45_recl_stack)
rcp45_freq_dat <- lapply(rcp45_freq, as.data.frame)

rcp85_freq <- freq(rcp85_recl_stack)
rcp85_freq_dat <- lapply(rcp85_freq, as.data.frame)

for(i in seq_along(rcp45_freq_dat)) {
  current_freq_dat[[i]]$number <- i
  rcp45_freq_dat[[i]]$number <- i
  rcp85_freq_dat[[i]]$number <- i
}

current_freq_df <- ldply(current_freq_dat, data.frame)
rcp45_freq_df <- ldply(rcp45_freq_dat, data.frame)
rcp85_freq_df <- ldply(rcp85_freq_dat, data.frame)

rcp_freq <- rbind(current_freq_df,rcp45_freq_df, rcp85_freq_df) %>% as.data.frame()

rcp_freq_diff_45_0 <- list()
rcp_freq_diff_45_1 <- list()
rcp_freq_diff_85_0 <- list()
rcp_freq_diff_85_1 <- list()
ls_names <- c("rcp_freq_diff_45_0", "rcp_freq_diff_45_1", 
              "rcp_freq_diff_85_0", "rcp_freq_diff_85_1")

for(i in seq_along(current_freq)){
  rcp_freq_diff_45_0[[i]] <- (current_freq[[i]][[1,2]]-rcp45_freq[[i]][[1,2]])/current_freq[[i]][[1,2]]*-100
  rcp_freq_diff_45_1[[i]] <- (current_freq[[i]][[2,2]]-rcp45_freq[[i]][[2,2]])/current_freq[[i]][[2,2]]*-100
  rcp_freq_diff_85_0[[i]] <- (current_freq[[i]][[1,2]]-rcp85_freq[[i]][[1,2]])/current_freq[[i]][[1,2]]*-100
  rcp_freq_diff_85_1[[i]] <- (current_freq[[i]][[2,2]]-rcp85_freq[[i]][[2,2]])/current_freq[[i]][[2,2]]*-100
}

change_perc <- do.call(mapply, c(FUN=c, sapply(ls_names, as.symbol), SIMPLIFY=FALSE))
change_perc
