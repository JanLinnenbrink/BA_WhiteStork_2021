library(reshape2)
library(purrr)
library(ggplot2)
library(ggsci)
library(rlist)
library(ggpubr)
library(gridExtra)
library(grid)
library(rgdal)
library(plyr)
library(reshape2)
library(maxnet)
library(dismo)
library(ENMeval)
library(iSDM)
library(ggplotify)
library(amt)
library(move)
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
library(spdplyr)
library(sf)

crs <- CRS("+init=epsg:4329")

set.seed(28)

#Dateipfad der Umweltvariablen
path_env <- "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/Environmental_Variables_Ind"

#Ausgabedateipfad für die Vorkommensdaten des Individuenmodells
path_occs_ind <- "C:/1_Bachelorarbeit/Statistik/Diagramme/Ind_Model/occs/"

#Ausgabedateipfad für die binarisierten Individuenmodelle
path_bin_ind <- "C:/1_Bachelorarbeit/Statistik/Diagramme/Ind_Model/bin_ind/"

#Ausgabedateipfad für die Vergleichskarten der realisierten und der potenziellen Verbreitung
path_realized_pot <- "C:/1_Bachelorarbeit/Statistik/Diagramme/realized_potential/"

# Vorkommensdaten vorbereiten ---------------------------------------------


#Spalten, die noch benötigt werden (für ef unterschiedlich zum Rest)

keep_cols_gps_ef <-
  c(
    "timestamp",
    "location_long",
    "location_lat",
    "ind_name"
  )

keep_cols_gps <-
  c(
    "timestamp",
    "location.long",
    "location.lat",
    "ind_name"
  )

keep_cols_argos <-
  c(
    "timestamp",
    "location.long",
    "location.lat",
    "ind_name",
    "argos.lc"
  )

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

crs(UG_sp) <- crs

UG_sf <- st_as_sf(UG_sp)

thin_distance <- 1000 # meters
thin_time <- 120

threshold_min_n <- 40 # Minimal occurence data/Individuum/month

login <- movebankLogin(username = "JanL", password = "Eiche123!")

t1 <-
  strptime("20140901000000", format = "%Y%m%d%H%M%S", tz = 'UTC')
t2 <-
  strptime("20150301000000", format = "%Y%m%d%H%M%S", tz = 'UTC')

# Download movebank data, only keep Argos data with estimate error < 1km

Eastern_flyway <-
  getMovebankData(
    study = "Eastern flyway spring migration of adult white storks (data from Rotics et al. 2018)",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

ef_df <- as.data.frame(Eastern_flyway)

n.storks <- length(unique(ef_df$individual_id))
ind_name <- paste0("EAS", 1:n.storks)
ef_df$ind_name <-
  factor(ef_df$individual_id, labels = ind_name)


ef_df <- ef_df[, (names(ef_df) %in% keep_cols_gps_ef)]

Bavaria <-
  getMovebankLocationData(
    study = "LifeTrack White Stork Bavaria",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

b_df <- as.data.frame(Bavaria)

n.storks <- length(unique(b_df$individual.id))
ind_name <- paste0("BAV", 1:n.storks)
b_df$ind_name <-
  factor(b_df$individual.id, labels = ind_name)

b_df <- b_df[, (names(b_df) %in% keep_cols_gps)]


Greece <-
  getMovebankLocationData(
    study = "LifeTrack White Stork Greece Evros Delta",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

g_df <- as.data.frame(Greece)

n.storks <- length(unique(g_df$individual.id))
ind_name <- paste0("GRE", 1:n.storks)
g_df$ind_name <-
  factor(g_df$individual.id, labels = ind_name)

g_df <- g_df[, (names(g_df) %in% keep_cols_gps)]


Loburg <-
  getMovebankLocationData(
    study = "LifeTrack White Stork Loburg",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

l_df <- as.data.frame(Loburg)

n.storks <- length(unique(l_df$individual.id))
ind_name <- paste0("LOB", 1:n.storks)
l_df$ind_name <-
  factor(l_df$individual.id, labels = ind_name)

l_df <- l_df[, (names(l_df) %in% keep_cols_gps)]


Moscow <-
  getMovebankLocationData(
    study = "LifeTrack White Stork Moscow",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

m_df <- as.data.frame(Moscow)

n.storks <- length(unique(m_df$individual.id))
ind_name <- paste0("MOS", 1:n.storks)
m_df$ind_name <-
  factor(m_df$individual.id, labels = ind_name)

m_df <- m_df[, (names(m_df) %in% keep_cols_gps)]


Poland <-
  getMovebankLocationData(
    study = "LifeTrack White Stork Poland ECG",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

p_df <- as.data.frame(Poland)

n.storks <- length(unique(p_df$individual.id))
ind_name <- paste0("POL", 1:n.storks)
p_df$ind_name <-
  factor(p_df$individual.id, labels = ind_name)

p_df <- p_df[, (names(p_df) %in% keep_cols_gps)]


MPIAB_gps <-
  getMovebankLocationData(
    study = "MPIAB Argos white stork tracking (1991-2018)",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653 #gps
  )

Mg_df <- as.data.frame(MPIAB_gps)

n.storks <- length(unique(Mg_df$individual.id))
ind_name <- paste0("MPg", 1:n.storks)
Mg_df$ind_name <-
  factor(Mg_df$individual.id, labels = ind_name)

Mg_df <- Mg_df[, (names(Mg_df) %in% keep_cols_gps)]


MPIAB_argos <-
  getMovebankLocationData(
    study = "MPIAB Argos white stork tracking (1991-2018)",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 82798 #argos-doppler-shift
  )

Ma_df <- as.data.frame(MPIAB_argos)

n.storks <- length(unique(Ma_df$individual.id))
ind_name <- paste0("MPa", 1:n.storks)
Ma_df$ind_name <-
  factor(Ma_df$individual.id, labels = ind_name)

Ma_df <- Ma_df[, (names(Ma_df) %in% keep_cols_argos)]

Ma_df <-
  Ma_df[Ma_df$argos.lc %in% c(3, 2, 1), ] # estimate error <= 1km

Ma_df$argos.lc <- NULL


Bergenhusen_gps <-
  getMovebankLocationData(
    study = "NABU_Bergenhusen",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 653
  )

Bg_df <- as.data.frame(Bergenhusen_gps)

n.storks <- length(unique(Bg_df$individual.id))
ind_name <- paste0("BEg", 1:n.storks)
Bg_df$ind_name <-
  factor(Bg_df$individual.id, labels = ind_name)

Bg_df <- Bg_df[, (names(Bg_df) %in% keep_cols_gps)]


Bergenhusen_argos <-
  getMovebankLocationData(
    study = "NABU_Bergenhusen",
    login = login,
    timestamp_start = t1,
    timestamp_end = t2,
    removeDuplicatedTimestamps = TRUE,
    sensorID = 82798
  )

Ba_df <- as.data.frame(Bergenhusen_argos)

n.storks <- length(unique(Ba_df$individual.id))
ind_name <- paste0("BEa", 1:n.storks)
Ba_df$ind_name <-
  factor(Ba_df$individual.id, labels = ind_name)

Ba_df <- Ba_df[, (names(Ba_df) %in% keep_cols_argos)]

Ba_df <-
  Ba_df[Ba_df$argos.lc %in% c(3, 2, 1), ] # estimate error <= 1km

Ba_df$argos.lc <- NULL


# Merge all TD into one data.frame

TD <-
  b_df %>%
  full_join(Bg_df) %>%
  full_join(Ba_df) %>%
  full_join(ef_df) %>%
  full_join(g_df) %>%
  full_join(l_df) %>%
  full_join(m_df) %>%
  full_join(Ma_df) %>%
  full_join(Mg_df) %>%
  full_join(p_df)

# Clip to UG 

TD$inUG <- factor(point.in.polygon(
  TD$location.long,
  TD$location.lat,
  UG$x,
  UG$y
))

TD_UG <- filter(TD, TD$inUG == 1)
TD_UG$inUG <- NULL

TD_UG$month <- month(TD_UG$timestamp)
TD_10_3 <- TD_UG %>% filter(month != 9)

n_UG <-
  TD_10_3 %>%
  group_by(ind_name, month) %>%
  dplyr::summarise(Freq = n())
colnames(n_UG) <- c("ind_id", "month", "Freq")


# Remove all locations from night except one 

TD_amt <-
  make_track(
    TD_10_3,
    .x = location.long,
    .y = location.lat,
    .t = timestamp,
    ind.id = ind_name,
    month = month,
    crs = crs
  )

TD_track <- time_of_day(TD_amt)

TD_track$day <- substr(x = TD_track$t_, start = 0, stop = 10)

TD_night <- TD_track %>%
  filter(tod_ == "night") %>%
  group_by(ind.id, day) %>%
  slice(1)

TD_day <- TD_track %>%
  filter(tod_ == "day")

TD_track <- full_join(TD_day, TD_night) %>%
  select(-day)

n_dn <- TD_track %>%
  group_by(ind.id, month) %>%
  dplyr::summarise(Freq = n())
colnames(n_dn) <- c("ind_id", "month", "Freq")


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

names(TD_track_tempFilter) <-
  c("lng", "lat", "time", "ind_id", "month")

TD_g <-
  TD_track_tempFilter %>% split(list(TD_track_tempFilter$ind_id, TD_track_tempFilter$month),
                                drop = TRUE
  )

spatialFilter <- function(df, dist) {
  coordinates(df) <- ~ lng + lat
  crs(df) <- crs
  df_tr <- spTransform(df, crs_new)
  TD_sT2 <-
    remove.near(df_tr, dist = dist)
}

TD_SpT <- lapply(TD_g, spatialFilter, dist = thin_distance)

TD_SpT_SPDF <- do.call(rbind.SpatialPointsDataFrame, TD_SpT)

#Schwellenwert von 40 als minimale Anzahl an Ortungspunkten

TD_threshold <-
  TD_SpT_SPDF %>%
  as.data.frame() %>%
  group_by(ind_id, month) %>%
  filter(n() >= threshold_min_n)

#remove not existing levels of ind_id
TD_threshold$ind_id <- factor(TD_threshold$ind_id)


n_spatiotemporalthinning <-
  TD_threshold %>%
  as.data.frame() %>%
  group_by(ind_id, month) %>%
  dplyr::summarise(Freq = n())

TD_inG <-
  n_spatiotemporalthinning %>%
  group_by(ind_id) %>%
  filter(n() == 5) # only individuals with data from all month

TD_threshold <-
  subset(TD_threshold, TD_threshold$ind_id %in% TD_inG$ind_id)

# Subsample the location data by the minimum number of location data for each individual 

n_min <- min(n_spatiotemporalthinning[, 3])

#MOS4 und POL4 haben nicht in allen Monaten > 40 Nachweise (-> Entfernen)

TD_threshold_wo <- TD_threshold[TD_threshold$ind_id != "MOS4" 
                                & TD_threshold$ind_id != "POL4",]

TD_samples <-
  TD_threshold_wo %>%
  as.data.frame() %>%
  group_by(ind_id, month) %>%
  sample_n(n_min) %>% 
  as.data.frame()
TD_samples$ind_id <- factor(TD_samples$ind_id)

coordinates(TD_samples) <- ~ lng + lat
crs(TD_samples) <- crs_new

TD_samples_shp <-
  writeOGR(
    obj = TD_samples,
    overwrite_layer = TRUE,
    dsn = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/SHP",
    layer = paste0("occ_", n_min),
    driver = "ESRI Shapefile"
  )

saveRDS(TD_samples, file = "TD_train_ind.rds")

# prepare occurence data and sample background data --------------------------------------------------

occ_sp_ind <- TD_samples %>% spTransform(crs)

occ_df_ind <- as.data.frame(occ_sp_ind)

occ_10_ind <- occ_df_ind %>% dplyr::filter(month == 10)
occ_11_ind <- occ_df_ind %>% dplyr::filter(month == 11)
occ_12_ind <- occ_df_ind %>% dplyr::filter(month == 12)
occ_01_ind <- occ_df_ind %>% dplyr::filter(month == 01)
occ_02_ind <- occ_df_ind %>% dplyr::filter(month == 02)


#Nach Individuen aufteilen
occ_10_ind_l <- occ_10_ind %>% split(f=occ_10_ind$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_11_ind_l <- occ_11_ind %>% split(f=occ_11_ind$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_12_ind_l <- occ_12_ind %>% split(f=occ_12_ind$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_01_ind_l <- occ_01_ind %>% split(f=occ_01_ind$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_02_ind_l <- occ_02_ind %>% split(f=occ_02_ind$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)


# prepare environmental variables -----------------------------------------

LSM_float_path <- paste0(path_env, "/LSM_float")
LSM_float_list <-list.files(path = LSM_float_path, pattern = ".tif$", full.names = TRUE)
LSM_float_rlist <- lapply(LSM_float_list, raster)
names(LSM_float_rlist) <- c("CoV", "Entropy", "Evenness", "Homogeneity")

LSM_movingW <- raster(paste0(path_env, "/LSM_cat/moving_window_44.tif"), extent = extent)
names(LSM_movingW) <- "Variance"

weather_path <- paste0(path_env, "/Weather")
weather_list <- list.files(path=weather_path, pattern = ".tif$", full.names = TRUE)
weather_rlist <- lapply(weather_list, raster)
names(weather_rlist) <- c("prec_10", "prec_11", "prec_12", "prec_01", "prec_02", "prec_03",
                          "tmax_10", "tmax_11", "tmax_12", "tmax_01", "tmax_02", "tmax_03",
                          "tmin_10", "tmin_11", "tmin_12", "tmin_01", "tmin_02", "tmin_03")

elev <- raster(paste0(path_env, "/elevation/elev_UG.tif"))
names(elev) <- "elevation"

settl_dist <- raster(paste0(path_env, "/Settlements/Settlements_distance.tif"))
names(settl_dist) <- "dist2settlements"

water_dist <- raster(paste0(path_env, "/Water_new/dist2water.tif"))
names(water_dist) <- "dist2water"

LandCov_path <- paste0(path_env, "/LandCover_float")
LandCov_list <- list.files(path=LandCov_path, pattern = ".tif$", full.names = TRUE)
LandCov_rlist <- lapply(LandCov_list, raster)
names(LandCov_rlist) <- c("tree_cover","shrub_cover","herb_cover","cultivated_cover")

NDVI_w_path <- paste0(path_env, "/NDVI_weather")
NDVI_w_list <- list.files(path=NDVI_w_path, pattern = ".tif$", full.names = TRUE)
NDVI_w_rlist <- lapply(NDVI_w_list, raster)
names(NDVI_w_rlist) <- c("NDVI_10", "NDVI_11", "NDVI_12", "NDVI_01", "NDVI_02","NDVI_03")


#Maerz entfernen

LSM_float_rlist_SDM <- LSM_float_rlist[c("Entropy", "Homogeneity")]

weather_rlist_SDM <- weather_rlist[c("prec_10", "prec_11", "prec_12", "prec_01", "prec_02",
                                     "tmax_10", "tmax_11", "tmax_12", "tmax_01", "tmax_02", 
                                     "tmin_10", "tmin_11", "tmin_12", "tmin_01", "tmin_02")]

NDVI_w_rlist_SDM <- NDVI_w_rlist[c(1:5)]

#Listen in Raster Stacks konvertieren

LSM_F_stack_SDM <- stack(LSM_float_rlist_SDM)
weather_stack_SDM <- stack(weather_rlist_SDM)
NDVI_w_stack_SDM <- stack(NDVI_w_rlist_SDM)
LandCov_stack_SDM <- stack(LandCov_rlist)


#Listen der EnvVariablen erstellen, ohne "tmin_n", da nur indirekter Einfluss

env10l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_10", "tmax_10")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_10"))
env11l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_11", "tmax_11")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_11"))
env12l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_12", "tmax_12")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_12"))
env01l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_01", "tmax_01")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_01"))
env02l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_02", "tmax_02")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_02"))


#Raster auf Aufloesung der Wetterdaten resamplen

envl <- list(env10l, env11l, env12l, env01l, env02l)

env10re <- lapply(env10l, raster::resample, weather_rlist_SDM[[1]], "bilinear")
env11re <- lapply(env11l, raster::resample, weather_rlist_SDM[[1]], "bilinear")
env12re <- lapply(env12l, raster::resample, weather_rlist_SDM[[1]], "bilinear")
env01re <- lapply(env01l, raster::resample, weather_rlist_SDM[[1]], "bilinear")
env02re <- lapply(env02l, raster::resample, weather_rlist_SDM[[1]], "bilinear")

#Raster stacks erstellen

env10 <- stack(env10re)
env11 <- stack(env11re)
env12 <- stack(env12re)
env01 <- stack(env01re)
env02 <- stack(env02re)


# Backround Points generieren ---------------------------------------------

env_bg_individual <- env10[[1]]
bp_ind <- randomPoints(env_bg_individual, 10000)
colnames(bp_ind) <- c("lng", "lat")

# Dispersal Kernel berechnen ----------------------------------------------

c10 <- lapply(occ_10_ind_l, as.matrix)
c11 <- lapply(occ_11_ind_l, as.matrix)
c12 <- lapply(occ_12_ind_l, as.matrix)
c01 <- lapply(occ_01_ind_l, as.matrix)
c02 <- lapply(occ_02_ind_l, as.matrix)

odf <- as.data.frame(rep(1,42))

a <- 0.1

dispK_10 <- lapply(c10, iForce, occData=odf, envData=weather_stack_SDM[[1]], a=a, binary=FALSE, longlat=TRUE)
dispK_11 <- lapply(c11, iForce, occData=odf, envData=weather_stack_SDM[[1]], a=a, binary=FALSE, longlat=TRUE)
dispK_12 <- lapply(c12, iForce, occData=odf, envData=weather_stack_SDM[[1]], a=a, binary=FALSE, longlat=TRUE)
dispK_01 <- lapply(c01, iForce, occData=odf, envData=weather_stack_SDM[[1]], a=a, binary=FALSE, longlat=TRUE)
dispK_02 <- lapply(c02, iForce, occData=odf, envData=weather_stack_SDM[[1]], a=a, binary=FALSE, longlat=TRUE)

dk_list <- list(dispK_10, dispK_11, dispK_12, dispK_01, dispK_02)

dk_stack <- lapply(dk_list, stack)

#Berechne den monatlichen Dispersalkernel für alle Individuen

dk_max <- lapply(dk_stack, FUN=calc, function(x) max(x, na.rm = TRUE)) 

dk_names <- c("DK_10", "DK_11", "DK_12", "DK_01", "DK_02")
names(dk_max) <- dk_names


#Binde den DK an die Umweltvariablen an

env10_dk <- addLayer(env10, dk_max[["DK_10"]])
env11_dk <- addLayer(env11, dk_max[["DK_11"]])
env12_dk <- addLayer(env12, dk_max[["DK_12"]])
env01_dk <- addLayer(env01, dk_max[["DK_01"]])
env02_dk <- addLayer(env02, dk_max[["DK_02"]])

env_list_ind <- list(env10_dk, env11_dk, env12_dk, env01_dk, env02_dk)

# Binde die Umweltvariablen an die Präsenz- und Hintergrundpunkte an ---------

occ_list_ind <- list(occ_10_ind[,4:5], occ_11_ind[,4:5], occ_12_ind[,4:5],
                     occ_01_ind[,4:5], occ_02_ind[,4:5])
names(occ_list) <- c("occ10", "occ11", "occ12", "occ01", "occ02")

occ_env_ind <- list()
bp_env_ind <- list()

for (i in seq_along(occ_list_ind)) {
  occ_env_ind[[i]] <- as.data.frame(cbind(occ_list_ind[[i]],
                                          raster::extract(env_list_ind[[i]], occ_list_ind[[i]])))
  bp_env_ind[[i]] <- as.data.frame(cbind(bp_ind, raster::extract(env_list_ind[[i]], bp_ind)))
}


# model calibration -------------------------------------------------------

fc_s <- "LQ"
rm_s <- 0.5

tune.args <- list(fc = fc_s, rm = rm_s)

i_model_a001 <- list()

for(i in seq_along(occ_env)){
  i_model_a001[[i]] <- ENMevaluate(occs = occ_env_ind[[i]], bg = bp_env_ind[[i]],
                                   algorithm = "maxent.jar", tune.args = tune.args, 
                                   partitions = "block", updateProgress = TRUE,
                                   numCores = NULL)
}
saveRDS(i_model_a001, "ind_model_LQ")

results_ind <- list()

for (i in seq_along(i_model_a001)) {
  results_ind[[i]] <- i_model_a001[[i]]@results
}

models_ind <- list()

for (i in seq_along(i_model_a001)) {
  models_ind[[i]] <- i_model_a001[[i]]@models[["fc.LQ_rm.0.5"]]
}

realized_distr_pred <- mapply(predict, env_list, models_ind, type = "cloglog")
realized_stack <- stack(realized_distr_pred)

saveRDS(realized_stack, "realized_distribution_LQ")

# Nullmodelle -------------------------------------------------------------

null_models_ind <- lapply(i_model_a001, ENMnulls, mod.settings=tune.args, no.iter = 50)


# Binarize der realisierten Verbreitung ----------------------------------------------------------------

realized_distr <- readRDS("realized_distribution") %>% as.list() 
potential_distr <- readRDS("bin_ndvi") %>% as.list()

occ_l_ind <- list(occ_10_ind, occ_11_ind, occ_12_ind, occ_01_ind, occ_02_ind) %>% 
  lapply(select, lng, lat)

#Vorkommenspunkte als Shapefile exportieren (Kartendarstellung)

occ_l_spat <- list()

for(i in seq_along(occ_l_ind)) {
  occ_l_spat[[i]] <- st_as_sf(x = occ_l_ind[[i]], 
                         coords = c("lng", "lat"),
                         crs = crs)
  st_write(occ_l_spat[[i]], paste0(path_occs_ind, "occs_ind_", i, ".shp"))
} 

#Binarize

pred_vals_ind <- list()
n10_ind <- list()
or_10_thr_ind <- list()
th10_ind <- list()

for(i in seq_along(realized_distr_pred)) {
  realized_distr_pred[[i]][is.na(realized_distr_pred[[i]]) <- 0]
  pred_vals_ind[[i]] <- raster::extract(realized_distr_pred[[i]], occ_l_ind[[i]])
  n10_ind[[i]] <- ceiling(length(pred_vals_ind[[i]]) * 0.4)
  or_10_thr_ind[[i]] <-  realized_distr_pred[[i]] > sort(pred_vals_ind[[i]])[n10_ind[[i]]]
  th10_ind[[i]] <- sort(pred_vals_ind[[i]])[n10_ind[[i]]] #Schwellenwert
  bin_ind <- stack(or_10_thr_ind)
  writeRaster(or_10_thr_ind[[i]], paste0(path_bin_ind, "bin_ind", i, ".tif"), overwrite=TRUE)
}

plot(bin_ind[[1]])
points(occ_l_ind[[1]])

saveRDS(bin_ind, "bin_ind")


# Unterschied potenzielle - realisierte Verbreitung -----------------------------------------------------

bin_pop <- readRDS("bin_ndvi") %>% resample(bin_ind, method="ngb")


realized_potential <- (10*bin_pop)-bin_ind #0=none, 9=both, 10=potential, -1=realized 
realized_potential_list <- unstack(relized_potential)

for (i in seq_along(relized_potential_list)) {
  writeRaster(realized_potential_list[[i]], overwrite = TRUE,
              paste0(path_realized_pot, "realized_pot_", i, ".tif"))
}

