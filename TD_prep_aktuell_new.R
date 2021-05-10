library(amt)
library(move)
library(rgdal)
library(lubridate)
library(maptools)
library(sf)
library(sp)
library(spThin)
library(dplyr)
library(data.table)
library(spatialEco)
library(PBSmapping)
library(tidyverse)
library(raster)
library(ecoinfo)


keep_cols_gps <-
  c(
    "event.id",
    "timestamp",
    "location.long",
    "location.lat",
    "utm.easting",
    "utm.northing",
    "utm.zone",
    "ind_name"
  )

keep_cols_argos <-
  c(
    "event.id",
    "timestamp",
    "location.long",
    "location.lat",
    "individual.id",
    "utm.easting",
    "utm.northing",
    "utm.zone",
    "ind_name",
    "argos.lc"
  )

crs <- CRS("+init=epsg:4326")

thin_distance <- 1 #kilometres


# Download movebank data, only keep Argos data with estimate error < 1km-----------------------------------------------

Eastern_flyway <-
  move(x = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/Eastern flyway spring migration of adult white storks (data from Rotics et al. 2018).csv")

ef_df <- as.data.frame(Eastern_flyway)

n.storks <- length(unique(ef_df$individual.local.identifier))
ind_name <- paste0("EF", 1:n.storks)
ef_df$ind_name <-
  factor(ef_df$individual.local.identifier, labels = ind_name)

ef_df <- ef_df[, (names(ef_df) %in% keep_cols_gps)]


Bavaria <-
  move(x = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Bavaria.csv")

b_df <- as.data.frame(Bavaria)

n.storks <- length(unique(b_df$individual.local.identifier))
ind_name <- paste0("B", 1:n.storks)
b_df$ind_name <-
  factor(b_df$individual.local.identifier, labels = ind_name)

b_df <- b_df[, (names(b_df) %in% keep_cols_gps)]

Greece <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Greece Evros Delta .csv"
  )

g_df <- as.data.frame(Greece)

n.storks <- length(unique(g_df$individual.local.identifier))
ind_name <- paste0("S", 1:n.storks)
g_df$ind_name <-
  factor(g_df$individual.local.identifier, labels = ind_name)

g_df <- g_df[, (names(g_df) %in% keep_cols_gps)]


Loburg <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Loburg.csv",
    removeDuplicatedTimestamps = TRUE
  )

l_df <- as.data.frame(Loburg)

n.storks <- length(unique(l_df$individual.local.identifier))
ind_name <- paste0("L", 1:n.storks)
l_df$ind_name <-
  factor(l_df$individual.local.identifier, labels = ind_name)

l_df <- l_df[, (names(l_df) %in% keep_cols_gps)]

Moscow <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Moscow.csv",
    removeDuplicatedTimestamps = TRUE
  )

m_df <- as.data.frame(Moscow)

n.storks <- length(unique(m_df$individual.local.identifier))
ind_name <- paste0("M", 1:n.storks)
m_df$ind_name <-
  factor(m_df$individual.local.identifier, labels = ind_name)

m_df <- m_df[, (names(m_df) %in% keep_cols_gps)]


Poland <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Poland.csv"
  )

p_df <- as.data.frame(Poland)

n.storks <- length(unique(p_df$individual.local.identifier))
ind_name <- paste0("P", 1:n.storks)
p_df$ind_name <-
  factor(p_df$individual.local.identifier, labels = ind_name)

p_df <- p_df[, (names(p_df) %in% keep_cols_gps)]


MPIAB_gps <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/MPIAB Argos white stork tracking (1991-2018).csv"
  )

Mg_df <- as.data.frame(MPIAB_gps)

n.storks <- length(unique(Mg_df$individual.local.identifier))
ind_name <- paste0("Mg", 1:n.storks)
Mg_df$ind_name <-
  factor(Mg_df$individual.local.identifier, labels = ind_name)

Mg_df <- Mg_df[, (names(Mg_df) %in% keep_cols_gps)]


MPIAB_argos <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/MPIAB Argos white stork tracking (1991-2018) (1).csv"
  )

Ma_df <- as.data.frame(MPIAB_argos)

n.storks <- length(unique(Ma_df$individual.local.identifier))
ind_name <- paste0("Ma", 1:n.storks)
Ma_df$ind_name <-
  factor(Ma_df$individual.local.identifier, labels = ind_name)

Ma_df <- Ma_df[, (names(Ma_df) %in% keep_cols_argos)]

Ma_df <-
  Ma_df[Ma_df$argos.lc %in% c(3, 2, 1),] #estimate error <= 1km

Ma_df$argos.lc <- NULL


Bergenhusen_gps <-
  move("C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/NABU_Bergenhusen.csv")

Bg_df <- as.data.frame(Bergenhusen_gps)

n.storks <- length(unique(Bg_df$individual.local.identifier))
ind_name <- paste0("Bg", 1:n.storks)
Bg_df$ind_name <-
  factor(Bg_df$individual.local.identifier, labels = ind_name)

Bg_df <- Bg_df[, (names(Bg_df) %in% keep_cols_gps)]


Bergenhusen_argos <-
  move("C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/NABU_Bergenhusen (1).csv")

Ba_df <- as.data.frame(Bergenhusen_argos)

n.storks <- length(unique(Ba_df$individual.local.identifier))
ind_name <- paste0("Ba", 1:n.storks)
Ba_df$ind_name <-
  factor(Ba_df$individual.local.identifier, labels = ind_name)

Ba_df <- Ba_df[, (names(Ba_df) %in% keep_cols_argos)]

Ba_df <-
  Ba_df[Ba_df$argos.lc %in% c(3, 2, 1),] #estimate error <= 1km

Ba_df$argos.lc <- NULL

rm(
  list = c(
    "ind_name",
    "n.storks",
    "Bergenhusen_argos",
    "Bergenhusen_gps",
    "Bavaria",
    "Eastern_flyway",
    "Greece",
    "Loburg",
    "Moscow",
    "MPIAB_argos",
    "MPIAB_gps",
    "Poland"
  )
)


# Merge all TD into one data.frame ----------------------------------------

TD  <-
  b_df %>% full_join(Bg_df) %>% full_join(Ba_df) %>% full_join(ef_df) %>% full_join(g_df) %>%
  full_join(l_df) %>% full_join(m_df) %>% full_join(Ma_df) %>% full_join(Mg_df) %>%
  full_join(p_df)


# Clip to UG ------------------------------------------------------

UG <- data.frame(lat = c(17, 17, 9, 9),
                 long = c(36, 14, 14, 36))

TD$inUG <-
  factor(point.in.polygon(TD$location.long,
                          TD$location.lat,
                          UG$long,
                          UG$lat))

TD_UG <- filter(TD, TD$inUG == 1)
TD_UG$inUG <- NULL


# Remove all locations from night except one and keep only one location per 2 hours------------------------------------------------------

TD_amt <-
  make_track(
    TD_UG,
    .x = location.long,
    .y = location.lat,
    .t = timestamp,
    ind.id = ind_name,
    crs = crs
  )

TD_dn <- time_of_day(TD_amt)

TD_dat <- as.data.frame(TD_dn)

TD_dat$goupsTime <- cut(TD_dat$t_, breaks = "120 min")

TD_dat <- TD_dat %>%
  group_by(goupsTime) %>%
  slice(1)

TD_dat$day <- substr(x = TD_dat$t_, start = 0, stop = 10)

TD_night <- TD_dat %>%
  filter(tod_ == "night") %>%
  group_by(ind.id, day) %>%
  slice(1)

TD_day <- TD_dat %>%
  filter(tod_ == "day")

TD_dat <- full_join(TD_day, TD_night) %>%
  select(-day)

TD_dat$goupsTime <- NULL

TD_dat <- TD_dat %>% ungroup() %>% group_by(ind.id)

IndFreqs <- TD_dat %>%
  group_by(ind.id) %>%
  dplyr::summarise(Freq = n())

TD_dat <- merge(TD_dat, IndFreqs, by = "ind.id")
TD_dat <- TD_dat %>% filter(Freq >= 20)
TD_dat <- TD_dat[c(2, 3, 4, 1, 5, 6)]


# Spatially filter the location data to 1 km ------------------------------

TD_dat_group <-
  TD_dat %>% group_by(ind.id) %>% group_split()

TD_group1_df <- as.data.frame(TD_dat_group[[1]])
TD_group2_df <- as.data.frame(TD_dat_group[[2]])
TD_group3_df <- as.data.frame(TD_dat_group[[3]])
TD_group4_df <- as.data.frame(TD_dat_group[[4]])
TD_group5_df <- as.data.frame(TD_dat_group[[5]])
TD_group6_df <- as.data.frame(TD_dat_group[[6]])


TD_1_xy <- TD_group1_df[, 1:2]

TD_1_data <- TD_group1_df[, 3:6]

TD_1_sp <-
  SpatialPointsDataFrame(coords = TD_1_xy,
                         data = TD_1_data,
                         proj4string = crs)

TD_1_st <- st_as_sf(x = TD_1_sp,
                    coords = TD_1_xy,
                    proj4string = crs)

TD_shapefile <-
  st_write(TD_st, paste0("Telemetry_data_", i, ".shp"))


TD_1_th <- remove.near(TD_1_sp, dist = thin_distance)


TD_2_xy <- TD_group2_df[, 1:2]

TD_2_data <- TD_group2_df[, 3:6]

TD_2_sp <-
  SpatialPointsDataFrame(coords = TD_2_xy,
                         data = TD_2_data,
                         proj4string = crs)

TD_2_st <- st_as_sf(x = TD_2_sp,
                    coords = TD_2_xy,
                    proj4string = crs)

TD_2_th <- remove.near(TD_2_sp, dist = thin_distance)


TD_3_xy <- TD_group3_df[, 1:2]

TD_3_data <- TD_group3_df[, 3:6]

TD_3_sp <-
  SpatialPointsDataFrame(coords = TD_3_xy,
                         data = TD_3_data,
                         proj4string = crs)

TD_3_st <- st_as_sf(x = TD_2_sp,
                    coords = TD_3_xy,
                    proj4string = crs)

TD_3_th <- remove.near(TD_3_sp, dist = thin_distance)


TD_4_xy <- TD_group4_df[, 1:2]

TD_4_data <- TD_group4_df[, 3:6]

TD_4_sp <-
  SpatialPointsDataFrame(coords = TD_4_xy,
                         data = TD_4_data,
                         proj4string = crs)

TD_4_st <- st_as_sf(x = TD_4_sp,
                    coords = TD_4_xy,
                    proj4string = crs)

TD_4_th <- remove.near(TD_4_sp, dist = thin_distance)


TD_5_xy <- TD_group5_df[, 1:2]

TD_5_data <- TD_group5_df[, 3:6]

TD_5_sp <-
  SpatialPointsDataFrame(coords = TD_5_xy,
                         data = TD_5_data,
                         proj4string = crs)

TD_5_st <- st_as_sf(x = TD_5_sp,
                    coords = TD_5_xy,
                    proj4string = crs)

TD_5_th <- remove.near(TD_5_sp, dist = thin_distance)


TD_6_xy <- TD_group6_df[, 1:2]

TD_6_data <- TD_group6_df[, 3:6]

TD_6_sp <-
  SpatialPointsDataFrame(coords = TD_6_xy,
                         data = TD_6_data,
                         proj4string = crs)

TD_6_st <- st_as_sf(x = TD_6_sp,
                    coords = TD_5_xy,
                    proj4string = crs)

TD_6_th <- remove.near(TD_6_sp, dist = thin_distance)


TD_thinned <-
  rbind(TD_1_th, TD_2_th, TD_3_th, TD_4_th, TD_5_th, TD_6_th)


TD_th_st <- st_as_sf(x = TD_thinned,
                     proj4string = crs)

TD_th_shapefile <-
  st_write(TD_th_st, paste0("Telemetry_data_thinned_", i, ".shp"))


# Get the number of location data after each filtering step ---------------

n_original <- nrow(TD)
n_UG <- nrow(TD_UG)
n_wo_night_2h <- nrow(TD_dat)
n_wo_1km <- nrow(TD_thinned)

rbind(n_original, n_UG, n_wo_night_2h, n_wo_1km)