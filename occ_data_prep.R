library(amt)
library(move)
library(rgdal)
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
library(dismo)


set.seed(28)

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

# Download movebank data, only keep Argos data with estimate error < 1km-----------------------------------------------

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


# Merge all TD into one data.frame ----------------------------------------

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



# Download Species occurrence data from other sources (independent  --------


test_ind <-
  occ(
    query = "Ciconia ciconia",
    from = "gbif",
    date = c("2014-10-01", "2015-04-01"),
    limit = 100000,
    has_coords = TRUE,
    geometry = UG_sf,
  ) %>% occ2sp(coord_string = "+init=epsg:4329")

# Achtung: 2014/15 nur 3 Beobachtungen am Ostrand des UG!

test_ind_shp <-
  writeOGR(
    obj = test_ind,
    overwrite_layer = TRUE,
    dsn = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/SHP",
    layer = paste0("test_ind"),
    driver = "ESRI Shapefile"
  )

# Ortungsdaten außerhalb des UG, die fälschlicherweise noch im test-Datensatz enthalten waren, wurden in ArcGIS entfernt

test_ind_ug <-
  readOGR(dsn = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/SHP/test_ind_ug.shp")




# Clip to UG ------------------------------------------------------


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


# Remove all locations from night except one ------------------------------------------------------

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


# temporal thinning -------------------------------------------------------


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

# Spatially filter the location data to 1 km ------------------------------

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

# Subsample the location data by the minimum number of location data for each individual ---------------------

n_min <- min(n_spatiotemporalthinning[, 3])

#MOS4 und POL4 haben nicht in allen Monaten > 40 Nachweise (Entfernen)

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

# get removed high-quality data for testing the Population model -------------------------------

TD_filtered_df <- TD_threshold %>% as.data.frame()

TD_hq_notused <-
  TD_filtered_df[!(TD_filtered_df$lng %in% TD_samples$lng &
                     TD_filtered_df$lat %in% TD_samples$lat),]

coordinates(TD_hq_notused) <- ~ lng + lat
crs(TD_hq_notused) <- crs_new

TD_test_shp <-
  writeOGR(
    obj = TD_hq_notused,
    overwrite_layer = TRUE,
    dsn = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/SHP",
    layer = paste0("TD_test_samples_n_", n_min),
    driver = "ESRI Shapefile"
  )

# Get the number of location data after each filtering step ---------------

n_UG$step <- 1
n_dn$step <- 2
n_temporalthinning$step <- 3
n_spatiotemporalthinning$step <- 4

n_ind <-
  rbind(n_UG, n_dn, n_temporalthinning, n_spatiotemporalthinning)

n_ind_extrema <- n_ind %>%
  group_by(month, step) %>%
  mutate(
    n_min = max(Freq, na.rm = TRUE),
    n_max = min(Freq, na.rm = TRUE),
    n_mean = mean(Freq, na.rm = TRUE),
    n_sd = sd(Freq, na.rm = TRUE)
  )
n_ind_extrema$month <- as.factor(n_ind_extrema$month)

n_ind_extrema <-
  n_ind_extrema %>% mutate(month = factor(month,
    levels =
      c("3", "2", "1", "12", "11", "10")
  ))

pd <- position_dodge(0.6)

filters_plot <-
  ggplot(data = n_ind_extrema, aes(
    x = Freq,
    y = as.factor(step),
    colour = as.factor(month)
  )) +
  theme_set(new = theme_bw()) +
  geom_errorbar(aes(xmin = n_min, xmax = n_max),
    position = pd,
    width = 0.5
  ) +
  geom_point(position = pd, aes(x = n_mean)) +
  scale_y_discrete(
    limits = rev,
    labels = c(
      "räumlich gefiltert",
      "zeitlich gefiltert",
      "am Tag*",
      "im UG"
    )
  ) +
  scale_x_continuous(
    trans = sqrt_trans(),
    breaks = trans_breaks("log2", function(x) {
      2^x
    }),
    labels = trans_format("log2", math_format(2^.x))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    tag = "Telemetriedaten\nje Individuum...",
    title = paste0("Filter: ", thin_distance, " m & ", thin_time, " min"),
    colour = "Monat",
    x = "n"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "italic"),
    axis.title.y = element_blank(),
    plot.margin = unit(c(2, 1, 1, 5), "lines"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0, 0.9),
    plot.tag = element_text(size = 12, face = "italic")
  ) +
  guides(colour = guide_legend(reverse = T))

filters_plot

ggsave(
  plot = filters_plot,
  filename = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/filter.png",
  width = 8,
  height = 6
)

saveRDS(TD_samples, file = "TD_train.rds")
saveRDS(TD_hq_notused, file = "TD_test_ind.rds")

# Remove unnecessary data and save the needed ones-------------------------------------------------


rm(list = setdiff(
  ls(),
  c(
    "n_ind_extrema",
    "filters_plot",
    "TD_samples",
    "TD_hq_notused",
    "UG_sp",
    "test_ind"
  )
))



