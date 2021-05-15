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

crs <- CRS("+init=epsg:4329")
crs_new <-
  CRS(
    "+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  )

thin_distance <- 1000 #meters
thin_time <- 120

threshold_min_n <- 40 #Minimal occurence data/Individuum/month



# Download movebank data, only keep Argos data with estimate error < 1km-----------------------------------------------

Eastern_flyway <-
  move(x = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/Eastern flyway spring migration of adult white storks (data from Rotics et al. 2018).csv")

ef_df <- as.data.frame(Eastern_flyway)

n.storks <- length(unique(ef_df$individual.local.identifier))
ind_name <- paste0("EAS", 1:n.storks)
ef_df$ind_name <-
  factor(ef_df$individual.local.identifier, labels = ind_name)


ef_df <- ef_df[, (names(ef_df) %in% keep_cols_gps)]

Bavaria <-
  move(x = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Bavaria.csv")

b_df <- as.data.frame(Bavaria)

n.storks <- length(unique(b_df$individual.local.identifier))
ind_name <- paste0("BAV", 1:n.storks)
b_df$ind_name <-
  factor(b_df$individual.local.identifier, labels = ind_name)

b_df <- b_df[, (names(b_df) %in% keep_cols_gps)]


Greece <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Greece Evros Delta .csv"
  )

g_df <- as.data.frame(Greece)

n.storks <- length(unique(g_df$individual.local.identifier))
ind_name <- paste0("GRE", 1:n.storks)
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
ind_name <- paste0("LOB", 1:n.storks)
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
ind_name <- paste0("MOS", 1:n.storks)
m_df$ind_name <-
  factor(m_df$individual.local.identifier, labels = ind_name)

m_df <- m_df[, (names(m_df) %in% keep_cols_gps)]


Poland <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/LifeTrack White Stork Poland.csv"
  )

p_df <- as.data.frame(Poland)

n.storks <- length(unique(p_df$individual.local.identifier))
ind_name <- paste0("POL", 1:n.storks)
p_df$ind_name <-
  factor(p_df$individual.local.identifier, labels = ind_name)

p_df <- p_df[, (names(p_df) %in% keep_cols_gps)]


MPIAB_gps <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/MPIAB Argos white stork tracking (1991-2018).csv"
  )

Mg_df <- as.data.frame(MPIAB_gps)

n.storks <- length(unique(Mg_df$individual.local.identifier))
ind_name <- paste0("MPg", 1:n.storks)
Mg_df$ind_name <-
  factor(Mg_df$individual.local.identifier, labels = ind_name)

Mg_df <- Mg_df[, (names(Mg_df) %in% keep_cols_gps)]


MPIAB_argos <-
  move(
    "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/MPIAB Argos white stork tracking (1991-2018) (1).csv"
  )

Ma_df <- as.data.frame(MPIAB_argos)

n.storks <- length(unique(Ma_df$individual.local.identifier))
ind_name <- paste0("MPa", 1:n.storks)
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
ind_name <- paste0("BEg", 1:n.storks)
Bg_df$ind_name <-
  factor(Bg_df$individual.local.identifier, labels = ind_name)

Bg_df <- Bg_df[, (names(Bg_df) %in% keep_cols_gps)]


Bergenhusen_argos <-
  move("C:/1_Bachelorarbeit/Statistik/1_Data_preparation/TD/NABU_Bergenhusen (1).csv")

Ba_df <- as.data.frame(Bergenhusen_argos)

n.storks <- length(unique(Ba_df$individual.local.identifier))
ind_name <- paste0("BEa", 1:n.storks)
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
                 long = c(39, 14, 14, 39))

TD$inUG <- factor(point.in.polygon(TD$location.long,
                                   TD$location.lat,
                                   UG$long,
                                   UG$lat))

TD_UG <- filter(TD, TD$inUG == 1)
TD_UG$inUG <- NULL

TD_UG$month <- month(TD_UG$timestamp)
TD_10_3 <- TD_UG %>% filter(month != 9)

n_UG <-
  TD_10_3 %>% group_by(ind_name, month) %>% dplyr::summarise(Freq = n())
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
  group_by(ind.id, month) %>% dplyr::summarise(Freq = n())
colnames(n_dn) <- c("ind_id", "month", "Freq")


# temporal thinning -------------------------------------------------------


TD_track$groupsTime <- cut(TD_track$t_, breaks = "90 mins")

TD_track_tempFilter <- TD_track %>%
  group_by(groupsTime, ind.id) %>%
  filter(row_number() == 1)

TD_track_tempFilter$tod_ <- NULL
TD_track_tempFilter$groupsTime <- NULL

n_temporalthinning <- TD_track_tempFilter %>%
  group_by(ind.id, month) %>% dplyr::summarise(Freq = n())
colnames(n_temporalthinning) <- c("ind_id", "month", "Freq")

# Spatially filter the location data to 1 km ------------------------------

names(TD_track_tempFilter) <-
  c("lng", "lat", "time", "ind_id", "month")

TD_g <-
  TD_track_tempFilter %>% split(list(TD_track_tempFilter$ind_id, TD_track_tempFilter$month),
                                drop = TRUE)

spatialFilter <- function(df, dist) {
  coordinates(df) <- ~ lng + lat
  crs(df) <- crs
  df_tr <- spTransform(df, crs_new)
  TD_sT2 <-
    remove.near(df_tr, dist = dist)
}

TD_SpT <- lapply(TD_g, spatialFilter, dist = thin_distance)

TD_SpT_SPDF <- do.call(rbind.SpatialPointsDataFrame, TD_SpT)

TD_threshold <-
  TD_SpT_SPDF %>% as.data.frame() %>% group_by(ind_id, month) %>% filter(n() >= threshold_min_n)

n_spatiotemporalthinning <-
  TD_threshold %>% as.data.frame() %>% group_by(ind_id, month) %>% dplyr::summarise(Freq = n())

TD_inG <-
  n_spatiotemporalthinning %>% group_by(ind_id) %>% filter(n() == 5) #only individuals with data from all month

TD_threshold <- subset(TD_threshold, TD_threshold$ind_id %in% TD_inG$ind_id)

# Subsample the location data by the minimum number of location data for each individual ---------------------

n_min <- min(n_spatiotemporalthinning[, 3])

TD_samples <-
  TD_threshold %>%  as.data.frame() %>% group_by(ind_id, month) %>% sample_n(n_min)


coordinates(TD_samples) <- ~ lng + lat
crs(TD_samples) <- crs_new

TD_samples_shp <-
  writeOGR(
    obj = TD_samples,
    overwrite_layer = TRUE,
    dsn = "C:/1_Bachelorarbeit/Statistik/1_Data_preparation/SHP",
    layer = paste0("TD_train_samples_n_", n_min),
    driver = "ESRI Shapefile"
  )


# get removed high-quality data for testing -------------------------------

TD_filtered_df <- TD_threshold %>% as.data.frame()

TD_hq_notused <-
  TD_filtered_df[!(TD_filtered_df$lng %in% TD_samples$lng &
                     TD_filtered_df$lat %in% TD_samples$lat), ]

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
n_ind_extrema$month <-  as.factor(n_ind_extrema$month)

n_ind_extrema <-
  n_ind_extrema %>% arrange(Freq) %>%  mutate(month = factor(month, levels =
                                                               c("10", "11", "12", "1", "2", "3")))

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
                width = 0.5) +
  geom_point(position = pd, aes(x = n_mean)) +
  scale_y_discrete(
    limits = rev,
    labels = c("räumlich gefiltert",
               "zeitlich gefiltert",
               "am Tag*",
               "im UG")
  ) +
  scale_x_continuous(trans = log10_trans(), labels = trans_format("log10", math_format(10 ^
                                                                                         .x))) +
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
  )

filters_plot

# Remove unnecessary data -------------------------------------------------
#rm(list=setdiff(ls(), c("n_ind"
#                        "TD_samples",
#                        "TD_hq_notused")))
