# prepare occurence data and sample background data --------------------------------------------------

occ_sp <- readRDS(file = "TD_train.rds") %>% spTransform(crs)

occ_df <- as.data.frame(occ_sp)

occ_10 <- occ_df %>% filter(month == 10)
occ_11 <- occ_df %>% filter(month == 11)
occ_12 <- occ_df %>% filter(month == 12)
occ_01 <- occ_df %>% filter(month == 01)
occ_02 <- occ_df %>% filter(month == 02)

#Achtung: möglicherweise sind Individuen in einzelnen Monaten nicht vertreten,
#Modellergebnisse darauf prüfen!

occ_10_l <- occ_10 %>% split(f=occ_10$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_11_l <- occ_11 %>% split(f=occ_11$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_12_l <- occ_12 %>% split(f=occ_12$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_01_l <- occ_01 %>% split(f=occ_01$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)
occ_02_l <- occ_02 %>% split(f=occ_02$ind_id, drop = TRUE) %>% lapply(dplyr::select,lng, lat)

#Monatliche Listen mit den Lng-Lat Infos der einzelnen Individuen wurden erstellt
#per lapply auf ENMEval-Funktion anwendbar


#plot occurences (And test-train?)
UG_df <- data.frame(
  long = c(36, 14, 14, 36),
  lat = c(17, 17, 9, 9))
UG_sf <- st_as_sf(UG_sp)

occ_df <- as.data.frame(occ) 


png("C:/1_Bachelorarbeit/Statistik/Diagramme/map_occs_wo.png",res = 1600, width = 15, height = 25, units = "cm")

theme_set(theme_bw())


occ_map <- ggplot(UG_sf) +
  geom_sf(fill = NA, linetype = 5) +
  geom_point(data = occ_df,
             mapping = aes(x = lng, y = lat, shape = month, fill = "#0000CD", alpha = 0.6))+
  coord_sf(datum = "+proj=longlat +datum=WGS84 +no_defs") +
  facet_wrap(~ind_id, ncol = 2) +
  xlab("Längengrad") + 
  ylab("Breitengrad") +
  theme(legend.position = "none")

occ_map

dev.off()


# prepare environmental variables -----------------------------------------

#March entfernen

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


#Listen der EnvVariablen erstellen

env10l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_10", "tmax_10", "tmin_10")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_10"))
env11l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_11", "tmax_11", "tmin_11")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_11"))
env12l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_12", "tmax_12", "tmin_12")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_12"))
env01l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_01", "tmax_01", "tmin_01")),
               raster::subset(NDVI_w_stack_SDM, "NDVI_01"))
env02l <- list(elev, settl_dist, water_dist, LandCov_stack_SDM, LSM_F_stack_SDM, LSM_movingW, 
               raster::subset(weather_stack_SDM,c("prec_02", "tmax_02", "tmin_02")),
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


#Backround-Points für das Populationsmodell (1x1km-grid)

env_bg_population <- crop(elev, UG_sp)

bp_population <- randomPoints(env_bg_population, 10000) # faster computing, logistische steigerung

#Background-Points für das Individuenmodell (5x5km-grid)

env_bg_individual <- crop(weather_stack_SDM, UG_sp)
bp_ind <- randomPoints(env_bg_individual, 10000)


# Werte der Env-Variables an occs und background anbinden (samples with data SWD) --------
#schneller und keine Entfernung von doppelten Präsenzdaten per Rasterzelle, aber
#keine Prediction-map

bgi.p <- cbind(bp_population, raster::extract(envs, bg))

#Individuenmodell
bg_i_10_e <- as.data.frame(cbind(bp_ind, raster::extract(env10, bp_ind)))
bg_i_11_e <- as.data.frame(cbind(bp_ind, raster::extract(env11, bp_ind)))
bg_i_12_e <- as.data.frame(cbind(bp_ind, raster::extract(env12, bp_ind)))
bg_i_01_e <- as.data.frame(cbind(bp_ind, raster::extract(env01, bp_ind)))
bg_i_02_e <- as.data.frame(cbind(bp_ind, raster::extract(env02, bp_ind)))

colnames(bg_i_10_e)[1:2] <- c("lng", "lat")
colnames(bg_i_11_e)[1:2] <- c("lng", "lat")
colnames(bg_i_12_e)[1:2] <- c("lng", "lat")
colnames(bg_i_01_e)[1:2] <- c("lng", "lat")
colnames(bg_i_02_e)[1:2] <- c("lng", "lat")

EnvBind <- function(occ_list, envs) {
  occ_list_e <- cbind(occ_list, raster::extract(envs, occ_list))
}

occ_i_10_e <- lapply(occ_10_l, EnvBind, envs = env10)
occ_i_11_e <- lapply(occ_11_l, EnvBind, envs = env11)
occ_i_12_e <- lapply(occ_12_l, EnvBind, envs = env12)
occ_i_01_e <- lapply(occ_01_l, EnvBind, envs = env01)
occ_i_02_e <- lapply(occ_02_l, EnvBind, envs = env02)


# tune model parameters ------------------------------------------------------------

#FC: Q: unimodal response curve, L: Niche split, P: complex interactions btw. preds
#RM: je hoeher, desto strenger penalized (und einfacher zu interpretieren, Standart 1)

tune.args <- list(fc = c("L", "Q", "LQ"), rm = 2:6)

#Build models


e_m_10 <- lapply(X = occ_i_10_e, FUN = ENMevaluate, bg = bg_i_10_e,
                      algorithm = "maxnet", tune.args = tune.args,
                      partitions = "block", updateProgress = TRUE,
                      numCores = NULL, taxon.name = "Ciconia ciconia")

e_m_11 <- lapply(X = occ_i_11_e, FUN = ENMevaluate, bg = bg_i_11_e,
                 algorithm = "maxnet", tune.args = tune.args,
                 partitions = "block", updateProgress = TRUE,
                 numCores = NULL, taxon.name = "Ciconia ciconia")


e_m_12 <- lapply(X = occ_i_12_e, FUN = ENMevaluate, bg = bg_i_12_e,
                 algorithm = "maxnet", tune.args = tune.args,
                 partitions = "block", updateProgress = TRUE,
                 numCores = NULL, taxon.name = "Ciconia ciconia")

e_m_01 <- lapply(X = occ_i_01_e, FUN = ENMevaluate, bg = bg_i_01_e,
                 algorithm = "maxnet", tune.args = tune.args,
                 partitions = "block", updateProgress = TRUE,
                 numCores = NULL, taxon.name = "Ciconia ciconia")

e_m_02 <- lapply(X = occ_i_02_e, FUN = ENMevaluate, bg = bg_i_02_e,
                 algorithm = "maxnet", tune.args = tune.args,
                 partitions = "block", updateProgress = TRUE,
                 numCores = NULL, taxon.name = "Ciconia ciconia")

e_m_l <- list(e_m_10, e_m_11, e_m_12, e_m_01, e_m_02)
names(e_m_l) <- c("10", "11", "12", "01", "02")

rangeModelMetadata::rmmToCSV(rmm, "ind_models_metadat.csv")

#save models

saveRDS(e_m_10, file = "ind_models_10")
saveRDS(e_m_11, file = "ind_models_11")
saveRDS(e_m_12, file = "ind_models_12")
saveRDS(e_m_01, file = "ind_models_01")
saveRDS(e_m_02, file = "ind_models_02")


#get the named vector of the variable coefficients


eval.results.partitions(e_m_10[[1]]) %>% head()


#combine evaluation metrics in one data.frame


e_m_ul <- e_m_l %>% unlist(use.names = TRUE) 

df <- data.frame(BAV3_10 = e_m_ul[["10.BAV3"]]@results,
                 BAV3_11 = e_m_ul[["11.BAV3"]]@results)

df_10.BAV3 <- e_m_ul[["10.BAV3"]]@results
df_10.BEg3 <- e_m_ul[["10.BEg3"]]@results
df_10.BEg6 <- e_m_ul[["10.BEg6"]]@results
df_10.BEg7 <- e_m_ul[["10.BEg7"]]@results
df_10.LOB7 <- e_m_ul[["10.LOB7"]]@results
df_10.MOS1 <- e_m_ul[["10.MOS1"]]@results
df_10.MPg2 <- e_m_ul[["10.MPg2"]]@results
df_10.MPg6 <- e_m_ul[["10.MPg6"]]@results
df_10.POL7 <- e_m_ul[["10.POL7"]]@results

ind_eval_m_10 <-rbind(df_10.BAV3,df_10.BEg3,df_10.BEg6,df_10.BEg7,df_10.LOB7,
                      df_10.MOS1,df_10.MPg2,df_10.MPg6,df_10.POL7)


df_11.BAV3 <- e_m_ul[["11.BAV3"]]@results
df_11.BEg3 <- e_m_ul[["11.BEg3"]]@results
df_11.BEg6 <- e_m_ul[["11.BEg6"]]@results
df_11.BEg7 <- e_m_ul[["11.BEg7"]]@results
df_11.LOB7 <- e_m_ul[["11.LOB7"]]@results
df_11.MOS1 <- e_m_ul[["11.MOS1"]]@results
df_11.MPg2 <- e_m_ul[["11.MPg2"]]@results
df_11.MPg6 <- e_m_ul[["11.MPg6"]]@results
df_11.POL7 <- e_m_ul[["11.POL7"]]@results

ind_eval_m_11 <-rbind(df_11.BAV3,df_11.BEg3,df_11.BEg6,df_11.BEg7,df_11.LOB7,
                      df_11.MOS1,df_11.MPg2,df_11.MPg6,df_11.POL7)

df_12.BAV3 <- e_m_ul[["12.BAV3"]]@results
df_12.BEg3 <- e_m_ul[["12.BEg3"]]@results
df_12.BEg6 <- e_m_ul[["12.BEg6"]]@results
df_12.BEg7 <- e_m_ul[["12.BEg7"]]@results
df_12.LOB7 <- e_m_ul[["12.LOB7"]]@results
df_12.MOS1 <- e_m_ul[["12.MOS1"]]@results
df_12.MPg2 <- e_m_ul[["12.MPg2"]]@results
df_12.MPg6 <- e_m_ul[["12.MPg6"]]@results
df_12.POL7 <- e_m_ul[["12.POL7"]]@results

ind_eval_m_12 <-rbind(df_12.BAV3,df_12.BEg3,df_12.BEg6,df_12.BEg7,df_12.LOB7,
                      df_12.MOS1,df_12.MPg2,df_12.MPg6,df_12.POL7)

df_01.BAV3 <- e_m_ul[["01.BAV3"]]@results
df_01.BEg3 <- e_m_ul[["01.BEg3"]]@results
df_01.BEg6 <- e_m_ul[["01.BEg6"]]@results
df_01.BEg7 <- e_m_ul[["01.BEg7"]]@results
df_01.LOB7 <- e_m_ul[["01.LOB7"]]@results
df_01.MOS1 <- e_m_ul[["01.MOS1"]]@results
df_01.MPg2 <- e_m_ul[["01.MPg2"]]@results
df_01.MPg6 <- e_m_ul[["01.MPg6"]]@results
df_01.POL7 <- e_m_ul[["01.POL7"]]@results

ind_eval_m_01 <-rbind(df_01.BAV3,df_01.BEg3,df_01.BEg6,df_01.BEg7,df_01.LOB7,
                      df_01.MOS1,df_01.MPg2,df_01.MPg6,df_01.POL7)

df_02.BAV3 <- e_m_ul[["02.BAV3"]]@results
df_02.BEg3 <- e_m_ul[["02.BEg3"]]@results
df_02.BEg6 <- e_m_ul[["02.BEg6"]]@results
df_02.BEg7 <- e_m_ul[["02.BEg7"]]@results
df_02.LOB7 <- e_m_ul[["02.LOB7"]]@results
df_02.MOS1 <- e_m_ul[["02.MOS1"]]@results
df_02.MPg2 <- e_m_ul[["02.MPg2"]]@results
df_02.MPg6 <- e_m_ul[["02.MPg6"]]@results
df_02.POL7 <- e_m_ul[["02.POL7"]]@results

ind_eval_m_02 <-rbind(df_02.BAV3,df_02.BEg3,df_02.BEg6,df_02.BEg7,df_02.LOB7,
                      df_02.MOS1,df_02.MPg2,df_02.MPg6,df_02.POL7)



ind_eval_m <- list(ind_eval_m_10, ind_eval_m_11, ind_eval_m_12, ind_eval_m_01, ind_eval_m_02)

ind_eval_ul <- ind_eval_m %>% map(~as_tibble(.)) %>% bind_rows(.id="index") #index 1 = Oktober

ind_eval_n <- as.numeric(ind_eval_ul$index)

mth <- c("Oktober", "November", "Dezember", "Januar", "Februar")

ind_eval_ul$month  <- rep(mth, each = 135)

ind_eval_ul2 <- mutate(transform(ind_eval_ul,
                                 month=factor(month,levels=mth)),month)

# Vizualise tuning results --------------------------------------------------


png("C:/1_Bachelorarbeit/Statistik/Diagramme/Ind_Model_Eval/AICc.png",
     width = 14, height = 14, units = "cm", res = 1200)

ggplot(ind_eval_ul2)+
  geom_boxplot(aes(x=rm, y = AICc, color=fc), 
              outlier.shape = NA)+
  facet_wrap(as.factor(ind_eval_ul$month), ncol = 2)+
  theme(legend.position = c(0.75, 0.15))

dev.off()


png("C:/1_Bachelorarbeit/Statistik/Diagramme/Ind_Model_Eval/AUCtrain.png",
    width = 14, height = 14, units = "cm", res = 1200)

ggplot(ind_eval_ul2)+
  geom_boxplot(aes(x=rm, y = auc.train, color=fc), 
               outlier.shape = NA)+
facet_wrap(as.factor(ind_eval_ul$month), ncol = 2)+
  theme(legend.position = c(0.75, 0.15))

dev.off()

# select model ------------------------------------------------------------


eval.models










for(i in seq_along(e_m_l)) { 
  for j in seq_along(e_m_l) {
  png(paste("C:/1_Bachelorarbeit/Statistik/Diagramme/Ind_Model_Eval/AICc_", i, j, ".png"), 
      width = 14, height = 12, units = "cm", res = 1200)
  evalplot <- lapply(e_m_l[[i]]@results, evalplot.stats, stats = "AICc", color = "fc", x.var = "rm", 
                     error.bars = FALSE)
  dev.off()}
}


lapply(e_m_l, function(x) lapply(lapply(x, evalplot.stats)))

evalplot.stats(e = e_m_10[[1]], stats = "AICc", color = "fc", x.var = "rm", 
               error.bars = FALSE)




modelBuild <- function(occ, bp, tune.args) {
  model <- ENMevaluate(occs = occ[[i]], bg = bp,
                       algorithm = "maxnet", tune.args = tune.args,
                       partitions = "block", updateProgress = TRUE,
                       numCores = NULL, taxon.name = "Ciconia ciconia")
}


m <- for (i in occ_i_10_e) {
  model[[i]] <- ENMevaluate(occs = occ_i_10_e[[i]], bg = bg_i_10_e,
                            algorithm = "maxnet", tune.args = tune.args,
                            partitions = "block", updateProgress = TRUE,
                            numCores = NULL, taxon.name = "Ciconia ciconia")
}

