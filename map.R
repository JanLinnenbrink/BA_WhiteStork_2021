library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
library(tidyverse)
library(rgdal)
library(rworldmap)

world <- map_data("world")

world$country <- str_to_upper(world$region)
countries.df$country <- str_to_upper(countries.df$country)
colnames(countries.df) <- c("region", "absolut", "relativ")

world.publications <-
  merge(world, countries.df, by = "region", all = TRUE)

world.publications[is.na(world.publications)] <- 0

JSCM <-
  joinCountryData2Map(dF = world.publications,
                      joinCode = "NAME",
                      nameJoinColumn = "country")
mapDevice()
mapBubbles(JSCM, nameZSize = "relativerAnteil", nameZColour = "absoluterAnteil", catMethod = "pretty")

JSCM.df <- as.data.frame(JSCM)

theme_set(new = theme_void())

world <- map_data("world")
cities <- world.cities
capitals <- cities[cities$capital==1,]
colnames(capitals) <- c("name", "region", "pop", "lat", "lon", "capital")
capitals.publications <- merge(capitals, countries.df, by = "region", all = TRUE)
capitals.publications[is.na(capitals.publications)] <- 0

ggplot() +
  geom_polygon(data = world, mapping = aes(x=long, y=lat, group=group), fill="grey") +
  geom_point(data = capitals.publications, aes(x=lon,y=lat,size = relativ, color=relativ))
  geom_sf()