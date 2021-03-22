# load packages -----------------------------------------------------------


library(plotly)
library(scales)
library(ggsci)
library(readxl)
library(lemon)
library(egg)


# general settings --------------------------------------------------------

citation <-
  "World Bank Group (2021): Climate Change Knowledge Portal. https://climateknowledgeportal.worldbank.org/"


theme_set(theme_bw())

col.pp <- "blue1"
col.np <- "deepskyblue2"
col.pt <- "sienna4"
col.nt <- "sienna2"

size.smooth <- 0.65
span.smooth <- 0.4
color.smooth <- "Loess smoothing"

breaks <- c(1900:2015)
labels <- breaks
labels[!(labels %% 10 == 0)] <- ""
tick.sizes <- rep(0.2, length(breaks))
tick.sizes[(breaks %% 10 == 0)] <- 1

title.size <- 9


# Precipation -------------------------------------------------------------


prec <- read.csv(file.choose())
prec.df <- as.data.frame(prec)


cn.p <- c("Niederschlag", "Jahr", "Monat")

colnames(prec.df) <- cn.p

prec.20 <- prec.df[which(prec.df$Jahr < 2001), ]
prec.mean.20 <-  mean(prec.20$Niederschlag)
prec.mean.y <-
  aggregate(prec[, 1], list(prec.df$Jahr), mean)

colnames(prec.mean.y) <- c("Jahr", "Niederschlag")

prec.mean.y$Niederschlagsanomalie <-
  prec.mean.y$Niederschlag - prec.mean.20

y.title.p <- "Niederschlagsanomalie (cm)"

prec.mean.y$colour <-
  ifelse(prec.mean.y$Niederschlagsanomalie < 0, "negative", "positive")

prec.p <- ggplot(data = prec.mean.y, aes(x = Jahr,
                                         y = Niederschlagsanomalie)) +
  geom_bar(
    aes(fill = colour),
    stat = "identity",
    position = "identity",
    width = 0.6,
    size = 0.4
  ) +
  geom_smooth(
    size = size.smooth,
    se = F,
    span = span.smooth,
    aes(color = color.smooth)
  ) +
  scale_fill_manual(values = c(positive = col.pp, negative = col.np)) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  labs(y = y.title.p) +
  theme(
    legend.position = "None",
    panel.grid =  element_blank(),
    axis.ticks.x = element_line(size = tick.sizes),
    axis.title.y.right = element_blank(),
    axis.title = element_text(size = title.size)
  )


# temperature -------------------------------------------------------------


temp <- read.csv(file.choose())

temp.df <- as.data.frame(temp)

cn.t <- c("Temperatur", "Jahr", "Monat")

colnames(temp.df) <- cn.t

temp.20 <- temp.df[which(temp.df$Jahr < 2001), ]
temp.mean.20 <-  mean(temp.20$Temperatur)
temp.mean.y <- aggregate(temp.df[, 1], list(temp.df$Jahr), mean)

colnames(temp.mean.y) <- c("Jahr", "Temperatur")

temp.mean.y$Temperaturanomalie <-
  temp.mean.y$Temperatur - temp.mean.20

temp.mean.y$colour <-
  ifelse(temp.mean.y$Temperaturanomalie < 0, "negative", "positive")

y.title.t <- "Temperaturanomalie (K)"

temp.p <- ggplot(data = temp.mean.y, aes(x = Jahr,
                                         y = Temperaturanomalie)) +
  geom_bar(
    aes(fill = colour),
    stat = "identity",
    position = "identity",
    width = 0.6,
    size = 0.4
  ) +
  geom_smooth(
    size = size.smooth,
    se = F,
    span = span.smooth,
    aes(color = color.smooth)
  ) +
  scale_fill_manual(values = c(positive = col.pt, negative = col.nt)) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  labs(y = y.title.t) +
  theme(
    legend.position = "None",
    panel.grid =  element_blank(),
    axis.ticks.x = element_line(size = tick.sizes),
    axis.title.y.right = element_blank(),
    axis.title = element_text(size = title.size)
  )


# combine and save plots -------------------------------------------------------------


sahel.pt <-
  ggarrange(
    plots = list(prec.p, temp.p),
    nrow = 2,
    labels = c("(a)", "(b)"),
    label.args = list(gp = grid::gpar(font = 2, cex = 1))
  )

ggsave(
  file = "sahel_prec_temp.png",
  plot = sahel.pt,
  width = 15,
  units = "cm",
  dpi = 500
)
