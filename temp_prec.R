# load packages -----------------------------------------------------------


library(plotly)
library(scales)
library(ggsci)
library(readxl)
library(lemon)
library(egg)


# general settings --------------------------------------------------------


col.p <- "blue3"
col.n <- "red"

size.smooth <- 0.9
span.smooth <- 0.4
color.smooth <- "Loess smoothing"

breaks <- c(1900:2015)
labels <- breaks
labels[!(labels %% 10 == 0)] <- ""
tick.sizes <- rep(0.2, length(breaks))
tick.sizes[(breaks %% 10 == 0)] <- 1

title.size <- 9


# Precipation -------------------------------------------------------------


citation <-
  "Mitchell, T. (2017): Sahel Precipitation Index (20-10N, 20W-10E), 1900 - May 2015. doi:10.6069/H5MW2F2Q"

prec <- read_excel(file.choose())
prec.df <- as.data.frame(prec)

prec.summer <- prec.df[which(prec.df$month > 5 &
                               prec.df$month < 11), ]
prec.winter <- prec.df[which(prec.df$month > 10 |
                               prec.df$month < 6), ]

prec.mean <- aggregate(prec.df[, 3], list(prec.df$year), mean)
prec.mean.s <-
  aggregate(prec.summer[, 3], list(prec.summer$year), mean)
prec.mean.w <-
  aggregate(prec.winter[, 3], list(prec.winter$year), mean)

cn.p <- c("Jahr", "Niederschlag")

colnames(prec.mean) <- cn.p
colnames(prec.mean.s) <- cn.p
colnames(prec.mean.w) <- cn.p


prec.mean$colour <-
  ifelse(prec.mean$Niederschlag < 0, "negative", "positive")
prec.mean.s$colour <-
  ifelse(prec.mean.s$Niederschlag < 0, "negative", "positive")
prec.mean.w$colour <-
  ifelse(prec.mean.w$Niederschlag < 0, "negative", "positive")

y.title.p <- "Niederschlagsanomalie (cm)"


theme_set(theme_bw())

prec.p <- ggplot(data = prec.mean.s, aes(x = Jahr,
                                         y = Niederschlag)) +
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
  scale_fill_manual(values = c(positive = col.p, negative = col.n)) +
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

temp.mean <- mean(temp.df$Temperatur)
temp.mean.y <- aggregate(temp.df[, 1], list(temp.df$Jahr), mean)

colnames(temp.mean.y) <- c("Jahr", "Temperatur")

temp.mean.y$Temperaturanomalie <- temp.mean.y$Temperatur - temp.mean

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
  scale_fill_manual(values = c(positive = col.n, negative = col.p)) +
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