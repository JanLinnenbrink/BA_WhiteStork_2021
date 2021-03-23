library(plotly)

prec <- read.csv(file.choose())
temp <- read.csv(file.choose())
x.labs <-
  c("J",
    "F",
    "M",
    "A",
    "M",
    "J",
    "J",
    "A",
    "S",
    "O",
    "N",
    "D")
x.title <- "Monat"
y.title.l <- "Temperatur (°C)"
y.title.r <- "Niederschlag (mm)"
size.labs <- 11
size.titles <- 12
col.t <- "sienna4"
col.p <- "blue1"
size <- 1

prec.1971 <- prec[which(prec$Year > 1970 & prec$Year < 2002), ]
temp.1971 <- temp[which(temp$Year > 1970 & temp$Year < 2002), ]

temp.prec <- merge(prec.1971, temp.1971)
colnames(temp.prec) <-
  c("Jahr", "Monat", "Niederschlag", "Temperatur")
temp.prec$Monat <- substr(temp.prec$Monat, 1, 4)

temp.prec.mean <-
  aggregate(temp.prec[, 3:4], list(temp.prec$Monat), mean)
colnames(temp.prec.mean) <- c("Monat", "Niederschlag", "Temperatur")

month.order <- c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)
month.num <- c(1:12)

temp.prec.mean <- temp.prec.mean[month.order ,]
temp.prec.mean$MonatN <- month.num


sahel.cd <- ggplot(data = temp.prec.mean) +
  geom_line(aes(x = MonatN, y = Temperatur, color = col.p), size = size) +
  geom_line(aes(x = MonatN, y = Niederschlag / 2, color = col.t), size = size) +
  scale_y_continuous(
    labels = c(0, 10, 20, 30, 40),
    breaks = c(0, 10, 20, 30, 40),
    limits = c(0, 40),
    sec.axis = sec_axis(
      ~ . * 2,
      name = y.title.r,
      labels = c(0, 20, 40, 60, 80),
      breaks = c(0, 20, 40, 60, 80)
    )
  ) +
  labs(x = x.title,
       y = y.title.l) +
  scale_x_continuous(breaks = c(1:12), labels = x.labs) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = size.titles),
    axis.text = element_text(size = size.labs),
    axis.title.x = element_text(vjust = 2),
    axis.title.y.left = element_text(vjust = 1),
    axis.title.y.right = element_text(vjust = 1)
  )

ggsave(
  file = "sahel_climate_diagram.png",
  plot = sahel.cd,
  width = 15,
  height = 8,
  units = "cm",
  dpi = 500
)