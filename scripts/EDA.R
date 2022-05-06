rm(list=ls())

WTI <- read.csv("../data/WTI.csv", header = F)
names(WTI) <- c("date", "USD")
WTI$date <- as.Date(WTI$date, "%m/%d/%Y")

BRENT <- read.csv("../data/BRENT.csv", header = F)
names(BRENT) <- c("date", "USD")
BRENT$date <- as.Date(BRENT$date, "%m/%d/%Y")

HENRY <- read.csv("../data/NG.csv", header = F)
names(HENRY) <- c("date", "USD")
HENRY$date <- as.Date(HENRY$date, "%m/%d/%Y")
plotC <- 20
HENRY$Btu <- HENRY$USD
HENRY$USD <- HENRY$Btu * plotC

library(ggplot2)

p <- ggplot(WTI, aes(x=date, y=USD)) +
  geom_line(data = WTI, color="#8c2303") + 
  geom_line(data = BRENT, color="#0c07a8") +
  geom_line(data = HENRY, color="#06a11e") +
  xlab("")
show(p)
