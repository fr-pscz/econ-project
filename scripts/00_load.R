library(ggplot2)
rm(list=ls())

monthly <- T

source("helper_functions.R")

WTI <- read.csv("../data/WTI.csv", header = F)
names(WTI) <- c("date", "WTI")
WTI$date <- as.Date(WTI$date, "%m/%d/%Y")

BRENT <- read.csv("../data/BRENT.csv", header = F)
names(BRENT) <- c("date", "BRENT")
BRENT$date <- as.Date(BRENT$date, "%m/%d/%Y")

HENRY <- read.csv("../data/NG.csv", header = F)
names(HENRY) <- c("date", "HENRY")
HENRY$date <- as.Date(HENRY$date, "%m/%d/%Y")
plotC <- 15

df <- merge(BRENT,HENRY)

if (monthly) {
  df <- turn_to_monthly(df)
  #df <- df[df$date < as.Date("2007-01-01"),]
  df <- df[df$date < as.Date("2008-04-16"),]
}

df$logBRENT <- log(df$BRENT)
df$logHENRY <- log(df$HENRY)

p <- ggplot(df, aes(x=date)) +
  geom_line(mapping = aes(y=logHENRY - mean(logHENRY),color="HENRY")) +
  geom_line(mapping = aes(y=logBRENT - mean(logBRENT),color="BRENT")) +
  ylab("Price") + xlab("") + theme_minimal() +
  scale_colour_manual("", 
                      breaks = c("HENRY", "BRENT"),
                      values = c("#06a11e", "#0c07a8"))
show(p)
