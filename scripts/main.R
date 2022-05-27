cat("\014")
rm(list=ls())

library(ggplot2)
library(lubridate)
library(vars)
library(urca)
library(aTSA)
library(tseries)
library(strucchange)

monthly <- T
start_day <- "1900-01-01"
end_day   <- "2022-12-31"

gasColor <- "#74a9f7"
oilColor <- "#21245c"
varColor <- "#fc9e2b"
vecColor <- "#6135f2"

source("00_load.R")
source("01_cointegration.R")
source("02_fitting.R")
source("03_structural.R")