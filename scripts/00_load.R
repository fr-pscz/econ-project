message("============")
message("0. LOAD DATA")
message("============")

source("helper_functions.R")

OIL <- read.csv("../data/WTI.csv", header = F)
names(OIL) <- c("date", "OIL")
OIL$date <- as.Date(OIL$date, "%m/%d/%Y")

NATGAS <- read.csv("../data/NG.csv", header = F)
names(NATGAS) <- c("date", "NATGAS")
NATGAS$date <- as.Date(NATGAS$date, "%m/%d/%Y")

df <- merge(OIL,NATGAS)
df <- turn_to_monthly(df)

df <- df[df$date <= as.Date(end_day),]
df <- df[df$date >= as.Date(start_day),]

df$logOIL <- log(df$OIL)
df$logNATGAS <- log(df$NATGAS)

p <- ggplot(df, aes(x=date)) +
  geom_line(mapping = aes(y=logNATGAS - mean(logNATGAS),color="NATGAS")) +
  geom_line(mapping = aes(y=logOIL - mean(logOIL),color="OIL")) +
  ylab("Log-price") + xlab("") + theme_minimal() +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ggtitle("Comparison of log-prices", subtitle = "(prices are mean-adjusted to account for different magnitude)")
show(p)


cat("Loaded monthly data from", as.character(df[1,"date"]), "to", as.character(df[nrow(df),"date"]))
message("")
message("")
