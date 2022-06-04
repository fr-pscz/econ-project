message("====================")
message("3. STRUCTURAL CHANGE")
message("====================")

# contains formulas for the one-step Chow test
source("explicitVAR.R")


# OIL ----
OIL.F <- numeric(nrow(df)-13)
OIL.P <- numeric(nrow(df)-13)
idx   <- 0
s     <- 11

tmp.m <- lm(VAR.OIL,data=datachow[1:s,])
tmp.RSS1 <- sum((tmp.m$residuals)^2)
for (s in 12:nrow(df)) {
  idx <- idx + 1
  tmp.m <- lm(VAR.OIL,data=datachow[1:s,])
  tmp.RSS <- sum((tmp.m$residuals)^2)
  OIL.F[idx] <- ((tmp.RSS - tmp.RSS1)*(s - 3 - 1)/tmp.RSS1)/qf(0.99,1,s-3-1)
  OIL.P[idx] <- pf(OIL.F[idx],1,s-3-1,lower.tail = F)
  tmp.RSS1 <- tmp.RSS
}

# NATGAS ----
NATGAS.F <- numeric(nrow(df)-13)
NATGAS.P <- numeric(nrow(df)-13)
idx <- 0
s <- 11

tmp.m <- lm(VAR.NATGAS,data=datachow[1:s,])
tmp.RSS1 <- sum((tmp.m$residuals)^2)
for (s in 12:nrow(df)) {
  idx <- idx + 1
  tmp.m <- lm(VAR.NATGAS,data=datachow[1:s,])
  tmp.RSS <- sum((tmp.m$residuals)^2)
  NATGAS.F[idx] <- ((tmp.RSS - tmp.RSS1)*(s - 3 - 1)/tmp.RSS1)/qf(0.99,1,s-3-1)
  NATGAS.P[idx] <- pf(NATGAS.F[idx],1,s-3-1,lower.tail = F)
  tmp.RSS1 <- tmp.RSS
}

# Plots ----
p <- ggplot(df[12:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=NATGAS.F,color="NATGAS")) +
  geom_line(mapping = aes(y=OIL.F,color="OIL")) +
  ylab("F-statistic") + xlab("") + theme_minimal() + geom_hline(yintercept = 1) +
  ggtitle("1-step Chow test", subtitle = "horizontal line at 1% significance") +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor))
show(p)

n <- nrow(datachow) - 7

Fchow.NATGAS <- as.numeric(Fstats(VAR.NATGAS,data=datachow,from=8,to=n)$Fstats/boundary(Fstats(VAR.NATGAS,data=datachow,from=8,to=n),alpha=0.01))
Fchow.OIL    <- as.numeric(Fstats(VAR.OIL,data=datachow,from=8,to=n)$Fstats/boundary(Fstats(VAR.OIL,data=datachow,from=8,to=n),alpha=0.01))

p <- ggplot(df[8:n,], aes(x=date)) +
  geom_line(mapping = aes(y=Fchow.NATGAS,color="NATGAS")) +
  geom_line(mapping = aes(y=Fchow.OIL,color="OIL")) +
  ylab("F-statistic") + xlab("") + theme_minimal() + geom_hline(yintercept = 1) +
  ggtitle("Breakpoint Chow test", subtitle = "horizontal line at 1% significance") +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor))
show(p)


F.NATGAS <- Fstats(VAR.NATGAS,data=datachow,from=8,to=n)$Fstats/boundary(Fstats(VAR.NATGAS,data=datachow,from=8,to=n),alpha=0.01)
nbreak.NATGAS <- which(Fchow.NATGAS>1)[1]
nbreak.OIL    <- which(Fchow.OIL>1)[1]
message("Date of first structural break")
message("------------------------------")
message(">>> Breakpoint Chow test :: first break")
printFMT(c("NATGAS", "OIL"))
printFMT(c(as.character(datachow[nbreak.NATGAS+7,"date"]),as.character(datachow[nbreak.OIL+7,"date"])))
