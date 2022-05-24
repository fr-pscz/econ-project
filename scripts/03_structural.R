library(strucchange)


source("explicitVAR.R")
#show(plot(Fstats(VAR.HENRY,data = datachow),pval=T))

# BRENT 
BRENT.F <- numeric(nrow(df)-13)
BRENT.P <- numeric(nrow(df)-13)
idx <- 0
s <- 11

#tmp.m <- VAR(df[1:s,c("logHENRY", "logBRENT")], p=1, type = "const")
#tmp.RSS1 <- sum((tmp.m$varresult$logBRENT$residuals)^2)
tmp.m <- lm(VAR.BRENT,data=datachow[1:s,])
tmp.RSS1 <- sum((tmp.m$residuals)^2)

for (s in 12:nrow(df)) {
  idx <- idx + 1
  #tmp.m <- VAR(df[1:s,c("logHENRY", "logBRENT")], p=1, type = "const")
  #tmp.RSS <- sum((tmp.m$varresult$logBRENT$residuals)^2)
  tmp.m <- lm(VAR.BRENT,data=datachow[1:s,])
  tmp.RSS <- sum((tmp.m$residuals)^2)
  BRENT.F[idx] <- ((tmp.RSS - tmp.RSS1)*(s - 3 - 1)/tmp.RSS1)/qf(0.99,1,s-3-1)
  BRENT.P[idx] <- pf(BRENT.F[idx],1,s-3-1,lower.tail = F)
  tmp.RSS1 <- tmp.RSS
}

# HENRY
HENRY.F <- numeric(nrow(df)-13)
HENRY.P <- numeric(nrow(df)-13)
idx <- 0
s <- 11

#tmp.m <- VAR(df[1:s,c("logHENRY", "logBRENT")], p=1, type = "const")
tmp.m <- lm(VAR.HENRY,data=datachow[1:s,])
tmp.RSS1 <- sum((tmp.m$residuals)^2)

for (s in 12:nrow(df)) {
  idx <- idx + 1
  tmp.m <- lm(VAR.HENRY,data=datachow[1:s,])
  #tmp.m <- VAR(df[1:s,c("logHENRY", "logBRENT")], p=1, type = "const")
  #tmp.RSS <- sum((tmp.m$varresult$logHENRY$residuals)^2)
  tmp.RSS <- sum((tmp.m$residuals)^2)
  HENRY.F[idx] <- ((tmp.RSS - tmp.RSS1)*(s - 3 - 1)/tmp.RSS1)/qf(0.99,1,s-3-1)
  HENRY.P[idx] <- pf(HENRY.F[idx],1,s-3-1,lower.tail = F)
  tmp.RSS1 <- tmp.RSS
}


p <- ggplot(df[12:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=HENRY.F,color="HENRY")) +
  geom_line(mapping = aes(y=BRENT.F,color="BRENT")) +
  ylab("F-statistic") + xlab("") + theme_minimal() + geom_hline(yintercept = 1) +
  scale_colour_manual("", 
                      breaks = c("HENRY", "BRENT"),
                      values = c("#06a11e", "#0c07a8"))
show(p)


sctest(VAR.HENRY,type="Rec-CUSUM",data=datachow)
sctest(VAR.HENRY,type="Chow",data=datachow)

p <- ggplot(df[8:306,], aes(x=date)) +
  geom_line(mapping = aes(y=Fstats(VAR.HENRY,data=datachow,from=8,to=306)$Fstats/boundary(Fstats(VAR.HENRY,data=datachow,from=8,to=306),alpha=0.01),color="HENRY")) +
  geom_line(mapping = aes(y=Fstats(VAR.BRENT,data=datachow,from=8,to=306)$Fstats/boundary(Fstats(VAR.BRENT,data=datachow,from=8,to=306),alpha=0.01),color="BRENT")) +
  ylab("F-statistic") + xlab("") + theme_minimal() + geom_hline(yintercept = 1) +
  scale_colour_manual("", 
                      breaks = c("HENRY", "BRENT"),
                      values = c("#06a11e", "#0c07a8"))
show(p)



