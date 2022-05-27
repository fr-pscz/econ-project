message("================")
message("2. MODEL FITTING")
message("================")

message("Vector autoregressive model")
message("---------------------------")

nlag <- VARselect(df[,c("logNATGAS", "logOIL")], lag.max = 8, type = "const")$selection["SC(n)"]
m1 <- VAR(df[,c("logNATGAS", "logOIL")], p=nlag, type = "const")
VAR.m1 <- summary(m1)
message(">>> logNATGAS model")
show(VAR.m1$varresult$logNATGAS$coefficients)
message("")
message(">>> logOIL model")
show(VAR.m1$varresult$logOIL$coefficients)

# Plot VAR ----
p <- ggplot(df[(1+nlag):nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=logNATGAS, color="NATGAS"), alpha = 0.5) +
  geom_line(mapping = aes(y=m1$varresult$logNATGAS$fitted.values, color="NATGAS")) +
  geom_line(mapping = aes(y=logOIL, color="OIL"), alpha = 0.5) +
  geom_line(mapping = aes(y=m1$varresult$logOIL$fitted.values, color="OIL")) +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ylab("Log-price") + xlab("") +
  ggtitle("Fitted log-prices (VAR)") +
  theme_minimal()
show(p)

p <- ggplot(df[(2+nlag):nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=diff(df$logNATGAS[(1+nlag):nrow(df)]), color="NATGAS"), alpha = 0.5) +
  geom_line(mapping = aes(y=diff(m1$varresult$logNATGAS$fitted.values), color="NATGAS")) +
  geom_line(mapping = aes(y=diff(df$logOIL[(1+nlag):nrow(df)]), color="OIL"), alpha = 0.5) +
  geom_line(mapping = aes(y=diff(m1$varresult$logOIL$fitted.values), color="OIL")) +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ylab("Daily log-change") + xlab("") +
  ggtitle("Fitted daily changes (VAR)") +
  theme_minimal()
show(p)

p <- ggplot(df[(2+nlag):nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=abs(diff(df$logNATGAS[(1+nlag):nrow(df)]) - diff(m1$varresult$logNATGAS$fitted.values)), color="NATGAS")) +
  geom_line(mapping = aes(y=abs(diff(df$logOIL[(1+nlag):nrow(df)]) - diff(m1$varresult$logOIL$fitted.values)), color="OIL")) +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ylab("Error") + xlab("") + ylim(0,1.5) +
  ggtitle("Absolute error in daily changes (VAR)") +
  theme_minimal()
show(p)

# VECM ----
message("")
message("Vector error correction model")
message("-----------------------------")

m2 <- cajorls(ca.jo(df[,c("logOIL", "logNATGAS")], type = "trace", K=2, spec="transitory"))
VECM.m2 <- summary(m2$rlm)

message(">>> Daily change in logNATGAS model")
show(VECM.m2$`Response logNATGAS.d`$coefficients)
message("")
message(">>> Daily change in logOIL model")
show(VECM.m2$`Response logOIL.d`$coefficients)

p <- ggplot(df[2:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=logNATGAS, color="NATGAS"), alpha=0.5) +
  geom_line(mapping = aes(y=cumsum(c(df$logNATGAS[1],m2$rlm$fitted.values[,2])), color="NATGAS")) +
  geom_line(mapping = aes(y=logOIL, color="OIL"), alpha=0.5) +
  geom_line(mapping = aes(y=cumsum(c(df$logOIL[1],m2$rlm$fitted.values[,1])), color="OIL")) +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ylab("Log-price") + xlab("") +
  ggtitle("Fitted log-prices (VECM)") +
  theme_minimal()
show(p)

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=diff(df$logNATGAS[2:nrow(df)]), color="NATGAS"),alpha=0.5) +
  geom_line(mapping = aes(y=m2$rlm$fitted.values[,2], color="NATGAS")) +
  geom_line(mapping = aes(y=diff(df$logOIL[2:nrow(df)]), color="OIL"),alpha=0.5) +
  geom_line(mapping = aes(y=m2$rlm$fitted.values[,1], color="OIL")) +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ylab("Daily log-change") + xlab("") +
  ggtitle("Fitted daily changes (VECM)") +
  theme_minimal()
show(p)

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=abs(diff(df$logNATGAS[2:nrow(df)]) - m2$rlm$fitted.values[,2]), color="NATGAS")) +
  geom_line(mapping = aes(y=abs(diff(df$logOIL[2:nrow(df)]) - m2$rlm$fitted.values[,1]), color="OIL")) +
  scale_colour_manual("", breaks = c("NATGAS", "OIL"), values = c(gasColor, oilColor)) +
  ylab("Error") + xlab("") + ylim(0,1.5) +
  ggtitle("Absolute error in daily changes (VECM)") +
  theme_minimal()
show(p)

# Comparison ----

errVAR  <- 0.5*(abs(
  diff(df$logNATGAS[(1+nlag):nrow(df)]) - diff(m1$varresult$logNATGAS$fitted.values)
  ) + 
    abs(
      diff(df$logOIL[(1+nlag):nrow(df)]) - diff(m1$varresult$logOIL$fitted.values)
  ))
errVECM <- 0.5*(abs(
  diff(df$logNATGAS[(1+nlag):nrow(df)]) - m2$rlm$fitted.values[nlag:(nrow(df)-2),2]
  ) + 
    abs(
      diff(df$logOIL[(1+nlag):nrow(df)]) - m2$rlm$fitted.values[nlag:(nrow(df)-2),1]
  ))

p <- ggplot(df[(2+nlag):nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=errVAR, color="VAR")) +
  geom_line(mapping = aes(y=errVECM, color="VECM")) +
  scale_colour_manual("", breaks = c("VAR", "VECM"), values = c(varColor, vecColor)) +
  ylab("Error") + xlab("") +
  ggtitle("Model comparison") +
  theme_minimal()
show(p)

message("")
message("Improvement of VECM over VAR")
message("----------------------------")
message(">>> Mean absolute error in first differences")
printFMT(c("MAE(VAR)","MAE(VECM)","Gain(%)"))
printFMT(c(mean(errVAR), mean(errVECM), (mean(errVECM) - mean(errVAR))*100/mean(errVAR)))
message("")
