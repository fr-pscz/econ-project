library(vars)

VARselect(df[,c("logHENRY", "logBRENT")], lag.max = 8, type = "const")

m1 <- VAR(df[,c("logHENRY", "logBRENT")], p=1, type = "const")
show(summary(m1))

# Plot VAR ----
p <- ggplot(df[2:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=logHENRY), color="#06a11e") +
  geom_line(mapping = aes(y=m1$varresult$logHENRY$fitted.values), color="#06a11e", linetype="dashed") +
  geom_line(mapping = aes(y=logBRENT), color="#0c07a8") +
  geom_line(mapping = aes(y=m1$varresult$logBRENT$fitted.values), color="#0c07a8", linetype="dashed") +
  theme_minimal()
show(p)

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=diff(df$logHENRY[2:nrow(df)])), color="#06a11e") +
  geom_line(mapping = aes(y=diff(m1$varresult$logHENRY$fitted.values)), color="#06a11e", linetype="dashed") +
  geom_line(mapping = aes(y=diff(df$logBRENT[2:nrow(df)])), color="#0c07a8") +
  geom_line(mapping = aes(y=diff(m1$varresult$logBRENT$fitted.values)), color="#0c07a8", linetype="dashed") +
  theme_minimal()
show(p)

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=abs(diff(df$logHENRY[2:nrow(df)]) - diff(m1$varresult$logHENRY$fitted.values))), color="#06a11e") +
  geom_line(mapping = aes(y=abs(diff(df$logBRENT[2:nrow(df)]) - diff(m1$varresult$logBRENT$fitted.values))), color="#0c07a8") +
  theme_minimal()
show(p)

# VECM ----

m2 <- cajorls(ca.jo(df[,c("logBRENT", "logHENRY")], type = "trace", K=2, spec="transitory"))
show(summary(m2$rlm))

cumsum(c(df$logHENRY[1],m2$rlm$fitted.values[,2]))

p <- ggplot(df[2:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=logHENRY), color="#06a11e") +
  geom_line(mapping = aes(y=cumsum(c(df$logHENRY[1],m2$rlm$fitted.values[,2]))), color="#06a11e", linetype="dashed") +
  geom_line(mapping = aes(y=logBRENT), color="#0c07a8") +
  geom_line(mapping = aes(y=cumsum(c(df$logBRENT[1],m2$rlm$fitted.values[,1]))), color="#0c07a8", linetype="dashed") +
  theme_minimal()
show(p)

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=diff(df$logHENRY[2:nrow(df)])), color="#06a11e") +
  geom_line(mapping = aes(y=m2$rlm$fitted.values[,2]), color="#06a11e", linetype="dashed") +
  geom_line(mapping = aes(y=diff(df$logBRENT[2:nrow(df)])), color="#0c07a8") +
  geom_line(mapping = aes(y=m2$rlm$fitted.values[,1]), color="#0c07a8", linetype="dashed") +
  theme_minimal()
show(p)

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=abs(diff(df$logHENRY[2:nrow(df)]) - m2$rlm$fitted.values[,2])), color="#06a11e") +
  geom_line(mapping = aes(y=abs(diff(df$logBRENT[2:nrow(df)]) - m2$rlm$fitted.values[,1])), color="#0c07a8") +
  theme_minimal()
show(p)

# Comparison ----
errVAR  <- 0.5*(abs(diff(df$logHENRY[2:nrow(df)]) - diff(m1$varresult$logHENRY$fitted.values)) + abs(diff(df$logBRENT[2:nrow(df)]) - diff(m1$varresult$logBRENT$fitted.values)))
errVECM <- 0.5*(abs(diff(df$logHENRY[2:nrow(df)]) - m2$rlm$fitted.values[,2]) + abs(diff(df$logBRENT[2:nrow(df)]) - m2$rlm$fitted.values[,1]))

p <- ggplot(df[3:nrow(df),], aes(x=date)) +
  geom_line(mapping = aes(y=errVAR), color="#ff0000") +
  geom_line(mapping = aes(y=errVECM), color="#0000ff") +
  theme_minimal()
show(p)

(mean(errVECM) - mean(errVAR))/mean(errVAR)
