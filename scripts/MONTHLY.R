data.monthly <- data[seq(1,nrow(data),20),]
data.yearly <- data.monthly[seq(1,nrow(data.monthly),10),]

p <- ggplot(data.yearly, aes(x=date)) +
  geom_line(mapping = aes(y=logBRENT), color="#0c07a8") +
  geom_line(mapping = aes(y=logNATGAS), color="#06a11e") +
  xlab("") +
  theme_minimal()
show(p)


data.var <- data.monthly[,c("logBRENT","logNATGAS")]
yVARIC <- VARselect(data.var, type = "const")
t(yVARIC$criteria)
nlag <- 7

data.H1 <- data.var[1:120,]
data.H2 <- data.var[3300:6300,]

summary(ca.jo(data.H1,type="trace",K=nlag))

summary(ca.jo(data.var,type="eigen",K=nlag))

summary(coint.test(data.monthly$logBRENT,data.monthly$logNATGAS,d=0,nlag = nlag, output = T))

