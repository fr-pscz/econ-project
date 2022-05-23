library(vars)

x <- BRENT
names(x) <- c("date","BRENT","logBRENT")
y <- HENRY
names(y) <- c("date","NATGAS","logNATGAS")
data <- merge(x,y)
data <- data[data$NATGAS<17,]
data$NATGAS <- data$NATGAS * 15
data$logNATGAS <- log(data$NATGAS)

p <- ggplot(data, aes(x=date)) +
  geom_line(mapping = aes(y=logBRENT), color="#0c07a8") +
  geom_line(mapping = aes(y=logNATGAS), color="#06a11e") +
  xlab("")
  theme_minimal()
show(p)

data.var <- data[,c("logBRENT","logNATGAS")]
yVARIC <- VARselect(data.var, type = "const")
t(yVARIC$criteria)
# 3
nlag <- 10

data.H1 <- data.var[1:3300,]
data.H2 <- data.var[3300:6300,]

summary(ca.jo(data.H1,type="trace",K=nlag))

summary(ca.jo(data.var,type="eigen",K=nlag))

summary(coint.test(data.H1$logBRENT,data.H1$logNATGAS,d=0,nlag = nlag, output = T))
