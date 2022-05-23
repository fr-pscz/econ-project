library(urca)
library(tseries)

# -----
# ERS unit root 
# H0: nonstationarity

message("==============")
message(" BRENT OIL")
message("==============")
show(summary(ur.ers(log(BRENT$USD),type = "P-test", model = "trend")))
message("Can not reject nonstationarity at 5%")

message("==============")
message(" HENRY NATGAS")
message("==============")

show(summary(ur.ers(log(HENRY$USD),type = "P-test", model = "trend")))
message("Stationarity significant at 5%")

message("Dropping the highly anomalous day of 17 Feb 2021:")
show(summary(ur.ers(log(HENRY$USD[HENRY$USD<20]),type = "P-test", model = "trend")))

message("Also dropping the spike in the early 2000s:")
show(summary(ur.ers(log(HENRY$USD[HENRY$USD<17]),type = "P-test", model = "trend")))

# -----
# ADF

adf.test(log(BRENT$USD))
adf.test(log(HENRY$USD[HENRY$USD<20]))
#adf.test(log(HENRY$USD[3300:6363]))

# -----
# plot logs
plotC <- 1
BRENT$logUSD <- log(BRENT$USD)
HENRY$logUSD <- log(HENRY$USD)

p <- ggplot(WTI, aes(x=date, y=logUSD)) +
  geom_line(data = BRENT, color="#0c07a8") +
  geom_line(data = HENRY, mapping = aes(y=logUSD*plotC), color="#06a11e") +
  xlab("") +
  scale_y_continuous(name = "USD/bbl",
                     sec.axis = sec_axis(~./plotC, name="USD/Btu")) +
  theme_minimal()
show(p)

# -----
# KPSS

kpss.test(log(BRENT$USD))
kpss.test(log(HENRY$USD[HENRY$USD<17]))

d <- diff(log(HENRY$USD))
t <- seq(1,length(d))

p <- ggplot(mapping = aes(x=t, y=d)) +
  geom_line(color="#8c2303") +
  xlab("") +
  theme_minimal()
show(p)
