library(urca)
library(tseries)

# -----
# ERS unit root 
# H0: nonstationarity

message("==============")
message(" BRENT OIL")
message("==============")
show(summary(ur.ers(BRENT$USD,type = "P-test", model = "trend")))
message("Can not reject nonstationarity at 5%")

message("==============")
message(" HENRY NATGAS")
message("==============")

show(summary(ur.ers(HENRY$USD,type = "P-test", model = "trend")))
message("Stationarity significant at 5%")

message("Dropping the highly anomalous day of 17 Feb 2021:")
show(summary(ur.ers(HENRY$USD[HENRY$USD<20],type = "P-test", model = "trend")))

message("Also dropping the spike in the early 2000s:")
show(summary(ur.ers(HENRY$USD[HENRY$USD<17],type = "P-test", model = "trend")))

# -----
# ADF

adf.test(BRENT$USD)
adf.test(HENRY$USD[HENRY$USD<20])

