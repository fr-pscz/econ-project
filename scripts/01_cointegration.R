library(urca)
library(aTSA)
library(tseries)
# Nonstationarity

show(adf.test(df$logHENRY))
show(adf.test(df$logBRENT))

show(kpss.test(df$logHENRY))
show(kpss.test(df$logBRENT))

# Cointegration
show(adf.test(residuals(lm(logHENRY ~ logBRENT, data=df))))

# Johansen
show(summary(ca.jo(df[,c("logBRENT", "logHENRY")], type = "trace", K=6)))

# Engle-Granger
#show(coint.test(df[,"logBRENT"],df[,"logHENRY"],d=0,nlag = 6, output = T))

           