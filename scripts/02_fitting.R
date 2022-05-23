library(vars)

VARselect(df[,c("logHENRY", "logBRENT")], lag.max = 8, type = "const")

m1 <- VAR(df[,c("logHENRY", "logBRENT")], p=1, type = "const")
show(summary(m1))

m2 <- cajorls(ca.jo(df[,c("logBRENT", "logHENRY")], type = "trace", K=2))
show(summary(m2$rlm))