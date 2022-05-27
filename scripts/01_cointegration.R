message("================")
message("1. COINTEGRATION")
message("================")
message("Testing whether series are stationary")
message("-------------------------------------")

# ----
# Test for I(0)

message(">>> ADF :: p-values")
message(">>>      | H0 : non-stationary")
message(">>>      | H1 : stationary")

ADF.NATGAS.p <- adf.test(df$logNATGAS)$p.value
ADF.OIL.p    <- adf.test(df$logOIL)$p.value

printFMT(c("log-NATGAS","log-OIL"))
printFMT(c(ADF.NATGAS.p,ADF.OIL.p))
message("")


message(">>> KPSS :: p-values")
message(">>>      | H0 : stationary")
message(">>>      | H1 : non-stationary")

KPSS.NATGAS.p <- suppressWarnings(kpss.test(df$logNATGAS)$p.value)
KPSS.OIL.p    <- suppressWarnings(kpss.test(df$logOIL)$p.value)

printFMT(c("log-NATGAS","log-OIL"))
printFMT(c(KPSS.NATGAS.p,KPSS.OIL.p))
message("(p-values smaller than 1% are printed as 1%)")
message("")

message("Testing residuals of regression for cointegration")
message("-------------------------------------------------")

message(">>> ADF :: p-values")
message(">>>      | H0 : non-stationary")
message(">>>      | H1 : stationary")

ADF.LM.p <- adf.test(residuals(lm(logNATGAS ~ logOIL, data=df)))$p.value

printFMT("log-NATGAS ~ log-OIL")
cat("      ", format(ADF.LM.p,nsmall=4,digits=4))
message("")
message("")

message("Direct tests of cointegration")
message("-----------------------------")

# Johansen
message(">>> Johansen Trace Procedure :: output")
message(">>>    | r : number of cointegrating relationships")
JOPROC <- summary(ca.jo(df[,c("logOIL", "logNATGAS")], type = "trace", K=6, spec="transitory"))

printFMT(c("H0","T-stat","10%","5%","1%"))
printFMT(c(
  "r <= 1",
  format(JOPROC@teststat[1], digits=4),
  format(JOPROC@cval[1,1], digits=4),
  format(JOPROC@cval[1,2], digits=4),
  format(JOPROC@cval[1,3], digits=4)))
printFMT(c(
  "r == 0",
  format(JOPROC@teststat[2], digits=4),
  format(JOPROC@cval[2,1], digits=4),
  format(JOPROC@cval[2,2], digits=4),
  format(JOPROC@cval[2,3], digits=4)))
message("")

message(">>> Engle-Granger :: p-values")
message(">>>      | H0 : not cointegrated")
message(">>>      | H1 : cointegratedd")

EG.p <- coint.test(df[,"logNATGAS"],df[,"logOIL"],d=0,nlag = 1, output = F)
printFMT(c("no trend","linear","quadratic"))
printFMT(c(EG.p[1,3], EG.p[2,3], EG.p[3,3]))
message("(p-values larger than 10% are printed as 10%)")
message("")
           