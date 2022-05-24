VAR.HENRY <- logHENRY ~ logHENRY.l1 + logBRENT.l1
VAR.BRENT <- logBRENT ~ logHENRY.l1 + logBRENT.l1
datachow <- df[2:nrow(df),c("logHENRY","logBRENT")]
datachow$logHENRY.l1 <- df[1:(nrow(df)-1),c("logHENRY")]
datachow$logBRENT.l1 <- df[1:(nrow(df)-1),c("logBRENT")]

VAR.HENRY <- logHENRY ~ logHENRY.l1 + logBRENT.l1 + logHENRY.l2 + logBRENT.l2
VAR.BRENT <- logBRENT ~ logHENRY.l1 + logBRENT.l1 + logHENRY.l2 + logBRENT.l2 + COVID
datachow <- df[3:nrow(df),c("logHENRY","logBRENT","date")]
datachow$logHENRY.l1 <- df[2:(nrow(df)-1),c("logHENRY")]
datachow$logBRENT.l1 <- df[2:(nrow(df)-1),c("logBRENT")]
datachow$logHENRY.l2 <- df[1:(nrow(df)-2),c("logHENRY")]
datachow$logBRENT.l2 <- df[1:(nrow(df)-2),c("logBRENT")]
datachow$trend       <- 1:(nrow(df)-2)
datachow$COVID       <- numeric(nrow(datachow))

datachow$COVID[(datachow$date >= "2020-03-01") & (datachow$date <= "2020-04-30")] <- 1

