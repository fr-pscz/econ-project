VAR.NATGAS <- logNATGAS ~ logNATGAS.l1 + logOIL.l1
VAR.OIL <- logOIL ~ logNATGAS.l1 + logOIL.l1
datachow <- df[2:nrow(df),c("logNATGAS","logOIL")]
datachow$logNATGAS.l1 <- df[1:(nrow(df)-1),c("logNATGAS")]
datachow$logOIL.l1 <- df[1:(nrow(df)-1),c("logOIL")]

VAR.NATGAS <- logNATGAS ~ logNATGAS.l1 + logOIL.l1 + logNATGAS.l2 + logOIL.l2
VAR.OIL <- logOIL ~ logNATGAS.l1 + logOIL.l1 + logNATGAS.l2 + logOIL.l2 + COVID
datachow <- df[3:nrow(df),c("logNATGAS","logOIL","date")]
datachow$logNATGAS.l1 <- df[2:(nrow(df)-1),c("logNATGAS")]
datachow$logOIL.l1 <- df[2:(nrow(df)-1),c("logOIL")]
datachow$logNATGAS.l2 <- df[1:(nrow(df)-2),c("logNATGAS")]
datachow$logOIL.l2 <- df[1:(nrow(df)-2),c("logOIL")]
datachow$trend       <- 1:(nrow(df)-2)
datachow$COVID       <- numeric(nrow(datachow))

datachow$COVID[(datachow$date >= "2020-03-01") & (datachow$date <= "2020-04-30")] <- 1

