printFMT <- function(x) {
  stout <- c()
  for (st in x) {
    stout <- c(stout, format(format(st, digits=4), width = 12, justify = "centre"))
  }
  cat(stout,sep="")
  message("")
} 

turn_to_monthly <- function(data) {
  data_new <- data[0,]
  data_row <- data[1,]
  
  start_day <- floor_date(data$date[1], unit = "month")
  end_day   <- floor_date(data$date[nrow(data)]-1, unit = "month")
  
  ms <- seq(start_day, end_day, by="month")
  ms <- c(ms, data$date[nrow(data)])
  
  for (k in 1:(length(ms)-1)) {
    for (n in names(data_row)) {
      if (n=="date") {
        data_row[1,n] <- ms[k]
      } else {
        data_row[1,n] <- mean(data[
          (data$date >= ms[k]) & (data$date <= ms[k+1]),
          n
        ])
      }
    }
    data_new <- rbind(data_new,data_row)
  }
  data_new <- data_new
}