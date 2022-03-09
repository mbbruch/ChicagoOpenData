library(data.table)

folder <- "C:\\Users\\Matthew\\Desktop\\Data Analysis\\"

weather <- fread(paste0(folder,"NOAA_OHare_2656899.csv"))
#weather <- fread(paste0(folder,"NOAA_Midway_2656900.csv"))
weather[,date.only:=substr(DATE,1,10)]
weather <- weather[REPORT_TYPE %like% "SOD"]
weather <- weather[, .SD, .SDcols = (names(weather) %like% "Daily" | names(weather)=='date.only')]
weather <- unique(weather)
weather <- weather[,which(unlist(lapply(weather, function(x)!all(is.na(x))))),with=F]