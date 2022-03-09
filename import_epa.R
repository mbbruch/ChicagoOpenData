library(stringr)
library(sp)
library(sf)
library(geosphere)
library(data.table)
library(ggplot2)
library(viridis)
library(fixest)
library(ggridges)
library(forcats)
setwd("C:\\Users\\Matthew\\Desktop\\Data Analysis\\AirQuality\\Chicago_EPA\\")
swr = function(string, nwrap=10) {
     paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)
files = list.files(path=getwd(), pattern=".*1440_minute_average*",full.names=TRUE)
files=lapply(files, read.csv)
filenames <- list.files(path=getwd(), pattern="1440_minute_average*",full.names=FALSE)

epa_pm25 = rbindlist(list(
     data.table(read.csv(paste0(getwd(),"\\EPA_AQS_PM25_2019.xls"))),
     data.table(read.csv(paste0(getwd(),"\\EPA_AQS_PM25_2020.xls")))
))
epa_pm25[,new.date:=as.Date(Date, "%m/%d/%Y")]
epa <- unique(epa_pm25[,.(sensor=paste0(Site.ID,"_",Site.Name),lat=SITE_LATITUDE,long=SITE_LONGITUDE,
                          created_date=new.date,pm25=Daily.Mean.PM2.5.Concentration)])

purpleair <- unique(all.combos[,.(sensor,lat,long,created_date,pm25=PM2.5_CF1_ug.m3)])
combined <- rbindlist(list(purpleair,epa))
#combined <- epa
combined <- combined[,.(pm25=mean(pm25,na.rm=TRUE)),by=.(sensor,lat,long,created_date)]
downtown.zone <- st_read(paste0("C:\\Users\\Matthew\\Desktop\\Data Analysis\\Shapefiles\\",
                                "Downtown.kml"))
downtown.edge <- as.matrix(downtown.zone$geometry[[1]])[,1:2]

#all.combos <- all.combos[year(created_at) >= 2019 & year(created_at) <= 2020 & !is.na(long) & !is.na(lat)]
points <- unique(combined[,.(sensor,long,lat)])
dist.mat <- geosphere::dist2Line(p = as.matrix(points[,.(long,lat)]), line = downtown.edge)
points[,distance:=dist.mat[,1]]
points[sensor %in% c("1138 PLYMOUTH","ODE"),distance:=distance*-1]
combined <- combined[points,distance.from.downtown:=distance,on=.(sensor)]
combined[,pm25:=pm25/mean(pm25,na.rm=TRUE),by=sensor]
combined[,weekday:=wday(created_date)]


library(tidyr)
#
#(year(created_date) == 2019) | (year(created_date)==2020 & created_date <'2020-03-06')
#(year(created_date) == 2019 & created_date >'2019-11-05') | (year(created_date)==2020 & created_date <'2020-03-08')
rollup <- combined[(year(created_date) == 2019 & created_date >'2019-11-05') | (year(created_date)==2020 & created_date <'2020-03-08'),
                     .(notmissing=sum(!is.na(pm25))),by=.(created_date,sensor,lat,long,distance.from.downtown)]

distances <- unique(rollup[,.(sensor,lat,long,distance.from.downtown)])
temp=crossing(rollup$sensor,rollup$created_date)
temp <- data.table(sensor=temp$`rollup$sensor`,
                   created_date=temp$`rollup$created_date`)
rollup <- temp[rollup,':='(notmissing=notmissing),
               on=.(created_date,sensor)]
rollup[is.na(notmissing),notmissing:=0]
rollup[,percent.nonmissing:=mean(notmissing),by=sensor]
rollup[distances,':='(distance.from.downtown=distance.from.downtown,lat=lat,long=long),on=.(sensor)]
rollup$sensor <- factor(rollup$sensor)
rollup$sensor <- fct_reorder(rollup$sensor, rollup$distance.from.downtown, max)
ggplot(rollup, aes(x=created_date,y=sensor,height=notmissing,fill=distance.from.downtown/1000)) + 
     geom_ridgeline() +
     theme_light() +
     scale_fill_viridis(discrete = FALSE,name = "km from\ndowntown") + 
     #geom_vline(xintercept = as.Date('2020-01-06'),linetype="dashed",color="red",size=1) +
     #geom_vline(xintercept = as.Date('2020-03-07'),linetype="dashed",color="red",size=1) +
     ggtitle("Each EPA sensor's non-missing days")

sensor.rollup <- unique(rollup[,.(sensor,lat,long,distance.from.downtown,percent.nonmissing)])
sensor.rollup <- sensor.rollup[percent.nonmissing>=0.3 & distance.from.downtown/1000 <= 84]
sensor.rollup[,type:=ifelse(is.na(str_length(str_extract(sensor, "[0-9]{9}"))),"PurpleAir","EPA")]
lat.padding <- sensor.rollup[,max(lat)-min(lat)]*0.05
long.padding <- sensor.rollup[,max(long)-min(long)]*0.05
min.lat <- sensor.rollup[,min(lat)] - lat.padding
max.lat <- sensor.rollup[,max(lat)] + lat.padding
min.long <- sensor.rollup[,min(long)] - long.padding
max.long <- sensor.rollup[,max(long)] + long.padding
area <- c(left = min.long, bottom = min.lat, right = max.long, top = max.lat)

library(ggmap); library(ggplot2); library(sf); library(viridis)
ph_basemap <- get_stamenmap(area, zoom=10, maptype = 'toner-lite', fill="bw", force=T)
ggmap(ph_basemap) + 
        geom_sf(downtown.zone, mapping=aes(fill="red"), inherit.aes = FALSE, show.legend=FALSE) +
        geom_point(data=sensor.rollup, mapping=aes(x = long, y = lat,  colour = percent.nonmissing,shape=type), size = 4) +
        #scale_colour_viridis(discrete = FALSE,name = "% non-\nmissing") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        scalebar(sensor.rollup, dist = 5, dist_unit = "mi", transform = TRUE, model = "WGS84")

downtown.sf <- data.table(lat=downtown.edge[,2],lon=downtown.edge[,1])
        

rollup[,':='(missing.days=sum(notmissing==0)),by=sensor]
sensors.to.use <- as.character(unique(rollup[missing.days<15]$sensor))
trends <- combined[(as.character(sensor) %in% sensors.to.use) & ((year(created_date) == 2019 & created_date >'2019-11-05') | (year(created_date)==2020 & created_date <'2020-01-06')),
                     .(pm25,created_date,sensor,distance.from.downtown)]
trends[,':='(pm25 = pm25/max(pm25,na.rm=TRUE)),by=sensor]
trends$sensor <- as.character(trends$sensor)
trends$sensor <- fct_reorder(trends$sensor, trends$distance.from.downtown, max)
ggplot(trends, aes(x=created_date,y=sensor,height=pm25,fill=distance.from.downtown/1000)) + 
        geom_ridgeline() +
        theme_light() +
        scale_fill_viridis(discrete = FALSE,name = "km from\ndowntown") + 
        ggtitle("Each sensor's pre-treatment PM2.5 trends")
