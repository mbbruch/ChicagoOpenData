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
setwd("C:\\Users\\Matthew\\Desktop\\Data Analysis\\AirQuality\\Chicago_PurpleAir\\")
swr = function(string, nwrap=10) {
        paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)
files = list.files(path=getwd(), pattern=".*1440_minute_average*",full.names=TRUE)
files=lapply(files, read.csv)
filenames <- list.files(path=getwd(), pattern="1440_minute_average*",full.names=FALSE)
sensor.names <- substr(str_extract(filenames, ".*?[\\s][(]"),1,nchar(str_extract(filenames, ".*?[\\s][(]"))-2)
sensor.letter <- ifelse(substr(sensor.names,nchar(sensor.names)-1,nchar(sensor.names))==" B",
                        "B","A")
sensor.names <- ifelse(substr(sensor.names,nchar(sensor.names)-1,nchar(sensor.names))==" B",
                       substr(sensor.names,1,nchar(sensor.names)-2),sensor.names)
sensor.outside <- str_extract(filenames, "[(][a-z|A-Z]*?[)]")
sensor.outside <- substr(sensor.outside,2,nchar(sensor.outside)-1)
sensor.ary <- str_extract(filenames, "[a-z|A-Z]*ary")
sensor.lats = str_extract(filenames, "4\\d[.]\\d+")
sensor.longs = str_extract(filenames, "-\\d+[.]\\d+")
for (i in 1:length(files)){
        if(nrow(files[[i]])>0){
                files[[i]]$sensor <- sensor.names[i]
                files[[i]]$letter <- sensor.letter[i]
                files[[i]]$primary <- sensor.ary[i]
                files[[i]]$outside <- sensor.outside[i]
                files[[i]]$lat <- as.numeric(sensor.lats[i])
                files[[i]]$long <- as.numeric(sensor.longs[i])
        }
}

primary <- rbindlist(files[sensor.ary=="Primary"],fill=TRUE)
secondary <- rbindlist(files[sensor.ary=="Secondary"],fill=TRUE)
primary[,':='(outdoor=max(outside=="outside")),by=.(sensor,lat,long)][,':='(outside=NULL,primary=NULL,letter=NULL)]
secondary[,':='(outdoor=as.integer(max(outside=="outside"))),by=.(sensor,lat,long)][,':='(outside=NULL,primary=NULL,letter=NULL)]

fixup <- function(x) {fifelse(min(x,na.rm=TRUE)<=0, max(x,na.rm=TRUE),mean(x,na.rm=TRUE))}
primary <- primary[, lapply(.SD, fixup), by=.(sensor,lat,long,created_at,outdoor)]
secondary <- secondary[, lapply(.SD, fixup), by=.(sensor,lat,long,created_at,outdoor)]
all.combos <- unique(rbindlist(list(primary[,.(sensor,lat,long,created_at,outdoor)],
                                    secondary[,.(sensor,lat,long,created_at,outdoor)])))
all.combos <- all.combos[primary,on=.(sensor,lat,long,created_at,outdoor)]
all.combos <- all.combos[secondary,on=.(sensor,lat,long,created_at,outdoor)]

all.combos[,created_date:=as.Date(created_at)]
#all.combos <- all.combos[year(created_date)==2019 | (year(created_date)==2020 & created_date <'2020-03-07')]

downtown.zone <- st_read(paste0("C:\\Users\\Matthew\\Desktop\\Data Analysis\\Shapefiles\\",
                                "Downtown.kml"))
downtown.edge <- as.matrix(downtown.zone$geometry[[1]])[,1:2]

all.combos[,date:=substr(created_at,1,10)]
all.combos[,time:=substr(created_at,12,19)]
all.combos[,date.string:=as.character(created_at)]
all.combos[,created_date:=as.Date(created_at)]
#all.combos <- all.combos[year(created_at) >= 2019 & year(created_at) <= 2020 & !is.na(long) & !is.na(lat)]
points <- unique(all.combos[,.(sensor,long,lat)])
dist.mat <- geosphere::dist2Line(p = as.matrix(points[,.(long,lat)]), line = downtown.edge)
points[,distance:=dist.mat[,1]]
points[sensor %in% c("1138 PLYMOUTH","ODE"),distance:=distance*-1]
all.combos <- all.combos[points,distance.from.downtown:=distance,on=.(sensor)]
all.combos[,pm25:=PM2.5_ATM_ug.m3/mean(PM2.5_ATM_ug.m3,na.rm=TRUE),by=sensor]
all.combos[,weekday:=wday(created_date)]
weekly <- all.combos[,.(PM2.5_CF1_ug.m3=mean(PM2.5_CF1_ug.m3,na.rm=TRUE)),by=.(sensor, distance.from.downtown,weekday)]
ggplot(all.combos, aes(x=created_date, y=log(PM2.5_ATM_ug.m3), group=sensor)) +
        geom_line(alpha=0.15) +
        theme_light() +
        #scale_color_viridis(discrete = FALSE) +
        ggtitle("PM2.5 at each PurpleAir sensor") +
        ylab("log(PM2.5)")
#created_date <= as.Date('2021-07-01')
#

library(tidyr)
#
#(year(created_date) == 2019) | (year(created_date)==2020 & created_date <'2020-03-06')
#(year(created_date) == 2019 & created_date >'2019-11-05') | (year(created_date)==2020 & created_date <'2020-03-08')
rollup <- all.combos[,
                     .(notmissing=sum(!is.na(PM2.5_CF1_ug.m3))),by=.(created_date,sensor,distance.from.downtown)]
distances <- unique(rollup[,.(sensor,distance.from.downtown)])
temp=crossing(rollup$sensor,rollup$created_date)
temp <- data.table(sensor=temp$`rollup$sensor`,
                   created_date=temp$`rollup$created_date`)
rollup <- temp[rollup,':='(notmissing=notmissing),
               on=.(created_date,sensor)]
rollup[is.na(notmissing),notmissing:=0]
rollup[distances,distance.from.downtown:=distance.from.downtown,on=.(sensor)]
rollup$sensor <- factor(rollup$sensor)
rollup$sensor <- fct_reorder(rollup$sensor, rollup$distance.from.downtown, max)
ggplot(rollup, aes(x=created_date,y=sensor,height=notmissing,fill=distance.from.downtown/1000)) + 
        geom_ridgeline() +
        theme_light() +
        scale_fill_viridis(discrete = FALSE,name = "km from\ndowntown") + 
        #geom_vline(xintercept = as.Date('2020-01-06'),linetype="dashed",color="red",size=1) +
        #geom_vline(xintercept = as.Date('2020-03-07'),linetype="dashed",color="red",size=1) +
        ggtitle("Each PurpleAir sensor's non-missing days")

rollup[,':='(missing.days=sum(notmissing==0)),by=sensor]
sensors.to.use <- as.character(unique(rollup[missing.days==0]$sensor))
trends <- all.combos[(as.character(sensor) %in% sensors.to.use) & ((year(created_date) == 2019 & created_date >'2019-11-05') | (year(created_date)==2020 & created_date <'2020-01-06')),
                     .(PM2.5_CF1_ug.m3,created_date,sensor,distance.from.downtown)]
trends[,':='(pm25 = PM2.5_CF1_ug.m3/max(PM2.5_CF1_ug.m3)),by=sensor]
trends$sensor <- as.character(trends$sensor)
trends$sensor <- fct_reorder(trends$sensor, trends$distance.from.downtown, max)
ggplot(trends, aes(x=created_date,y=sensor,height=pm25,fill=distance.from.downtown/1000)) + 
        geom_ridgeline() +
        theme_light() +
        scale_fill_viridis(discrete = FALSE,name = "km from\ndowntown") + 
        ggtitle("Each PurpleAir sensor's pre-treatment PM2.5 trends")

all.combos[,':='(pm25 = PM2.5_CF1_ug.m3/max(PM2.5_CF1_ug.m3,na.rm=TRUE)),by=sensor]
trends <- all.combos[(as.character(sensor) %in% sensors.to.use) & ((year(created_date) == 2019 & created_date >'2019-01-05') | (year(created_date)==2020 & created_date <'2020-01-06')),
                     .(pm25=mean(pm25,na.rm=TRUE)),by=.(weekday,sensor,distance.from.downtown)]
trends[,':='(pm25 = pm25/max(pm25,na.rm=TRUE)),by=sensor]
trends$sensor <- as.character(trends$sensor)
trends$sensor <- fct_reorder(trends$sensor, trends$distance.from.downtown, max)
ggplot(trends, aes(x=weekday,y=sensor,height=pm25,fill=distance.from.downtown/1000)) + 
        geom_ridgeline() +
        theme_light() +
        scale_fill_viridis(discrete = FALSE,name = "km from\ndowntown") + 
        ggtitle("Each PurpleAir sensor's pre-treatment PM2.5 trends")

        
ggplot(rollup, aes(x=created_date,y=sensor,height=notmissing)) + 
        geom_ridgeline() 

all.combos[,treatment:=ifelse(created_at>=as.Date('2020-01-06'),distance.from.downtown,0)]

model.formula <- log(X..5.0um.dl) ~ treatment | sensor + date.string + time + created_at + Humidity_. + Temperature_F
# this will need some tweaking for clustered standard errors probably
model = feols(model.formula, data=all.combos)#, weights=training.set$OD_Avg_Trips)
summary(model)

