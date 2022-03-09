rm(list=ls());
base.path <- "/ocean/projects/eng200002p/mbruchon/"
#base.path <- 'U:/'
data.path <- paste0(base.path,"trips/")
.libPaths( c(paste0(base.path,"R_Libs"), .libPaths())) 
#install.packages(c("data.table","lubridate","bit64")
library(stats)
library(data.table)
library(ClustGeo)
library(ggplot2)
library(plyr)
library(gridExtra)
library(Matrix)
library(NMOF)
library(ggmap)
library(sf)
library(viridis)
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

segment.speeds <- fread("/ocean/projects/eng200002p/mbruchon/DataAnalysis/Congestion/Chicago_Traffic_Tracker_-_Historical_Congestion_Estimates_by_Segment_-_2018-Current.csv")
segment.speeds <- segment.speeds[DAY_OF_WEEK > 1 & DAY_OF_WEEK < 7]
segment.speeds[,':='(STREET=NULL,TO_STREET=NULL,FROM_STREET=NULL,MONTH=NULL,START_LOCATION=NULL,END_LOCATION=NULL)]
segment.speeds[,':='(lat=(START_LATITUDE+END_LATITUDE)/2.0,long=(END_LONGITUDE+START_LONGITUDE)/2.0)]
segment.speeds[,':='(START_LATITUDE=NULL,START_LONGITUDE=NULL,END_LATITUDE=NULL,END_LONGITUDE=NULL)]
segment.speeds[,':='(COMMENTS=NULL)]
segment.speeds[,date:=as.Date(substr(TIME,1,10),"%m/%d/%y")]
segment.speeds <- segment.speeds[year != 2021 & !(year==2020 & month(date)>3) & !(year==2020 & month(date)==3 & mday(date)>6)]
missing.segment = segment.speeds[,.(missing=sum(SPEED==-1,na.rm=TRUE),count=sum(BUS_COUNT,na.rm=TRUE),speed=mean(ifelse(SPEED>=0,SPEED,NA),na.rm=TRUE)),by=.(SEGMENT_ID,DAY_OF_WEEK,HOUR)]

rollup <- segment.speeds[,.(missing=sum(SPEED==-1,na.rm=TRUE),count=sum(BUS_COUNT,na.rm=TRUE),speed=mean(ifelse(SPEED>=0,SPEED,NA),na.rm=TRUE)),by=.(lat,long,date,hour=HOUR,downtown,SEGMENT_ID)]

write.csv(missing.segment,"/ocean/projects/eng200002p/mbruchon/DataAnalysis/Congestion/segment_rollup.csv")

saveRDS(segment.speeds,"/ocean/projects/eng200002p/mbruchon/DataAnalysis/Congestion/column_subset_congestion_segments_2020.RDS")
#1076: 925-25516
#segment.speeds <- readRDS("/ocean/projects/eng200002p/mbruchon/DataAnalysis/Congestion/column_subset_congestion_segments_2020.RDS")
downtown.zone <- st_read(paste0("/ocean/projects/eng200002p/mbruchon/trips/Shapefiles/Downtown.kml"))
rollup
long.lat <- unique(rollup[,.(x=long,y=lat)])
pnts_sf <- st_as_sf(long.lat, coords = c('x', 'y'), crs = st_crs(downtown.zone))
intersections <- lengths(st_intersects(pnts_sf,downtown.zone))>0
long.lat$downtown = intersections
trip_rollup_geo = readRDS(paste0(data.path,"outcomes_rollup_geo_20220221.RDS"))

rollup[long.lat,downtown:=downtown,on=.(long=x,lat=y)]
rollup <- rollup[date %in% trip_rollup_geo$start.date]
rollup <- rollup[hour > 5 & hour < 22]
rollup <- rollup[!is.na(speed)]
rollup[,date.rank:=frank(date,ties.method="dense")]
rollup[,week:=date.rank-date.rank%%5]
rollup[,week.date:=min(date),by=week]
rollup[speed==0,speed:=1]
rollup <- rollup[,.(speed=weighted.mean(speed,w=count),inv.speed=weighted.mean(1/speed,w=count)),by=.(week.date,downtown)]
write.csv(rollup,paste0(data.path,"segment_congestion_rollup.csv"))