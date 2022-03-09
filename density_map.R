library(data.table)
library(stringr)
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)
library(bit64)
library(dtw)

folder <- "C:\\Users\\Matthew\\Desktop\\Data Analysis\\Shapefiles\\"
areas <- st_read(paste0(folder,"Boundaries - Community Areas (current)\\geo_export_6c2c6aa8-3ec3-445f-83dd-85a3ae13bfd3.shp"))
volumes <- fread("C:\\Users\\Matthew\\Desktop\\Chicago\\od_pre_treatment_volumes.csv")
downtown.areas <-  fread("C:\\Users\\Matthew\\Desktop\\Chicago\\downtown_areas.csv")
downtown.tracts <-  fread("C:\\Users\\Matthew\\Desktop\\Chicago\\downtown_tracts.csv")
special.locations <- fread("C:\\Users\\Matthew\\Desktop\\Chicago\\special_locations.csv")

downtown.areas <- rbindlist(list(downtown.areas,
                                 data.table(area=76,downtown.area=1),
                                 data.table(area=56,downtown.area=-1),
                                 data.table(area=33,downtown.area=-1)
                                 ))
downtown.tracts <- rbindlist(list(downtown.tracts,
                                  data.table(tract=as.integer64(17031980000),downtown.tract=1),
                                  data.table(tract=as.integer64(17031980100),downtown.tract=1),
                                  data.table(tract=as.integer64(17031330100),downtown.tract=-1),
                                  data.table(tract=as.integer64(17031841000),downtown.tract=-1),
                                  data.table(tract=as.integer64(17031081402),downtown.tract=1)
))

volumes[downtown.tracts,downtown.tract.start:=downtown.tract,on=.(start.tract=tract)][
     is.na(downtown.tract.start),downtown.tract.start:=0][
          downtown.tract.start==-1,downtown.tract.start:=as.numeric(NA)]
volumes[downtown.tracts,downtown.tract.end:=downtown.tract,on=.(end.tract=tract)][
     is.na(downtown.tract.end),downtown.tract.end:=0][
          downtown.tract.end==-1,downtown.tract.end:=as.numeric(NA)]
volumes[downtown.areas,downtown.area.start:=downtown.area,on=.(start.area=area)][
     is.na(downtown.area.start),downtown.area.start:=0][
          downtown.area.start==-1,downtown.area.start:=as.numeric(NA)]
volumes[downtown.areas,downtown.area.end:=downtown.area,on=.(end.area=area)][
     is.na(downtown.area.end),downtown.area.end:=0][
          downtown.area.end==-1,downtown.area.end:=as.numeric(NA)]
volumes[,downtown.start:=ifelse(downtown.tract.start==1 | downtown.area.start==1,1,
                                ifelse(downtown.tract.start==0 & downtown.area.start==0,0,NA))]
volumes[,downtown.end:=ifelse(downtown.tract.end==1 | downtown.area.end==1,1,
                                ifelse(downtown.tract.end==0 & downtown.area.end==0,0,NA))]
volumes[,downtown:=ifelse(downtown.start==1 | downtown.end==1,1,
                              ifelse(downtown.start==0 & downtown.end==0,0,NA))][is.na(downtown),downtown:=0.5]
volumes <- volumes[!is.na(start.lat) | !is.na(end.lat)]
volumes[,':='(string1=paste0(start.area,"_",start.tract),
              string2=paste0(end.area,"_",end.tract))]
volumes[,':='(string.1=fifelse(string1>string2,string1,string2),
              string.2=fifelse(string1>string2,string2,string1))]

unique.arcs <- unique(volumes[!is.na(start.lat) & !is.na(end.lat) & start.area !=end.area,.(string.1=fifelse(string1>string2,string1,string2),
           string.2=fifelse(string1>string2,string2,string1),                      
           area1=fifelse(string1>string2,start.area,end.area),
           area2=fifelse(string1>string2,end.area,start.area),
           tract1=fifelse(string1>string2,start.tract,end.tract),
           tract2=fifelse(string1>string2,end.tract,start.tract),
           lat1=fifelse(string1>string2,start.lat,end.lat),
           lat2=fifelse(string1>string2,end.lat,start.lat),
           long1=fifelse(string1>string2,start.long,end.long),
           long2=fifelse(string1>string2,end.long,start.long))])
rm(ods); rm(volumes)
string1 <- c(); string2 <- c(); intersections <- c()
for(i in 1:nrow(unique.arcs)){
     if(i%%1000==0){cat(paste0(i,"\n"))}
     this.row <- unique.arcs[i]
     this.arc <- st_linestring(rbind(c(this.row$long1,this.row$lat1),c(this.row$long2,this.row$lat2)))
     intersection.areas <- areas$area_num_1[st_intersects(this.arc, areas)[[1]]]
     if(length(intersection.areas>2)){
          for(j in 1:length(intersection.areas)){
               if(!(intersection.areas[j] %in% c(this.row$area1,this.row$area2))){
                    string1 <- c(string1,this.row$string.1)
                    string2 <- c(string2,this.row$string.2)
                    intersections <- c(intersections,as.integer(intersection.areas[j]))
               }
          }
     }
}
en.route <- data.table(intersection=intersections,
                       string1=string1,
                       string2=string2)

unique.areas <- as.integer(areas$comarea)
area.start.end.trips <- c()
area.start.end.downtown.trips <- c()
area.en.route.trips <- c()
area.en.route.downtown.trips <- c()
for(i in 1:nrow(areas)){
     this.area <- as.integer(areas$area_num_1[i])
     area.start.end.trips <- c(area.start.end.trips,
                               volumes[start.area==this.area | end.area==this.area,
                                sum(daily.trips,na.rm=TRUE)])
     area.start.end.downtown.trips <- c(area.start.end.downtown.trips,
                                        volumes[start.area==this.area | end.area==this.area,
                                         sum(downtown*daily.trips,na.rm=TRUE)])
     en.routes <- en.route[intersection==this.area]
     en.route.trips <- 0;
     en.route.downtown.trips <- 0;
     for(j in 1:nrow(en.routes)){
          this.en.route <- en.routes[j,]
          match <- unique.arcs[string.1==this.en.route$string1 & 
                                       string.1==this.en.route$string2]
          en.route.trips = en.route.trips +volumes[(start.area==match$area1 & end.area==match$area2)
                                                   | (start.area==match$area2 & end.area==match$area1),
                                     sum(daily.trips,na.rm=TRUE)]
          en.route.downtown.trips = en.route.downtown.trips +volumes[(start.area==match$area1 & end.area==match$area2)
                                                   | (start.area==match$area2 & end.area==match$area1),
                                                   sum(downtown*daily.trips,na.rm=TRUE)]
     }
     area.en.route.trips <- c(area.en.route.trips,en.route.trips)
     area.en.route.downtown.trips <- c(area.en.route.downtown.trips,en.route.downtown.trips)
}
total.downtown.trips <- volumes[,sum(downtown*daily.trips,na.rm=TRUE)]
total.trips <- volumes[,sum(downtown*daily.trips,na.rm=TRUE)]
area.summary <- data.table(area=unique.areas,
                           start.end.trips = area.start.end.trips,
                           start.end.downtown.trips = area.start.end.downtown.trips,
                           en.route.trips = area.en.route.trips,
                           en.route.downtown.trips = area.en.route.downtown.trips)
area.summary[,':='(trips=start.end.trips+en.route.trips,
                   downtown.trips=start.end.downtown.trips+en.route.downtown.trips)][
                        ,':='(pct.that.are.downtown=downtown.trips/trips,
                              pct.of.downtown=downtown.trips/total.downtown.trips,
                              pct.of.total=trips/total.trips)]

areas$pct.of.downtown <- area.summary$pct.of.downtown
ggplot(areas) +
     geom_sf(aes(fill = pct.of.downtown))

st_intersects(line_dat, poly_dat)
unique.links <- volumes
unique.links <- unique(rbindlist(list(
     volumes[.(start.area,end.area,start.tract,end.tract,start.lat,end.lat,start.long,end.long)]]