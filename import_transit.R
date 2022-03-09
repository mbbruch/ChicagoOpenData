library(data.table)
library(stringr)
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)
library(dtw)

folder <- "C:\\Users\\Matthew\\Desktop\\Data Analysis\\Shapefiles\\"

census_tracts <- st_read(paste0(folder,"Boundaries - Census Tracts - 2010\\geo_export_93c13ad4-b944-43aa-8964-6ccf5949da3f.shp"))
census_blocks <- st_read(paste0(folder,"Boundaries - Census Blocks - 2010\\geo_export_e843e29b-7177-426b-8734-92621b9ae333.shp"))
downtown.tracts <- fread("C:\\Users\\Matthew\\Desktop\\Chicago\\downtown_tracts.csv")
downtown.tracts[,tract:=as.character(tract)]

folder <- "C:\\Users\\Matthew\\Desktop\\Data Analysis\\Ridership\\"

setwd(folder)
swr = function(string, nwrap=10) {
        paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)
files = list.files(path=getwd(), pattern="Divvy*",full.names=TRUE)
files=lapply(files, read.csv)
divvy <- rbindlist(files,fill=TRUE)
divvy[is.na(start_time),start_time:=X01...Rental.Details.Local.Start.Time]
divvy[is.na(start_time),start_time:=started_at]
divvy[,date:=as.POSIXct(substr(start_time,1,10),format="%Y-%m-%d")]
divvy <- divvy[,.(divvy.rides=.N),by=date]

transit.ridership <- fread(paste0(folder,"CTA_-_Ridership_-_Daily_Boarding_Totals.csv"))
transit.ridership[,date:=as.POSIXct(service_date,format="%m/%d/%Y")]
transit.ridership <- transit.ridership[year(date)==2019 | (year(date)==2020 & month(date) <= 3)]
transit.ridership[divvy,bikeshare:=divvy.rides,on=.(date=date)]

ridership <- rbindlist(list(
        transit.ridership[,.(date,volume=bus,mode="Bus")],
        transit.ridership[,.(date,volume=rail_boardings,mode="Rail")],
        transit.ridership[,.(date,volume=bikeshare,mode="Bikeshare")]
))

ggplot(ridership, 
       aes(x = date, y = volume)) + 
        geom_line() + theme_light() +
        ggtitle("Daily ridership for bikeshare, bus, and rail") + facet_grid(mode ~ ., scales="free")



bus.ridership <- fread(paste0(folder,"CTA_-_Ridership_-_Bus_Routes_-_Daily_Totals_by_Route.csv"))
bus.ridership <- bus.ridership[substr(date,7,11)=='2019']
bus.lines <- bus.ridership[,.(route=unique(route))]
bus.stops <- fread(paste0(folder,"CTA_BusStops.csv"))
bus.stops.sf <- bus.stops %>%
     filter(!is.na(POINT_X), !is.na(POINT_Y)) %>%
     st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = st_crs(census_tracts))
intersected <- st_intersects(bus.stops.sf, census_tracts)
bus.stops <- bus.stops.sf %>%
     mutate(intersection = as.integer(intersected),
            tract = if_else(is.na(intersection), "",
                           census_tracts$geoid10[intersection]))
temp <- matrix(unlist(bus.stops$geometry),ncol=2,byrow=TRUE)
bus.stops$lat <- temp[,2]
bus.stops$long <- temp[,1]
rm(bus.stops.sf)
bus.stops$geometry <- NULL; bus.stops <- as.data.table(bus.stops)
bus.stops[downtown.tracts,downtown:=downtown.tract,on=.(tract)]

bus.lines[,':='(stops=0,stops.downtown.min=0,stops.downtown.max=0,
                stops.night=0,stops.night.downtown.min=0,stops.night.downtown.max=0)]

bus.lines[,':='(downtown=stops.downtown.max>0 | stops.night.downtown.max>0)]


list1 <- str_split(bus.stops[,ROUTESSTPG], ",")
list1 = gsub(" ", "", stops, fixed = TRUE)
stop.of.lines <- c()
for(i in 1:length(list1)){
        for(j in 1:length(list1[[i]])){
                stop.of.lines <- c(stop.of.lines,bus.stops[i,SYSTEMSTOP])
        }
}

stops = unlist(list1)
stop.line.combos <- data.table(SYSTEMSTOP=stop.of.lines,line=stops)
stop.line.combos[bus.stops,':='(lat=lat,long=long),on=.(SYSTEMSTOP=SYSTEMSTOP)]
stops = gsub(" ", "", stops, fixed = TRUE)
stops.downtown.min = unlist(str_split(bus.stops[downtown==1,ROUTESSTPG], ","))
stops.downtown.min = gsub(" ", "", stops.downtown.min, fixed = TRUE)
stops.downtown.max = unlist(str_split(bus.stops[!is.na(downtown),ROUTESSTPG], ","))
stops.downtown.max = gsub(" ", "", stops.downtown.max, fixed = TRUE)

stops.night = unlist(str_split(bus.stops[,OWLROUTES], ","))
stops.night = gsub(" ", "", stops.night, fixed = TRUE)
stops.night.downtown.min = unlist(str_split(bus.stops[downtown==1,OWLROUTES], ","))
stops.night.downtown.min = gsub(" ", "", stops.night.downtown.min, fixed = TRUE)
stops.night.downtown.max = unlist(str_split(bus.stops[!is.na(downtown),OWLROUTES], ","))
stops.night.downtown.max = gsub(" ", "", stops.night.downtown.max, fixed = TRUE)

for(i in 1:nrow(bus.lines)){
     bus.lines[i,]$stops = sum(stops==bus.lines[i,]$route)
     bus.lines[i,]$stops.downtown.min = sum(stops.downtown.min==bus.lines[i,]$route)
     bus.lines[i,]$stops.downtown.max = sum(stops.downtown.max==bus.lines[i,]$route)
     bus.lines[i,]$stops.night = sum(stops.night==bus.lines[i,]$route | stops.night==paste0("N",bus.lines[i,]$route))
     bus.lines[i,]$stops.night.downtown.min = sum(stops.night.downtown.min==bus.lines[i,]$route | stops.night.downtown.min==paste0("N",bus.lines[i,]$route))
     bus.lines[i,]$stops.night.downtown.max = sum(stops.night.downtown.max==bus.lines[i,]$route | stops.night.downtown.max==paste0("N",bus.lines[i,]$route))
}

bus.lines[,downtown:=max(ifelse(is.na(stops.downtown.max) | stops.downtown.max==0,0,1)),by=route]

bus.ridership[bus.lines,downtown:=downtown,on=.(route)]
bus.ridership[,date:=as.POSIXct(date,format="%m/%d/%Y")]
bus.ridership.avg <- bus.ridership[date<"2020-01-06",
                                   .(daily.rides=sum(rides,na.rm=TRUE)/uniqueN(date,na.rm=TRUE)),
                                   by=.(route,downtown)]
ggplot(bus.ridership.avg, aes(x = daily.rides)) + geom_histogram() + facet_grid(downtown ~ .)
ggplot(stop.line.combos[line=="169" | line=="201"],aes(x=long,y=lat,color=line)) + geom_point()

bus.routes <- read_sf(paste0(folder,"doc.kml"))
to.use <- bus.lines[route!="1001" & stops>0, which = TRUE]
distances <- matrix(ncol=length(to.use),nrow=length(to.use))
library(geometry)
library(GeoRange)
for(i in 1:(length(to.use)-1)){
        for(j in (i+1):length(to.use)){
                stops1 = stop.line.combos[line==bus.lines[to.use[i],route],.(long,lat)]
                stops2 = stop.line.combos[line==bus.lines[to.use[j],route],.(long,lat)]
                stops3 = unique(rbindlist(list(stops1,stops2)))
                area1 = convhulln(stops1,output.options="FA")$area
                area2 = convhulln(stops2,output.options="FA")$area
                area3 = convhulln(stops3,output.options="FA")$area
                distances[i,j] <- area3 - min(area1,area2)
}}
line1 = c()
line2 = c()
distance = c()
for(i in 1:(length(bus.routes$geometry)-1)){
        for(j in (i+1):length(bus.routes$geometry)){
                stops1 = do.call(rbind,bus.routes$geometry[[i]])[,1:2]
                stops2 = do.call(rbind,bus.routes$geometry[[j]])[,1:2]
                stops3 = unique(rbind(stops1,stops2))
                area1 = convhulln(stops1,output.options="FA")$vol
                area2 = convhulln(stops2,output.options="FA")$vol
                area3 = convhulln(stops3,output.options="FA")$vol
                line1 = c(line1,bus.routes$Name[[i]])
                line2 = c(line2,bus.routes$Name[[j]])
                distance = c(distance,area3 - min(area1,area2))
        }}
bus.line.distances <- data.table(line1=line1,line2=line2,distance=distance)
bus.line.distances[,distance:=distance/max(distance,na.rm=TRUE)]
        
ggplot(stop.line.combos[line=="44" | line=="3" | line=="67" | line=="31" | line=="70" | line=="51"],aes(x=long,y=lat,color=ifelse(line=="44" | line=="3","a. more similar",ifelse(line=="67" | line=="31","b. in between","c. more different")))) + geom_point()
ggplot(stop.line.combos[line=="169" | line=="201" | line=="66" | line=="37" | line=="171" | line=="172"],aes(x=long,y=lat,color=ifelse(line=="169" | line=="201","c. most different",ifelse(line=="66" | line=="37","b. in between","a. most similar")))) + geom_point()

rail.ridership <- fread(paste0(folder,"CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv"))
rail.stops <- fread(paste0(folder,"CTA_-_System_Information_-_List_of__L__Stops.csv"))
rail.stops.sf <- rail.stops %>%
     filter(!is.na(Long), !is.na(Lat)) %>%
     st_as_sf(coords = c("Long", "Lat"), crs = st_crs(census_tracts))
intersected <- st_intersects(rail.stops.sf, census_tracts)
rail.stops <- rail.stops.sf %>%
     mutate(intersection = as.integer(intersected),
            tract = if_else(is.na(intersection), "",
                            census_tracts$geoid10[intersection]))
rm(rail.stops.sf)
rail.stops$geometry <- NULL; rail.stops <- as.data.table(rail.stops)
rail.stops[downtown.tracts,downtown:=downtown.tract,on=.(tract)]
rail.stops[,downtown.final:=max(downtown,na.rm=TRUE),by=STATION_NAME]
rail.stops <- unique(rail.stops[,.(MAP_ID,downtown.final)])
rail.ridership[rail.stops,downtown:=downtown.final,on=.(station_id=MAP_ID)]
rail.ridership[,downtown:=(!is.na(downtown) & downtown>=-1)]
#rail.ridership <- rail.ridership[substr(date,7,11)=='2019']
rail.rollup <- rail.ridership[,.(daily.riders=mean(rides,na.rm=TRUE)),by=.(date,downtown)]
bus.rollup <- bus.ridership[,.(daily.riders=mean(rides,na.rm=TRUE)),by=.(date,downtown)]
rail.rollup[,datefixed:=as.POSIXct(date,format="%m/%d/%Y")]
rail.rollup[,weekday:=wday(datefixed)]
bus.rollup[,datefixed:=as.POSIXct(date,format="%m/%d/%Y")]
bus.rollup[,wday:=wday(datefixed)]

rail.ridership.avg <- rail.ridership[date<"2020-01-06",
                                   .(daily.rides=sum(rides,na.rm=TRUE)/uniqueN(date,na.rm=TRUE)),
                                   by=.(station_id,downtown)]
ggplot(rail.ridership.avg, aes(x = daily.rides)) + geom_histogram() + facet_grid(downtown ~ .)

ggplot(rail.rollup[year(datefixed)==2019 & month(datefixed) > 6 & wday>1 & wday < 7], 
       aes(x = datefixed, y = log(daily.riders), group=downtown, color=downtown)) + 
        geom_line() + theme_light() +
        ggtitle("Pre-treatment trends for rail ridership") + facet_grid(downtown ~ ., scales="free")

ggplot(bus.rollup[year(datefixed)==2019 & month(datefixed) > 6 & wday>1 & wday < 7], 
        aes(x = datefixed, y = log(daily.riders), group=downtown, color=downtown)) + 
        geom_line() + theme_light() +
        ggtitle("Pre-treatment trends for bus ridership") + facet_grid(downtown ~ ., scales="free")
