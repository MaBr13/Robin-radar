library(lubridate)
library(DBI)
library(sf)
library(RPostgreSQL)
library(suncalc)
library(dplyr)

Sys.setenv(TZ='UTC')

hoursS19 <- seq(from=as.POSIXct('2019-02-15 00:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"),to=as.POSIXct('2019-06-01 07:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"), by="hour")
hoursA19 <- seq(from=as.POSIXct('2019-08-15 00:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"),to=as.POSIXct('2019-12-01 07:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"), by="hour")
hoursS20 <- seq(from=as.POSIXct('2020-02-15 00:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"),to=as.POSIXct('2020-06-01 07:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"), by="hour")
hoursA20 <- seq(from=as.POSIXct('2020-08-15 00:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"),to=as.POSIXct('2020-12-01 07:00:00',tz="UTC",format = "%Y-%m-%d %H:%M:%S"), by="hour")
allHours <- c(hoursS19,hoursA19,hoursS20,hoursA20)
allHours <- as.data.frame(allHours)
colnames(allHours)[colnames(allHours)==c("allHours")] <- c("timestamph")
allHours$date <- date(allHours$timestamp)
sun<- getSunlightTimes(unique(allHours$date),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
allHours <- left_join(allHours,sun,by=c('date'))
rm(list=c("sun"))
allHours$light <- with(allHours,ifelse(hour(allHours$timestamp)>=hour(sunset),"evening",
                                       ifelse(hour(allHours$timestamp)<=hour(sunrise),"night","day")))
allHours$night <- with(allHours,ifelse(allHours$light=="evening",allHours$date,
                                       ifelse(allHours$light=="night",allHours$date-lubridate::days(1),0)))
allHours <- subset(allHours,!night==0)
allHours$night <- zoo::as.Date(allHours$night,tz="UTC")
allHours <- allHours[,c(1:2,8)]


        
new_alts <- list()
database <- "rws01"
con <- dbConnect("PostgreSQL", 
                 dbname=database, # database name  
                 host='robin1.e-ecology.nl', 
                 user= "maja_bradaric",# username
                 password="55nebitno992@") # password

for(k in 1:length(allHours$timestamph)){
  
  new_alts[[k]] <- st_read(con,query = paste0("select track.id as track_id,track.tracktype, st_z(trackestimate.position) as alt
                         from public.track
                         INNER JOIN public.trackestimate 
                         ON track.id=trackestimate.track_id
                         where track.timestamp_start BETWEEN '", as.character(allHours$timestamph[k]) , "' AND '", as.character(allHours$timestamph[k]+hours(1)) ,"' 
                         AND track.tracktype in ('RaAzEl')
                         AND trackestimate.radar_id in (1,4,7,8,10,12)"))
}
 library(beepr)
beep()
trackestimate <- do.call("rbind",new_alts)
write.table(trackestimate,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/trackest_2019-2020.csv",sep=";")
