library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(imputeTS)
library(suncalc)

memory.size(max=200000)
Sys.setenv(TZ='UTC')
## load the sea data
sea <- read.csv("C:/Users/mbradar/Documents/Sea_waves/sea_2019-2020_L_2.csv",sep=";")
sea <- sea[,c(2,20:21,23)]
colnames(sea)[colnames(sea)==c("MEETPUNT_IDENTIFICATIE","WAARNEMINGDATUM","WAARNEMINGTIJD",'NUMERIEKEWAARDE')] <- c("station","date","time","height")
sea <- subset(sea,station=='IJgeul 1 boei')
sea$timestamp <- paste(sea$date, sea$time, sep= ' ')
sea$timestamp <- as.POSIXct(sea$timestamp,format="%d-%m-%Y %H:%M:%S")
sea$timestamph <- ymd_h(paste(date(sea$timestamp), hour(sea$timestamp),sep= ' '))
sea <- subset(sea,height<10000)

seaH <- sea %>%
  group_by(timestamph) %>%
  summarise(height=mean(height))

write.table(seaH,"C:/Users/mbradar/Documents/Sea_waves/sea_2019_2020_L_hourly.csv",sep=";")

## load the bird data
mix1 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2019_mixed&vert.csv",sep=";")
mix2 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn_2019mixedandvert.csv",sep=";")
mix2 <- mix2[,-(16)]
mix3 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2020_mixed&vert.csv",sep=";")
mix3 <- mix3[,-c(7,17)]
mix4 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/Autumn_2020_mixed&vert.csv",sep=";")
mix4 <- mix4[,-c(7,17)]

listm <- list(mix1,mix2,mix3,mix4)

mix <- do.call("rbind",listm)
rm(list=c("mix1","mix2","mix3","mix4","listm"))
trackest <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/trackest_2019-2020.csv",sep=";")
trackest <- subset(trackest,alt>=0)
trackestH <- trackest%>%
  group_by(track_id)%>%
  summarise(alt=mean(alt))



mix1 <- left_join(mix,trackestH,by="track_id")
mix1$new_alt <- ifelse(mix1$tracktype=='RaAzEl',mix1$alt,mix1$altitude)
mix1 <- subset(mix1,new_alt>=0)
mix1 <- subset(mix1,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
mix1$timestamp_start <- as.POSIXct(mix1$timestamp_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
mix1$timestamp <- as.POSIXct(ymd_hm(paste(date(mix1$timestamp_start), mix1$hourx,minute(mix1$timestamp_start), sep= ' ')))
mix1$timestamph<- as.character(ymd_h(paste(date(mix1$timestamp_start), hour(mix1$timestamp_start),sep= ' ')))
mix1$timestamph <- as.POSIXct(mix1$timestamph,format="%Y-%m-%d %H:%M:%S")

write.table(mix1,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert&mixed.csv",sep=";")

mix1H <- mix1 %>%
  group_by(timestamph) %>%
  summarise(count=length(track_id),nr_flocks=length(which(classification=='FLOCK')),
            small=length(which(classification=='SMALL_BIRD')),medium=length(which(classification=='MEDIUM_BIRD')),
            large=length(which(classification=='LARGE_BIRD')),alt=mean(new_alt))
write.table(mix1H,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_hourly_vert&mixed.csv",sep=";")

mix1 <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp_start <- as.POSIXct(vert$timestamp_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamph<- as.character(ymd_h(paste(date(vert$timestamp_start), hour(vert$timestamp_start),sep= ' ')))
vert$timestamph <- as.POSIXct(vert$timestamph,format="%Y-%m-%d %H:%M:%S")

write.table(vert,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert.csv",sep=";")



vertH <- vert %>%
  group_by(timestamph) %>%
  summarise(count=length(track_id),nr_flocks=length(which(classification=='FLOCK')),
            small=length(which(classification=='SMALL_BIRD')),medium=length(which(classification=='MEDIUM_BIRD')),
            large=length(which(classification=='LARGE_BIRD')))
vertl <- subset(vert,altitude_layer<100)  
vertHl <- vertl %>%
  group_by(timestamph) %>%
  summarise(count=length(track_id),nr_flocks=length(which(classification=='FLOCK')),
            small=length(which(classification=='SMALL_BIRD')),medium=length(which(classification=='MEDIUM_BIRD')),
            large=length(which(classification=='LARGE_BIRD')))

write.table(vertH,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_hourly_vert.csv",sep=";")
write.table(vertHl,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_hourly_vert_low.csv",sep=";")

clutter1 <- read.csv("C:/Users/mbradar/Documents/Robin/data/clutter_spring2019_L.csv",sep=";")
clutter2 <- read.csv("C:/Users/mbradar/Documents/Robin/data/clutter_autumn2019_L.csv",sep=";")
clutter3 <- read.csv("C:/Users/mbradar/Documents/Robin/data/clutter_spring2020_L.csv",sep=";")
clutter4 <- read.csv("C:/Users/mbradar/Documents/Robin/data/clutter_autumn2020_L.csv",sep=";")
listc <- list(clutter1,clutter2,clutter3,clutter4)
clutter <- do.call("rbind",listc)
rm(list = c("clutter1","clutter2","clutter3","clutter4","listc"))
colnames(clutter)[which(names(clutter) == "timestamp1")] <- "timestamp"
clutter$timestamp <- as.POSIXct(clutter$timestamp,tz="UTC")
clutter$timestamph <- as.POSIXct(ymd_h(paste(as.Date(clutter$timestamp), hour(clutter$timestamp),sep= ' ')))
clutter$date <- date(clutter$timestamph)
rm(list = c("clutter1","clutter2","clutter3","clutter4"))
sun<- getSunlightTimes(clutter$date,52.25,4,keep = c("sunrise","sunset"),tz="UTC")
sun <- dplyr::distinct(sun)
clutter <- left_join(clutter,sun,by=c('date'))
rm(list=c("sun"))
clutter$light <- with(clutter,ifelse(hour(clutter$timestamph)>=hour(sunset),"evening",
                                     ifelse(hour(clutter$timestamph)<=hour(sunrise),"night","day")))
clutter$night <- with(clutter,ifelse(clutter$light=="evening",clutter$date,
                                     ifelse(clutter$light=="night",clutter$date-lubridate::days(1),0)))
clutter <- subset(clutter,!night==0)
clutter$night <- zoo::as.Date(clutter$night,tz="UTC")

write.table(clutter,"C:/Users/mbradar/Documents/Robin/data/Clutter/Luchterduinen/clutter_2019-2020.csv",sep=";")
#depending on the year the horizontal radar_id can be 2,3,5,6,9 and 11
#first check the radar id in the data
unique(clutter$radar_id)
clutterH <- subset(clutter,radar_id==2 | radar_id==3 | radar_id==5 | radar_id==6 | radar_id==9 | radar_id==11)
clutterV <- subset(clutter,radar_id==1 | radar_id==4 | radar_id==7 | radar_id==8 | radar_id==10 | radar_id==12)

clutH <- clutterH %>%
  group_by(timestamph)%>%
  summarise(l_mask=mean(l_mask),v_mask=mean(v_mask),r_mask=mean(r_mask))

clutV <- clutterV %>%
  group_by(timestamph)%>%
  summarise(l_mask=mean(l_mask),v_mask=mean(v_mask),r_mask=mean(r_mask))

write.table(clutH,"C:/Users/mbradar/Documents/Robin/data/Clutter/Luchterduinen/clutter_2019-2020_horiz_hourly.csv",sep=";")
write.table(clutV,"C:/Users/mbradar/Documents/Robin/data/Clutter/Luchterduinen/clutter_2019-2020_vert_hourly.csv",sep=";")

rain1 <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_spring2019.csv", sep=";")
rain2<- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2019.csv", sep=";")
rain3 <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_spring2020.csv", sep=";")
rain4 <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2020.csv", sep=";")

listr <- list(rain1,rain2,rain3,rain4)
rain <- do.call("rbind",listr)

write.table(rain,"C:/Users/mbradar/Documents/Rain-Johannes/rain_2019-2020_L.csv", sep=";")

colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
rain$timestamph <-as.POSIXct(ymd_h(paste(as.Date(rain$timestamp), hour(rain$timestamp),sep= ' '))) 
rain <- subset(rain,mean<500)

rainH <- rain %>%
  group_by(timestamph) %>%
  summarise(mean=mean(mean))

write.table(rainH,"C:/Users/mbradar/Documents/Rain-Johannes/rain_2019-2020_L_hourly.csv", sep=";")


activity1 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_spring2019_L.csv",sep=";")
activity1 <- activity1[,1:2]
activity1$date <- date(activity1$timestamp1)
sun<- getSunlightTimes(unique(activity1$date),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
activity1 <- left_join(activity1,sun,by=c('date'))
rm(list=c("sun"))
activity1$light <- with(activity1,ifelse(hour(activity1$timestamp1)>=hour(sunset),"evening",
                                     ifelse(hour(activity1$timestamp1)<=hour(sunrise),"night","day")))
activity1$night <- with(activity1,ifelse(activity1$light=="evening",activity1$date,
                                     ifelse(activity1$light=="night",activity1$date-lubridate::days(1),0)))
activity1 <- subset(activity1,!night==0)
activity1$night <- zoo::as.Date(activity1$night,tz="UTC")


activity2 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_autumn2019_L.csv",sep=";")
activity2 <- activity2[,1:2]
activity2$date <- date(activity2$timestamp1)
sun<- getSunlightTimes(unique(activity2$date),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
activity2 <- left_join(activity2,sun,by=c('date'))
rm(list=c("sun"))
activity2$light <- with(activity2,ifelse(hour(activity2$timestamp1)>=hour(sunset),"evening",
                                         ifelse(hour(activity2$timestamp1)<=hour(sunrise),"night","day")))
activity2$night <- with(activity2,ifelse(activity2$light=="evening",activity2$date,
                                         ifelse(activity2$light=="night",activity2$date-lubridate::days(1),0)))
activity2 <- subset(activity2,!night==0)
activity2$night <- zoo::as.Date(activity2$night,tz="UTC")

activity3 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_spring2020_L.csv",sep=";")
activity3 <- activity3[,1:2]
activity3$date <- date(activity3$timestamp1)
sun<- getSunlightTimes(unique(activity3$date),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
activity3 <- left_join(activity3,sun,by=c('date'))
rm(list=c("sun"))
activity3$light <- with(activity3,ifelse(hour(activity3$timestamp1)>=hour(sunset),"evening",
                                         ifelse(hour(activity3$timestamp1)<=hour(sunrise),"night","day")))
activity3$night <- with(activity3,ifelse(activity3$light=="evening",activity3$date,
                                         ifelse(activity3$light=="night",activity3$date-lubridate::days(1),0)))
activity3 <- subset(activity3,!night==0)
activity3$night <- zoo::as.Date(activity3$night,tz="UTC")

activity4 <- read.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_autumn2020_L.csv",sep=";")
activity4 <- activity4[,1:2]
activity4$date <- date(activity4$timestamp1)
sun<- getSunlightTimes(unique(activity4$date),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
activity4 <- left_join(activity4,sun,by=c('date'))
rm(list=c("sun"))
activity4$light <- with(activity4,ifelse(hour(activity4$timestamp1)>=hour(sunset),"evening",
                                         ifelse(hour(activity4$timestamp1)<=hour(sunrise),"night","day")))
activity4$night <- with(activity4,ifelse(activity4$light=="evening",activity4$date,
                                         ifelse(activity4$light=="night",activity4$date-lubridate::days(1),0)))
activity4 <- subset(activity4,!night==0)
activity4$night <- zoo::as.Date(activity4$night,tz="UTC")



lista <- list(activity1,activity2,activity3,activity4)

activity <- do.call("rbind",lista)

write.table(activity,"C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_all2019-2020_L.csv", sep=";")
rm(list=c("activity1","activity2","activity3","activity4","lista"))
activity <- read.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_all2019-2020_L.csv", sep=";")
colnames(activity)[which(names(activity) == "timestamp1")] <- "timestamp"
activity$timestamp <- as.POSIXct(activity$timestamp, tz="UTC")

activity$timestamph <-as.character(ymd_h(paste(date(activity$timestamp), hour(activity$timestamp),sep= ' ')))
library(beepr)
beep()
activityH <- subset(activity,radar_id==2 | radar_id==3 | radar_id==5 | radar_id==6 | radar_id==9 | radar_id==11)
activityV <- subset(activity,radar_id==1 | radar_id==4 | radar_id==7 | radar_id==8 | radar_id==10 | radar_id==12)

write.table(activityH,"C:/Users/mbradar/Documents/Robin/data/Radar_activity/activity_all2019-2020_L_horiz.csv", sep=";")
write.table(activityV,"C:/Users/mbradar/Documents/Robin/data/Radar_activity/activity_all2019-2020_L_vert.csv", sep=";")

activityH <- read.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_all2019-2020_L_horiz.csv", sep=";")
activityV <- Vread.csv("C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_all2019-2020_L_vert.csv", sep=";")

actH <- activityH %>%
  group_by(timestamph)%>%
  summarise(count=length(radar_id))

actV <- activityV %>%
  group_by(timestamph) %>%
  summarise(count=length(radar_id))

write.table(actH,"C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_all2019-2020_L_horiz_hourly.csv", sep=";")
write.table(actV,"C:/Users/mbradar/Documents/Robin/data/Radar_activity/Luchterduinen/activity_all2019-2020_L_vert_hourly.csv", sep=";")


