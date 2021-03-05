library(dplyr)
library(lubridate)
library(imputeTS)
library(suncalc)

Sys.setenv(TZ='UTC')

# load bird data
vert <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert&mixed.csv",sep=";")
vert$timestamp <- as.POSIXct(vert$timestamp,tz='UTC')
vert$timestamph <- as.POSIXct(vert$timestamph,tz='UTC')
#vert <- vert %>%
#  group_by(timestamp)%>%
#  summarise(count=length(track_id),altitude=mean(altitude))
# load and prepare KNMI rain data
rain <- read.csv("C:/Users/mbradar/Documents/Rain-Johannes/rain_2019-2020_L.csv",sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
rain$timestamph <-as.POSIXct(ymd_h(paste(as.Date(rain$timestamp), hour(rain$timestamp),sep= ' '))) 
rain <- subset(rain,mean<500)
# join bird and rain data and filter out rain minutes
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)
# load and prepare clutter data
clutterV <- read.csv("C:/Users/mbradar/Documents/Robin/data/Clutter/Luchterduinen/clutter_2019-2020_vert_hourly.csv",sep=";")
clutterV <- subset(clutter,radar_id==1 | radar_id==4 | radar_id==7 | radar_id==8 | radar_id==10 | radar_id==12)
colnames(clutterV)[colnames(clutterV)==c("timestamp1")] <- c("timestamp")
clutterV$timestamph <- as.POSIXct(clutterV$timestamph,tz='UTC')
# remove what we don't need
rm(list = c("rain","clutter"))
# initiate a vector with all possible minutes
hoursS19 <- seq(ymd_hms('2019-02-15 00:00:00'),ymd_hms('2019-06-01 07:00:00'), by="hours")
hoursA19 <- seq(ymd_hms('2019-08-15 00:00:00'),ymd_hms('2019-12-01 07:00:00'), by="hours")
hoursS20 <- seq(ymd_hms('2020-02-15 00:00:00'),ymd_hms('2020-06-01 07:00:00'), by="hours")
hoursA20 <- seq(ymd_hms('2020-08-15 00:00:00'),ymd_hms('2020-12-01 07:00:00'), by="hours")
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
# merge the files
final <- left_join(allHours,clutterV,by='timestamph')
# remove what is not needed
rm(list=c("allHours","clutterV","hoursA19","hoursA20","hoursS19","hoursS20"))
#make a distinction between hours with no birds and radar not active
final$activity <- ifelse(is.na(final$l_mask),"OFF","ON")
colnames(vert)[colnames(vert)==c("timestamph.x")] <- c("timestamph")
data <- left_join(vert,final,by="timestamph")
data1 <- subset(data,r_mask==0)
write.table(data1,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert&mixed_filtered.csv",sep=";")
