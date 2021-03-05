library(lubridate)
library(rWind)
library(dplyr)
library(RNCEP)
library(circular)
library(suncalc)
library(imputeTS)

Sys.setenv(tz="UTC")

birds <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert&mixed_filtered.csv",sep=";")
birds$altitude_layer <- ceiling((birds$new_alt)/100)*100
birds$timestamph <- as.POSIXct(birds$timestamph,tz="UTC")

weatherS19 <-  read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/spring_2019.csv",sep=";")
weatherA19 <- read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/autumn_2019.csv",sep=";")
weatherS20 <-  read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/spring_2020.csv",sep=";")
weatherA20 <- read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/autumn_2020.csv",sep=";")

weather <- list(weatherS19,weatherA19,weatherS20,weatherA20)
weather <- do.call("rbind",weather)
weather$altitude_layer <- ceiling((weather$height)/100)*100
weather$wind_sp <- uv2ds(weather$u,weather$v)[,2]
weather$temp <- weather$t - 273.15 #to get temperature in C from K
weather$time <- as.POSIXct(weather$time,tz="UTC")
listp <- split(weather,weather$night)

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

final <- vector("list", 15)
c <- seq(100,1500,100)


for(k in 1:length(final)){
  final[[k]] <- allHours
}

for(k in 1:length(final)){
  final[[k]]$altitude_layer <- rep(c[k],nrow(final[[k]]))
}

st <- do.call("rbind",final)
check <- left_join(st,weather,by=c('altitude_layer','timestamph'))
listp <- split(check,check$night)

for(k in 1:length(listp)){
  listp[[k]]$cc<- na_interpolation(listp[[k]]$cc, option ="linear")
  listp[[k]]$r<- na_interpolation(listp[[k]]$r, option ="linear")
  listp[[k]]$crwc<- na_interpolation(listp[[k]]$crwc, option ="linear")
  listp[[k]]$t<- na_interpolation(listp[[k]]$t, option ="linear")
  listp[[k]]$u<- na_interpolation(listp[[k]]$u, option ="linear")
  listp[[k]]$v<- na_interpolation(listp[[k]]$v, option ="linear")
  listp[[k]]$wind_sp<- na_interpolation(listp[[k]]$wind_sp, option ="linear")
  listp[[k]]$temp<- na_interpolation(listp[[k]]$temp, option ="linear")
  
}

weather <- do.call("rbind",listp)
weather$wind_dir <- uv2ds(weather$u,weather$v)[,1]
weather$wind_dir <- as.circular(weather$wind_dir,units = "degrees",modulo ="2pi",zero=pi/2,rotation="clock", template="geographic")
weather$wind_ass <- ifelse(month(weather$time)>=2 & month(weather$time)<=6, NCEP.Tailwind(weather$u,weather$v,90,15)[,1],NCEP.Tailwind(weather$u,weather$v,220,15)[,1])
colnames(weather)[colnames(weather)==c("time")] <- c("timestamph")
birds_f <- left_join(birds,weather,by=c("timestamph","altitude_layer"))
birds_f$height_lev <- ifelse(birds_f$altitude<=300,"low","high")
birds_f$height_lev_n <- ifelse(birds_f$height_lev=="low",0,1)
final <- birds_f[,c(1,5,6,7,9,14,21,22,23,36,40,45,46,48:53)]
write.table(final,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_an.csv",sep = ";" )

birdsS19 <- subset(final,datex>="2019-02-15" & datex<='2019-06-01') 
write.table(birdsS19,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2019_an.csv",sep = ";" )
birdsA19 <- subset(final,datex>="2019-08-15" & datex<='2019-12-01') 
write.table(birdsA19,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn_2019_an.csv",sep = ";" )
birdsS20 <- subset(final,datex>="2020-02-15" & datex<='2020-06-01') 
write.table(birdsS20,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2020_an.csv",sep = ";" )
birdsA20 <- subset(final,datex>="2020-08-15" & datex<='2020-12-01') 
write.table(birdsS20,"C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/spring_2020_an.csv",sep = ";" )


