library(ggplot2)
library(rWind)
library(dplyr)
library(lubridate)
library(circular)
library(imputeTS)
library(ggquiver)
library(suncalc)
library(RNCEP)

#SPRING
#load radar data
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring2019mixed&vert.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamo_start <- as.POSIXct(vert$timestamp_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamp_start <- as.POSIXct(ymd_h(paste(date(vert$timestamp_start), vert$hourx,sep= ' ')))
#load rain data and filter rain minutes
rain <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2019.csv", sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)
#prepare the bird radar data for weather data
vert$time <- as.POSIXct(vert$timestamp_start,tz="UTC")
vert <- vert %>%
  group_by(time,altitude_layer)%>%
  summarise(count=length(track_id))
#load weather data
weather <- read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/Autumn_2019.csv", sep=';')
weather$altitude_layer <- floor((weather$height)/100)*100
#weather$wind_dir <- uv2ds(weather$u,weather$v)[,1]
#weather$wind_dir <- as.circular(weather$wind_dir,units = "degrees",modulo ="2pi",zero=pi/2,rotation="clock", template="geographic")
weather$wind_sp <- uv2ds(weather$u,weather$v)[,2]
weather$temp <- weather$t - 273.15 #to get temperature in C from K
weather$time <- as.POSIXct(weather$time,tz="UTC")
#make a list of all days and alt layers in spring
final <- vector("list", 16)
c <- seq(0,1500,100)
time <- seq(as.POSIXct("2019-08-15 00:00:00",tz="UTC"), as.POSIXct("2019-12-01 00:00:00",tz="UTC"), by="hour")
time <- as.data.frame(time)

for(k in 1:length(final)){
  final[[k]] <- time
}

for(k in 1:length(final)){
  final[[k]]$altitude_layer <- rep(c[k],nrow(final[[k]]))
}

st <- do.call("rbind",final)

#prepare bird data
check2 <- left_join(st,vert,by=c('altitude_layer','time'))
check2 <- subset(check2,altitude_layer<1500)
check2 <- check2 %>% arrange(time)
check2$date <- date(check2$time)
#prepare weather data
check <- left_join(st,weather,by=c('altitude_layer','time'))
rm(list = c("st","weather"))
check <- subset(check,altitude_layer<1500)
check <- check %>% arrange(time)
check$date <- date(check$time)
#calculate sunrise and sunset and assign migration night to bird data
sun<- getSunlightTimes(check2$date,52.25,4,keep = c("sunrise","sunset"),tz="UTC")
check2 <- left_join(check2,sun,by=c('date'))
check2 <- dplyr::distinct(check2)
check2$light <- with(check2,ifelse(hour(check2$time)>=hour(sunset),"evening",
                                 ifelse(hour(check2$time)<=hour(sunrise),"night","day")))
check2$night <- with(check2,ifelse(check2$light=="evening",check2$date,
                                        ifelse(check2$light=="night",check2$date-lubridate::days(1),0)))
check2 <- subset(check2,!night==0)
check2$night <- zoo::as.Date(check2$night,tz="UTC")
#calculate sunrise and sunset and assign migration night to weather data
sun1 <- getSunlightTimes(date(check$time),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
check <- left_join(check,sun1,by=c('date'))
check <- dplyr::distinct(check)
check$light <- with(check,ifelse(hour(check$time)>=hour(sunset),"evening",
                                     ifelse(hour(check$time)<=hour(sunrise),"night","day")))
check$night <- with(check,ifelse(check$light=="evening",check$date,
                                     ifelse(check$light=="night",check$date-lubridate::days(1),0)))
check <- subset(check,!night==0)
check$night <- ymd(check$night)

rm(list = c("sun","sun1","mix","final","time","c"))

#leave only migration nights we are interested in
check2 <- subset(check2,night>="2019-08-15" & night<="2019-11-31")
check <- subset(check,night>="2019-08-15" & night<="2019-11-31")
#interpolate missing values in weather data
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
check3 <-do.call("rbind",listp)
rm(list = c("check"))
check3$wind_dir <- uv2ds(check3$u,check3$v)[,1]
check3$wind_dir <- as.circular(check3$wind_dir,units = "degrees",modulo ="2pi",zero=pi/2,rotation="clock", template="geographic")
check3$wind_ass <- NCEP.Tailwind(check3$u,check3$v,90,18)[,1]

#rasterize bird data (night,altitude layer)
pts1 <- check2[,c('night','altitude_layer','count')]
pts1$IDgrid<-with(pts1,interaction(night,altitude_layer))
pts1$IDNgrid<-factor(pts1$IDgrid)
levels(pts1$IDNgrid)<-seq_along(levels(pts1$IDNgrid))
tab <- aggregate(pts1$count,pts1[,c('night','altitude_layer','IDNgrid')],FUN=sum,na.rm=T)
tab$x <- ifelse(tab$x==0,NA,tab$x)
#rasterize weather data
pts <- check3[,c('night','altitude_layer','wind_dir','wind_sp','wind_ass','temp','cc','r','t','crwc')]
pts$IDgrid<-with(pts,interaction(night,altitude_layer))
pts$IDNgrid<-factor(pts$IDgrid)
levels(pts$IDNgrid)<-seq_along(levels(pts$IDNgrid))
wsp <- aggregate(pts$wind_sp, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
wdir <- aggregate(pts$wind_dir,pts[,c('night','altitude_layer','IDNgrid')], 
                  function(x) mean.circular(x,units = "degrees",modulo ="2pi", 
                                            template="geographic",na.rm=T))
wdir$u <- ds2uv(wdir$x,wsp$x)[,1]
wdir$v <- ds2uv(wdir$x,wsp$x)[,2]
ass <- aggregate(pts$wind_ass, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
temp <- aggregate(pts$temp, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
cc <- aggregate(pts$cc, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
r <- aggregate(pts$r, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
crwc <- aggregate(pts$crwc, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)





#Bird counts
windows(30,15)
count1 <- ggplot(data = tab,
                 aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    legend.title = element_blank(),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    #legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn( "Nr.",limits=c(1, 6000),
                       breaks = c(1,2,3,5,10,20,50,100,200,400,1000,2000,4000),  
                       colours = c("yellow","red","black"),trans="log", na.value = "white")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/countS1.png"),count1,dpi=500)

#wind speed and direction
windows(30,15)
wind1 <- ggplot(data = wsp,
                aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  geom_quiver(data=wdir,aes(x = night, y = altitude_layer,u=u,v=v),col="black",vecsize=1.2,size = 0.5,center = TRUE)+
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  coord_cartesian(xlim=c(as.Date("2020-02-15"), as.Date("2020-05-31")))+
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('wsp(m/s)', limits=c(0, 40),
                       breaks = seq(0,40,5), 
                       colours = c("white","darkblue"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2020-02-15"), as.Date("2020-05-31")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/windS.png"),wind1,dpi=500)
dev.off()
#wind assistance
windows(30,15)
ass1 <- ggplot(data = ass,
                aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    legend.title = element_blank(),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    #legend.title = element_text(color = "black", size = 10)
  ) +
  coord_cartesian(xlim=c(as.Date("2020-02-15"), as.Date("2020-05-31")))+
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('was', limits=c(-30,30),
                       breaks = seq(-30,30,5), 
                       colours = c("cyan","white","orange"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2020-02-15"), as.Date("2020-05-31")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/wind_assS1.png"),ass1,dpi=500)
dev.off()
#temperature
windows(30,15)
temp1 <- ggplot(data = temp,
                aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    legend.title = element_blank(),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    #legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('t',limits=c(-25,25),
                       breaks = seq(-25,25,5), 
                       colours = c("blue","white","red"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2020-02-15"), as.Date("2020-05-31")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/tempS1.png"),temp1,dpi=500)
dev.off()
#Fraction of cloud cover
windows(30,15)
cc1 <- ggplot(data = cc,
              aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('c cov', limits=c(0,1),
                       breaks = seq(0,1,0.1), 
                       colours = c("gray95","gray33"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2020-02-15"), as.Date("2020-05-31")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/ccS.png"),cc1,dpi=500)
dev.off()
#Relative humidity
windows(30,15)
r1 <- ggplot(data = r,
             aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('rh(%) ', limits=c(0,120),
                       breaks = seq(0,120,20), 
                       colours = c("white","darkgreen"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2020-02-15"), as.Date("2020-05-31")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/rS.png"),r1,dpi=500)
dev.off()
#Specific rain water content
windows(30,15)
crwc1 <- ggplot(data = r,
                aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('crwc(kg/kg-1) ', limits=c(0,150),
                       breaks = seq(0,150,20), 
                       colours = c("lightskyblue1","lightskyblue4"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2020-02-15"), as.Date("2020-05-31")),expand = c(0,0))

ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/crwcS.png"),crwc1,dpi=500)
dev.off()


rm(list = c())

#AUTUMN
#load bird data
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/autumn_2019_Gem_vert2.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamp_start <- as.POSIXct(ymd_h(paste(date(vert$timestamp_start), vert$hourx,sep= ' ')))
#load rain data and filter rain minutes
rain <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2019.csv", sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)
#prepare the bird radar data for weather data
vert$time <- as.POSIXct(vert$timestamp_start,tz="UTC")
vert <- vert %>%
  group_by(time,altitude_layer)%>%
  summarise(count=length(track_id))
#load weather data
weather <- read.csv("C:/Users/mbradar/Documents/ERA5/Gemini/Autumn_2019.csv", sep=';')
weather$altitude_layer <- floor((weather$height)/100)*100
weather$wind_sp <- uv2ds(weather$u,weather$v)[,2]
weather$temp <- weather$t - 273.15 #to get temperature in C from K
weather$time <- as.POSIXct(weather$time,tz="UTC")


#make a list of all days and alt layers in spring
final <- vector("list", 15)
c <- seq(100,1500,100)
time <- seq(as.POSIXct("2019-08-15 00:00:00"), as.POSIXct("2019-12-01 00:00:00"), by="hour")
time <- as.data.frame(time)

for(k in 1:length(final)){
  final[[k]] <- time
}

for(k in 1:length(final)){
  final[[k]]$altitude_layer <- rep(c[k],nrow(final[[k]]))
}

st <- do.call("rbind",final)

#prepare bird data
check2 <- left_join(st,vert,by=c('altitude_layer','time'))
check2 <- subset(check2,altitude_layer<1500)
check2 <- check2 %>% arrange(time)
check2$date <- date(check2$time)
#prepare weather data
check <- left_join(st,weather,by=c('altitude_layer','time'))
rm(list = c("st","weather"))
check <- subset(check,altitude_layer<1500)
check <- check %>% arrange(time)
check$date <- date(check$time)
#calculate sunrise and sunset and assign migration night to bird data
sun<- getSunlightTimes(check2$date,52.25,4,keep = c("sunrise","sunset"),tz="UTC")
check2 <- left_join(check2,sun,by=c('date'))
check2 <- dplyr::distinct(check2)
check2$light <- with(check2,ifelse(hour(check2$time)>=hour(sunset),"evening",
                                   ifelse(hour(check2$time)<=hour(sunrise),"night","day")))
check2$night <- with(check2,ifelse(check2$light=="evening",check2$date,
                                   ifelse(check2$light=="night",check2$date-lubridate::days(1),0)))
check2 <- subset(check2,!night==0)

check2$night <- zoo::as.Date(check2$night)
#calculate sunrise and sunset and assign migration night to weather data
sun1 <- getSunlightTimes(date(check$time),52.25,4,keep = c("sunrise","sunset"),tz="UTC")

check <- left_join(check,sun1,by=c('date'))
check <- dplyr::distinct(check)
check$light <- with(check,ifelse(hour(check$time)>=hour(sunset),"evening",
                                 ifelse(hour(check$time)<=hour(sunrise),"night","day")))
check$night <- with(check,ifelse(check$light=="evening",check$date,
                                 ifelse(check$light=="night",check$date-lubridate::days(1),0)))
check <- subset(check,!night==0)
check$night <- zoo::as.Date(check$night)

rm(list = c("sun","sun1","mix","final","time","c"))

#leave only migration nights we are interested in
check2 <- subset(check2,night>="2019-08-15" & night<="2019-11-30")
check <- subset(check,night>="2019-08-15" & night<="2019-11-30")
#interpolate missing values in weather data
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
check3 <-do.call("rbind",listp)
rm(list = c("check"))
check3$wind_dir <- uv2ds(check3$u,check3$v)[,1]
check3$wind_dir <- as.circular(check3$wind_dir,units = "degrees",modulo ="2pi",zero=pi/2,rotation="clock", template="geographic")
check3$wind_ass <- NCEP.Tailwind(check3$u,check3$v,220,18)[,1]
#rasterize bird data (night,altitude layer)
pts1 <- check2[,c('night','altitude_layer','count')]
pts1$IDgrid<-with(pts1,interaction(night,altitude_layer))
pts1$IDNgrid<-factor(pts1$IDgrid)
levels(pts1$IDNgrid)<-seq_along(levels(pts1$IDNgrid))
tab <- aggregate(pts1$count,pts1[,c('night','altitude_layer','IDNgrid')],FUN=sum,na.rm=T)
tab$x <- ifelse(tab$x==0,NA,tab$x)
#rasterize weather data
pts <- check3[,c('night','altitude_layer','wind_dir','wind_sp','wind_ass','temp','cc','r','t','crwc')]
pts$IDgrid<-with(pts,interaction(night,altitude_layer))
pts$IDNgrid<-factor(pts$IDgrid)
levels(pts$IDNgrid)<-seq_along(levels(pts$IDNgrid))
wsp <- aggregate(pts$wind_sp, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
wdir <- aggregate(pts$wind_dir,pts[,c('night','altitude_layer','IDNgrid')], 
                  function(x) mean.circular(x,units = "degrees",modulo ="2pi", 
                                            template="geographic",na.rm=T))
wdir$u <- ds2uv(wdir$x,wsp$x)[,1]
wdir$v <- ds2uv(wdir$x,wsp$x)[,2]
ass <- aggregate(pts$wind_ass, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
temp <- aggregate(pts$temp, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
cc <- aggregate(pts$cc, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
r <- aggregate(pts$r, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)
crwc <- aggregate(pts$crwc, pts[,c('night','altitude_layer','IDNgrid')], FUN=mean)

#Bird counts
windows(30,15)
count2 <- ggplot(data = tab,
       aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    legend.title = element_blank(),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    #legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn(limits=c(1, 6000),
                       breaks = c(1,2,3,5,10,20,50,100,200,400,1000,2000,4000),  
                       colours = c("yellow","red","black"),trans="log", na.value = "white")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/countA1_2018.png"),count2,dpi=500)
dev.off()
#wind speed and direction
windows(30,15)
wind2 <- ggplot(data = wsp,
       aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  geom_quiver(data=wdir,aes(x = night, y = altitude_layer,u=u,v=v),col="black",vecsize=1.5,size = 0.1,center = FALSE)+
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('wsp(m/s)', limits=c(0, 40),
                       breaks = seq(0,40,5), 
                       colours = c("white","darkblue"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/windA.png"),wind2,dpi=500)
dev.off()
#wind assistance
windows(30,15)
ass1 <- ggplot(data = ass,
                aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    legend.title = element_blank(),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    #legend.title = element_text(color = "black", size = 10)
  ) +
  coord_cartesian(xlim=c(as.Date("2020-08-15"), as.Date("2020-11-30")))+
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn(limits=c(-30,30),
                       breaks = seq(-30,30,5), 
                       colours = c("cyan","white","orange"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/wind_assA1.png"),ass1,dpi=500)
dev.off()
#temperature
windows(30,15)
temp2 <- ggplot(data = temp,
       aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('t(C)', limits=c(-27,27),
                       breaks = seq(-27,27,5), 
                       colours = c("blue","white","red"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/tempA.png"),temp2,dpi=500)
dev.off()
#Fraction of cloud cover
windows(30,15)
cc2 <- ggplot(data = cc,
       aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('cloud cover (0-1)', limits=c(0,1),
                       breaks = seq(0,1,0.1), 
                       colours = c("gray95","gray33"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/ccA.png"),cc2,dpi=500)
dev.off()
#Relative humidity
windows(30,15)
r2 <- ggplot(data = r,
       aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('rh(%) ', limits=c(0,120),
                       breaks = seq(0,120,20), 
                       colours = c("white","darkgreen"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/rA.png"),r2,dpi=500)
dev.off()
#Specific rain water content
windows(30,15)
crwc2 <- ggplot(data = r,
               aes(x = night, y = altitude_layer, fill = x)) +
  geom_tile() +
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
    #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('night') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('crwc(kg/kg-1) ', limits=c(0,150),
                       breaks = seq(0,150,20), 
                       colours = c("lightskyblue1","lightskyblue4"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",limits=c(as.Date("2018-08-15"), as.Date("2018-11-30")),expand = c(0,0))

ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/crwcA.png"),crwc2,dpi=500)
dev.off()

