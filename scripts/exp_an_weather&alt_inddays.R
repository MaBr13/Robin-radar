library(ggplot2)
library(rWind)
library(dplyr)
library(lubridate)
library(circular)
library(imputeTS)
library(ggquiver)


#SPRING
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/2018.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp_start <- as.POSIXct(ymd_h(paste(date(vert$timestamp_start), vert$hourx, sep= ' ')))
vert$time <- as.POSIXct(vert$timestamp_start,tz="UTC")
vert$timestamp_start <- as.POSIXct(ymd_h(paste(date(vert$timestamp_start), vert$hourx, sep= ' ')))
vert$time <- as.POSIXct(vert$timestamp_start,tz="UTC")
vert <- vert %>%
  group_by(time,altitude_layer)%>%
  summarise(count=length(track_id))
#load rain data and filter rain hours
rain <- read.table("C:/Users/mbradar/Documents/KNMI_rain/Rain_hourly_spring2019.txt",header=FALSE,sep=",")
colnames(rain)[colnames(rain)==c("V1","V2","V3","V4","V5","V6")] <- c("station","date","hour","dr","rh","r")
rain$date <- ymd(rain$date,tz="UTC")
rain$time <- with(rain,ymd_h(paste(date,hour,sep = ' '),tz="UTC"))
vert <- left_join(vert,rain,by=c("time"))
vert$count <- ifelse(vert$r==0,vert$count,NA)


weather <- read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/Spring_2019.csv", sep=';')
weather$altitude_layer <- floor((weather$height)/100)*100
weather$wind_dir <- uv2ds(weather$u,weather$v)[,1]
weather$wind_dir <- as.circular(weather$wind_dir,units = "degrees",modulo ="2pi",zero=pi/2,rotation="clock", template="geographic")
weather$wind_sp <- uv2ds(weather$u,weather$v)[,2]
weather$temp <- weather$t - 273.15 #to get temperature in C from K
weather$time <- as.POSIXct(weather$time,tz="UTC")


check <- left_join(vert,weather,by=c('altitude_layer','time'))
check2 <- subset(check,altitude_layer<=1500)
listp <- split(check2,check2$night)


for(k in 1:length(listp)){
  listp[[k]]$cc<- na_interpolation(listp[[k]]$cc, option ="linear")
  listp[[k]]$r<- na_interpolation(listp[[k]]$r, option ="linear")
  listp[[k]]$crwc<- na_interpolation(listp[[k]]$crwc, option ="linear")
  listp[[k]]$t<- na_interpolation(listp[[k]]$t, option ="linear")
  listp[[k]]$wind_dir<- na_interpolation(listp[[k]]$wind_dir, option ="linear")
  listp[[k]]$wind_sp<- na_interpolation(listp[[k]]$wind_sp, option ="linear")
  listp[[k]]$temp<- na_interpolation(listp[[k]]$temp, option ="linear")
}

check3 <-do.call("rbind",listp)
oneday <- subset(check3, night=="2019-10-30")
oneday1 <- subset(check2,night=="2019-10-30")

pts1 <- oneday1[,c('time','altitude_layer','count')]
pts1$IDgrid<-with(pts1,interaction(time,altitude_layer))
pts1$IDNgrid<-factor(pts1$IDgrid)
levels(pts1$IDNgrid)<-seq_along(levels(pts1$IDNgrid))
tab <- aggregate(pts1$count,pts1[,c('time','altitude_layer','IDNgrid')],FUN=sum,na.rm=TRUE)
tab$x <- ifelse(tab$x==0,NA,tab$x)

pts <- oneday[,c('time','altitude_layer','wind_dir','wind_sp','wind_ass','temp','cc','r','t','crwc')]
pts$IDgrid<-with(pts,interaction(time,altitude_layer))
pts$IDNgrid<-factor(pts$IDgrid)
levels(pts$IDNgrid)<-seq_along(levels(pts$IDNgrid))
wsp <- aggregate(pts$wind_sp, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
wdir <- aggregate(pts$wind_dir,pts[,c('time','altitude_layer','IDNgrid')], 
                  function(x) mean.circular(x,units = "degrees",modulo ="2pi", 
                                            template="geographic",na.rm=T))
ass <- aggregate(pts$wind_ass, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
wdir$u <- ds2uv(wdir$x,wsp$x)[,1]
wdir$v <- ds2uv(wdir$x,wsp$x)[,2]
temp <- aggregate(pts$temp, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
cc <- aggregate(pts$cc, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
r <- aggregate(pts$r, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
crwc <- aggregate(pts$crwc, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)

#Bird counts
windows(30,15)
count1 <- ggplot(data = tab,
                 aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('nr.tracks', limits=c(1, 6000),
                       breaks = c(1,2,3,5,10,20,50,100,200,400,1000,2000,4000),  
                       colours = c("yellow","red","black"),trans="log", na.value = "white")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/countS_20191030_G.png"),count1,dpi=500)
dev.off()
#wind speed and direction
windows(30,15)
wind1 <- ggplot(data = wsp,
                aes(x = time, y = altitude_layer, fill = x)) +
  geom_tile() +
  geom_quiver(data=wdir,aes(x = time, y = altitude_layer,u=u,v=v),col="black",vecsize=1.5,size = 0.1,center = TRUE)+
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('wspd(m/s)', limits=c(0, 40),
                       breaks = seq(0,40,5), 
                       colours = c("white","darkblue"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/windS_20191030_G.png"),wind1,dpi=500)
dev.off()
#wind assistance
windows(30,15)
ass1 <- ggplot(data = ass,
                aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('was(m/s)', limits=c(-30,30),
                       breaks = seq(-30,30,5), 
                       colours = c("cyan","white","orange"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/assS_20191030_G.png"),ass1,dpi=500)
dev.off()
#temperature
windows(30,15)
temp1 <- ggplot(data = temp,
                aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('t (C)', limits=c(-25,25),
                       breaks = seq(-25,25,5), 
                       colours = c("blue","white","red"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/tempS_20191029_G.png"),temp1,dpi=500)
dev.off()
#Fraction of cloud cover
windows(30,15)
cc1 <- ggplot(data = cc,
              aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('cc', limits=c(0,1),
                       breaks = seq(0,1,0.1), 
                       colours = c("gray95","gray33"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/ccS_20191029_G.png"),cc1,dpi=500)
dev.off()
#Relative humidity
windows(30,15)
r1 <- ggplot(data = r,
             aes(x = time, y = altitude_layer, fill = x)) +
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
  scale_fill_gradientn('rh (%)', limits=c(0,120),
                       breaks = seq(0,120,20), 
                       colours = c("white","darkgreen"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/rS_20191029_G.png"),r1,dpi=500)
dev.off()
#Specific rain water content
windows(30,15)
crwc1 <- ggplot(data = r,
                aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('crwc (kg/kg-1) ', limits=c(0,150),
                       breaks = seq(0,150,20), 
                       colours = c("lightskyblue1","lightskyblue4"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))

ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/crwcS_20191029_G.png"),crwc1,dpi=500)
dev.off()

#AUTUMN
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/2018_onenight.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert$timestamp_start <- as.POSIXct(vert$timestamp_start,tz="UTC")
vert$time <- strptime(vert$timestamp_start, format = "%Y-%m-%d %H")
vert$time <- as.POSIXct(vert$time,tz="UTC")



weather <- read.csv("C:/Users/mbradar/Documents/ERA5/Luchterduinen/Oct_2018.csv", sep=';')
weather$altitude_layer <- floor((weather$height)/100)*100
weather$wind_dir <- uv2ds(weather$u,weather$v)[,1]
weather$wind_dir <- as.circular(weather$wind_dir,units = "degrees",modulo ="2pi",zero=pi/2,rotation="clock", template="geographic")
weather$wind_sp <- uv2ds(weather$u,weather$v)[,2]
weather$temp <- weather$t - 273.15 #to get temperature in C from K
weather$time <- as.POSIXct(weather$time)


check <- left_join(vert,weather,by=c('altitude_layer','time'))
check2 <- subset(check,altitude<=2000)
listp <- split(check2,check2$altitude_layer)


for(k in 1:length(listp)){
  listp[[k]]$cc<- na_interpolation(listp[[k]]$cc, option ="linear")
  listp[[k]]$r<- na_interpolation(listp[[k]]$r, option ="linear")
  listp[[k]]$crwc<- na_interpolation(listp[[k]]$crwc, option ="linear")
  listp[[k]]$t<- na_interpolation(listp[[k]]$t, option ="linear")
  listp[[k]]$wind_dir<- na_interpolation(listp[[k]]$wind_dir, option ="linear")
  listp[[k]]$wind_sp<- na_interpolation(listp[[k]]$wind_sp, option ="linear")
  listp[[k]]$temp<- na_interpolation(listp[[k]]$temp, option ="linear")
}

check3 <-do.call("rbind",listp)
oneday <- subset(check3, night=="2019-10-28")
oneday1 <- subset(check2,night=="2019-10-28")

pts1 <- oneday1[,c('time','altitude_layer','count')]
pts1$IDgrid<-with(pts1,interaction(time,altitude_layer))
pts1$IDNgrid<-factor(pts1$IDgrid)
levels(pts1$IDNgrid)<-seq_along(levels(pts1$IDNgrid))
tab <- aggregate(pts1$count,pts1[,c('time','altitude_layer','IDNgrid')],FUN=sum,na.rm=TRUE)
tab$x <- ifelse(tab$x==0,NA,tab$x)

pts <- oneday[,c('time','altitude_layer','wind_dir','wind_ass','wind_sp','temp','cc','r','t','crwc')]
pts$IDgrid<-with(pts,interaction(time,altitude_layer))
pts$IDNgrid<-factor(pts$IDgrid)
levels(pts$IDNgrid)<-seq_along(levels(pts$IDNgrid))
wsp <- aggregate(pts$wind_sp, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
wdir <- aggregate(pts$wind_dir,pts[,c('time','altitude_layer','IDNgrid')], 
                  function(x) mean.circular(x,units = "degrees",modulo ="2pi", 
                                            template="geographic",na.rm=T))
ass <- aggregate(pts$wind_ass, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
wdir$u <- ds2uv(wdir$x,wsp$x)[,1]
wdir$v <- ds2uv(wdir$x,wsp$x)[,2]
temp <- aggregate(pts$temp, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
cc <- aggregate(pts$cc, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
r <- aggregate(pts$r, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)
crwc <- aggregate(pts$crwc, pts[,c('time','altitude_layer','IDNgrid')], FUN=mean)


#Bird counts
windows(30,15)
count2 <- ggplot(data = tab,
                 aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('nr.tracks', limits=c(1, 6000),
                       breaks = c(1,2,3,5,10,20,50,100,200,400,1000,2000,4000),  
                       colours = c("yellow","red","black"), trans="log",na.value = "white")+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/countA_20191028.png"),count2,dpi=500)

dev.off()
#wind speed and direction
windows(30,15)
wind2 <- ggplot(data = wsp,
                aes(x = time, y = altitude_layer, fill = x)) +
  geom_tile() +
  geom_quiver(data=wdir,aes(x = time, y = altitude_layer,u=u,v=v),col="black",vecsize=1.5,size = 0.1,center = FALSE)+
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('wsp(m/s)', limits=c(0, 40),
                       breaks = seq(0,40,5), 
                       colours = c("white","darkblue"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/windA_20191118.png"),wind2,dpi=500)
dev.off()
#wind assistance
windows(30,15)
ass2 <- ggplot(data = ass,
                aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('was(m/s)', limits=c(-30,30),
                       breaks = seq(-30,30,5), 
                       colours = c("cyan","white","orange"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/assA_20191028.png"),ass2,dpi=500)
dev.off()
#temperature
windows(30,15)
temp2 <- ggplot(data = temp,
                aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('t(C)', limits=c(-25,25),
                       breaks = seq(-25,25,5), 
                       colours = c("blue","white","red"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/tempA_20191118.png"),temp2,dpi=500)
dev.off()
#Fraction of cloud cover
windows(30,15)
cc2 <- ggplot(data = cc,
              aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('cc (0-1)', limits=c(0,1),
                       breaks = seq(0,1,0.1), 
                       colours = c("gray95","gray33"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/ccA_20191118.png"),cc2,dpi=500)
dev.off()
#Relative humidity
windows(30,15)
r2 <- ggplot(data = r,
             aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('rh (%) ', limits=c(0,120),
                       breaks = seq(0,120,20), 
                       colours = c("white","darkgreen"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/rA_20191118.png"),r2,dpi=500)
dev.off()
#Specific rain water content
windows(30,15)
crwc2 <- ggplot(data = r,
                aes(x = time, y = altitude_layer, fill = x)) +
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
  xlab('time') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('crwc(kg/kg-1) ', limits=c(0,150),
                       breaks = seq(0,150,20), 
                       colours = c("lightskyblue1","lightskyblue4"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(wsp$time[1]), as_datetime(max(wsp$time))),expand = c(0,0))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/crwcA_20191128.png"),crwc2,dpi=500)
dev.off()
