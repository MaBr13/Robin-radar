## Clutter test Luchterduinen
#M. Bradaric
# 14/01/2021
library(sf)
library(DBI)
library(RPostgreSQL)
library(beepr)
library(lubridate)
library(dplyr)
library(imputeTS)
library(ggplot2)

Sys.setenv(TZ='UTC')


# set the database of interest 
database <-'rws01' # SET PARAMETER 
# connect to the database 
con <- dbConnect("PostgreSQL", 
                 dbname=database, # database name  
                 host='robin1.e-ecology.nl', 
                 user= "maja_bradaric",# username
                 password="55nebitno992@") # password

clut <- st_read(con, query ="SELECT date_trunc('minute', timestamp)::character varying as timestamp1,radar_id,
                             avg(landmask_percentage) as l_mask, avg(variantmask_percentage) as v_mask, 
                             avg(rain_percentage) as r_mask FROM public.image
                             INNER JOIN public.ip_metainfo ON public.image.id=public.ip_metainfo.image_id
                             WHERE timestamp between '2020-08-15 00:00:00' and '2020-12-01 00:00:00'
                             GROUP BY timestamp1,radar_id")
beep()
write.table(clut,"C:/Users/mbradar/Documents/Robin/data/clutter_autumn2020_L.csv",sep=";")

activity <- st_read(con,query="SELECT date_trunc('minute', timestamp)::character varying as timestamp1,radar_id,
                             id from public.image
                             WHERE timestamp between '2020-02-15 00:00:00' and '2020-06-01 00:00:00'")
beep()

write.table(activity,"C:/Users/mbradar/Documents/Robin/data/activity_spring2020_L.csv",sep=";")
#SPRING
#load radar data
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring2019mixed&vert.csv",sep=";")
vert <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring2020vert.csv",sep=";")
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp_start <- as.POSIXct(vert$timestamp_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamph<- as.character(ymd_h(paste(date(vert$timestamp_start), hour(vert$timestamp_start),sep= ' ')))
vert$timestamph <- as.POSIXct(vert$timestamph,format="%Y-%m-%d %H:%M:%S")
vert <- subset(vert,timestamp_start>="2020-08-15 15:00:00")
vertH <- vert %>%
  group_by(timestamph) %>%
  summarise(count=length(track_id),classification=median(classification))

#rain
rain <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2019.csv", sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)

#load clutter data
clutter <- read.csv("C:/Users/mbradar/Documents/Robin/data/clutter_spring2020_L.csv",sep=";")
colnames(clutter)[which(names(clutter) == "timestamp1")] <- "timestamp"
clutter$timestamp <- as.POSIXct(clutter$timestamp,tz="UTC")
clutter$timestamph <- as.POSIXct(ymd_h(paste(as.Date(clutter$timestamp), hour(clutter$timestamp),sep= ' ')))
#depending on the year the raadar_id can be 2,3,5,6,9 and 11
#first check the radar id in the data
unique(clutter$radar_id)
clutterH <- subset(clutter,radar_id==5 | radar_id==6 | radar_id==11)

clutH <- clutterH %>%
  group_by(timestamph)%>%
  summarise(l_mask=mean(l_mask),v_mask=mean(v_mask),r_mask=mean(r_mask))

#plots

windows(30,15)
clut <- ggplot(vert, aes(timestamph)) +
  geom_bar(mapping = aes(x = timestamph, y = ..count..,colour=vert$classification, fill=vert$classification), stat = "count",show.legend = T) +
  scale_fill_manual(values = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  geom_line(data=clutH,aes(timestamph,l_mask*100),colour="green")+
  geom_line(data=clutH,aes(timestamph,v_mask*100),colour="brown")+
  geom_line(data=clutH,aes(timestamph,r_mask*100),colour="blue")+
  theme_bw()+
  theme(axis.title.y = element_text(size=16), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=14), axis.text.x=element_text(angle=90), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  scale_x_datetime(date_breaks="days", date_labels="%Y-%m-%d",
                   limits=c(as.POSIXct("2019-02-15 17:00:00"), as.POSIXct("2020-06-01 07:00:00")))+
  scale_y_continuous(name = "Counts", sec.axis = sec_axis( trans=~./100, name="Percentage clutter"))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/clutter/spring/7.png"),clut,dpi=500)
dev.off()

#divide per altitude layer
altdif <- subset(vert,altitude_layer>=0 & altitude_layer<500 | 
                   altitude_layer>=500 & altitude_layer<1000 | 
                   altitude_layer>=1000 & altitude_layer<1500)
altdif$height <- with(altdif,ifelse(altdif$altitude_layer>=0 & altdif$altitude_layer<=200,"low",
                                    ifelse(altdif$altitude_layer>=500 & altdif$altitude_layer<=700,"medium","high")))
altdif$height = factor(altdif$height, levels=c("high","medium","low"), labels=c("high","medium","low"))

windows(30,15)
alts <- ggplot(altdif, aes(timestamph)) +
  geom_bar(mapping = aes(x = timestamph, y = ..count..,colour=classification, fill=classification), stat = "count",show.legend = T) +
  scale_fill_manual(values = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  geom_line(data=clutH,aes(timestamph,l_mask*1000),colour="green")+
  geom_line(data=clutH,aes(timestamph,v_mask*1000),colour="brown")+
  geom_line(data=clutH,aes(timestamph,r_mask*1000),colour="blue")+
  facet_grid(height ~ ., scales = "free_y") + 
  theme_bw()+
  theme(axis.title.y = element_text(size=16), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=14), axis.text.x=element_text(angle=90), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  scale_x_datetime(date_breaks="days", date_labels="%Y-%m-%d",
                   limits=c(as.POSIXct("2020-05-15 17:00:00"), as.POSIXct("2020-06-01 07:00:00")))+
  scale_y_continuous(name = "Counts", sec.axis = sec_axis( trans=~./1000, name="Percentage clutter"))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/clutter/altdifS2020_7.png"),alts,dpi=500)
dev.off()

ggplotly(alts, dynamicTicks = T) %>%
  rangeslider(borderwidth = 0.2) %>%
  layout(hovermode = "x", yaxis = list(tickformat = "%"))


#AUTUMN
#load bird data
#load radar data
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/autumn2019mixedandvert.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp_start <- as.POSIXct(vert$timestamp_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamph<- as.character(ymd_h(paste(date(vert$timestamp_start), hour(vert$timestamp_start),sep= ' ')))
vert$timestamph <- as.POSIXct(vert$timestamph,format="%Y-%m-%d %H:%M:%S")
vert <- subset(vert,timestamp_start>="2020-08-15 15:00:00")
vertH <- vert %>%
  group_by(timestamph) %>%
  summarise(count=length(track_id),classification=median(classification))

#rain
rain <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2019.csv", sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)
#load clutter data
clutter <- read.csv("C:/Users/mbradar/Documents/Robin/data/clutter_autumn2019_L.csv",sep=";")
colnames(clutter)[which(names(clutter) == "timestamp1")] <- "timestamp"
clutter$timestamp <- as.POSIXct(clutter$timestamp,tz="UTC")
clutter$timestamph <- as.POSIXct(ymd_h(paste(as.Date(clutter$timestamp), hour(clutter$timestamp),sep= ' ')))
#depending on the year the raadar_id can be 2,3,5,6,9 and 11
#first check the radar id in the data
unique(clutter$radar_id)
clutterH <- subset(clutter,radar_id==5 | radar_id==6 | radar_id==11)

clutH <- clutterH %>%
  group_by(timestamph)%>%
  summarise(l_mask=mean(l_mask),v_mask=mean(v_mask),r_mask=mean(r_mask))


#plots
windows(30,15)
clut <- ggplot(vert, aes(timestamph)) +
  geom_bar(mapping = aes(x = timestamph, y = ..count..,colour=vert$classification, fill=vert$classification), stat = "count",show.legend = T) +
  scale_fill_manual(values = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  geom_line(data=clutH,aes(timestamph,l_mask*1000),colour="green")+
  geom_line(data=clutH,aes(timestamph,v_mask*1000),colour="brown")+
  geom_line(data=clutH,aes(timestamph,r_mask*1000),colour="blue")+
  theme_bw()+
  theme(axis.title.y = element_text(size=16), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=14), axis.text.x=element_text(angle=90), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  scale_x_datetime(date_breaks="days", date_labels="%Y-%m-%d",
                   limits=c(as.POSIXct("2020-05-15 17:00:00"), as.POSIXct("2020-06-01 07:00:00")))+
  scale_y_continuous(name = "Counts", sec.axis = sec_axis( trans=~./1000, name="Percentage clutter"))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/clutter/spring/7.png"),clut,dpi=500)
dev.off()

altdif <- subset(vert,altitude_layer>=0 & altitude_layer<500 | 
                   altitude_layer>=500 & altitude_layer<1000 | 
                   altitude_layer>=1000 & altitude_layer<1500)
altdif$height <- with(altdif,ifelse(altdif$altitude_layer>=0 & altdif$altitude_layer<=200,"low",
                                    ifelse(altdif$altitude_layer>=500 & altdif$altitude_layer<=700,"medium","high")))
altdif$height = factor(altdif$height, levels=c("high","medium","low"), labels=c("high","medium","low"))

windows(30,15)
alts <- ggplot(altdif, aes(timestamph)) +
  geom_bar(mapping = aes(x = timestamph, y = ..count..,colour=classification, fill=classification), stat = "count",show.legend = T) +
  scale_fill_manual(values = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  geom_line(data=clutH,aes(timestamph,l_mask*1000),colour="green")+
  geom_line(data=clutH,aes(timestamph,v_mask*1000),colour="brown")+
  geom_line(data=clutH,aes(timestamph,r_mask*1000),colour="blue")+
  facet_grid(height ~ ., scales = "free_y") + 
  theme_bw()+
  theme(axis.title.y = element_text(size=16), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=14), axis.text.x=element_text(angle=90), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  scale_x_datetime(date_breaks="days", date_labels="%Y-%m-%d",
                   limits=c(as.POSIXct("2019-11-15 17:00:00"), as.POSIXct("2019-12-01 07:00:00")))+
  scale_y_continuous(name = "Counts", sec.axis = sec_axis( trans=~./1000, name="Percentage clutter"))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/clutter/altdifA2019_R3.png"),alts,dpi=500)
dev.off()
