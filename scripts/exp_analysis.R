library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

db.file <-"MyRobinData" #provide the name of the database or file for ODBC driver
library(RODBC) 
Rdb <- odbcConnect(db.file)#establish connection with database
Bdb <- odbcConnect("Borssele")
Gdb <- odbcConnect("Gemini")
trackestimate <- sqlQuery(Rdb, "select * from public.trackestimate where timestamp between '2018-10-14 19:00:00' and '2018-10-15 07:00:00' 
                                and airspeed<28
                                order by timestamp")

#select from table track all track that are birds (id higher than 4) during specific times on specific days
# and add separate columns for year, month, day and hour

track <- sqlQuery(Rdb, "select id,timestamp_start,classification_id,phi_diff,airspeed, extract(year from timestamp_start) as year,extract (month from timestamp_start) as month, 
extract (day from timestamp_start) as day,extract (hour from timestamp_start) as hour  from public.track 
                        where timestamp_start between '2020-10-14 18:00:00' and '2020-10-15 05:00:00'
                        and classification_id>4
                        order by timestamp_start")
track$dir_rad <- circular(track$phi_diff, type = "angles", units = "radians", zero = 0, rotation = "clock")
track$dir_deg <- track$phi_diff * 180/pi
track$dir_deg  <- ifelse(track$dir_deg <0, 360+track$dir_deg , track$dir_deg )
track$dir_deg <- circular(track$dir_deg, units = "degrees",modulo ="2pi", template="geographic")

ggplot(track, aes(x=dir_deg)) + 
  geom_histogram(breaks=seq(0,360,10),col='white',fill="black") +
  coord_polar(start = 0) +
  scale_x_continuous("", breaks = c(0,90,180,270),labels = c("N","E","S","W"))+
  theme_minimal()+
  theme(axis.title.y = element_blank(),axis.text.y = element_blank(),panel.grid.minor  =element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="black"),
        panel.grid.major=element_line(colour = "black"))

Sys.setenv(TZ="UTC")
track$localtime <- format(track$timestamp_start,tz="Europe/Amsterdam",usetz=TRUE)
track$l.hour <- hour(track$localtime)
track$l.year <- year(track$localtime)
track$l.month <- month(track$localtime)
track$l.day <- day(track$localtime)


close(Rdb)
#cut(capture.output(print(track),file="track.csv"))#if you want to save the table as csv


###divide into categories per type of the track
flock <- subset(track, classification_id==5, select = id:l.day) 
small <- subset(track, classification_id==8, select = id:l.day)
medium <- subset(track, classification_id==6,select = id:l.day)
big <- subset(track, classification_id==7, select = id:l.day)
###list everything together to be able to loop through ti
alltrack <- list(flock,small,medium,big)
###add hourly and daily timestamps

for (k in 1:length(alltrack)){
  alltrack[[k]]$date <- with(alltrack[[k]], ymd(paste(l.year,l.month,l.day, sep=' ')))
  alltrack[[k]]$timestamp2 <- with(alltrack[[k]], ymd_h(paste(l.year,l.month,l.day, l.hour, sep= ' ')))
}


###agregate values per hour
library(xts)

trackph <- list()
date <- list()
birds_mean <- list()
first <- list()
class <- list()


for (k in 1:length(alltrack)){
  trackph[[k]] <- aggregate(alltrack[[k]]$id, by = list(alltrack[[k]]$timestamp2), FUN="length")
  date [[k]] <- aggregate(alltrack[[k]]$date, by=list(alltrack[[k]]$timestamp2), FUN="mean")
  class[[k]] <- aggregate(alltrack[[k]]$classification_id, by=list(alltrack[[k]]$timestamp2), FUN="median")
  first[[k]] <- merge(class[[k]], date[[k]], by="Group.1", sort=T)
  birds_mean[[k]] <- merge(trackph[[k]],first[[k]],by="Group.1", sort=TRUE)
  names(birds_mean[[k]])[c(1,2,3,4)]<-paste(c("Timestamp","trackph","class", "Date"))
}
###transfer into a dataframe from the list
allbirdsm <- rbind(birds_mean[[1]],birds_mean[[2]],birds_mean[[3]],birds_mean[[4]])
###necessary for plots (changing continuous values to discrete)
allbirdsm$bird <- factor(allbirdsm$class, levels=c(5,7,6,8),labels=c("Flock", "Big birds","Medium birds","Small birds"))
###plotting number of migrants and their classification
windows(8,5)
ggplot(allbirdsm, aes(Date,trackph)) +
  geom_bar(stat="identity",aes(colour=allbirdsm$bird, fill=allbirdsm$bird), show.legend = T) +
  scale_fill_manual(values = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("magenta","red","orange","yellow"), name="Classification", drop=F)+
  ggtitle("Number of tracks Borssele Spring 2020") +
  theme_bw()+
  theme(axis.title.y = element_text(size=16), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Day") + ylab("Number of track") +
  ylim(0,250000)
  scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date("2018-09-01"), as.Date("2018-10-31")))
  

  ggplot(track, aes())
  
  
###comparing how good classification of the radar system is by applying extra filter of the airspeed
flockbig <- subset(flock, airspeed>20, select=id:hour)
flocksmall <- subset(flock, airspeed<20, select=id:hour)
flockmedium <- subset(flock, airspeed<20, select=id:hour)
bigr <- subset(big, airspeed>20, select=id:hour)
mediumr <- subset(medium, airspeed<20, select=id:hour)
smallr <- subset(small, airspeed<20, select=id:hour)

flockbig$bird <- ifelse(flockbig$class==5, 1,0)
flocksmall$bird <- ifelse(flocksmall$class==5, 2,0)
flockmedium$bird <- ifelse(flockmedium$class==5, 3,0)
bigr$bird <- ifelse(bigr$class==6, 4,0)
smallr$bird <- ifelse(smallr$class==7, 5,0)
mediumr$bird <- ifelse(mediumr$class==8, 6,0)



alltrackr <- list(flockbig,flocksmall,flockmedium,bigr,mediumr, smallr)


library(lubridate)
for (k in 1:length(alltrackr)){
  alltrackr[[k]]$date <- with(alltrackr[[k]], ymd(paste(year,month,day, sep=' ')))
  alltrackr[[k]]$timestamp2 <- with(alltrackr[[k]], ymd_h(paste(year,month,day, hour, sep= ' ')))
}

  
trackphr <- list()
dater <- list()
birds_meanr <- list()
firstr <- list()
classr <- list()


for (k in 1:length(alltrackr)){
  trackphr[[k]] <- aggregate(alltrackr[[k]]$id, by = list(alltrackr[[k]]$timestamp2), FUN="length")
  dater [[k]] <- aggregate(alltrackr[[k]]$date, by=list(alltrackr[[k]]$timestamp2), FUN="mean")
  classr[[k]] <- aggregate(alltrackr[[k]]$bird, by=list(alltrackr[[k]]$timestamp2), FUN="median")
  firstr[[k]] <- merge(classr[[k]], dater[[k]], by="Group.1", sort=T)
  birds_meanr[[k]] <- merge(trackphr[[k]],firstr[[k]],by="Group.1", sort=TRUE)
  names(birds_meanr[[k]])[c(1,2,3,4)]<-paste(c("Timestamp","trackph","class", "Date"))
}


###transfer into a dataframe from the list
allbirdsmr <- rbind(birds_meanr[[1]],birds_meanr[[2]],birds_meanr[[3]],birds_meanr[[4]])
###necessary for plots (changing continuous values to discrete)
allbirdsmr$bird <- factor(allbirdsmr$class, levels=c(1,2,3,4,5,6),
                         labels=c("Flock of big birds", "Flock of small birds", 
                                  "Flock of medium birds","Big birds", "Small birds", "Medium birds"))
###plotting number of migrants and their classification
ggplot(allbirdsmr, aes(Date,trackph)) +
  geom_bar(stat="identity",aes(colour=allbirdsmr$bird, fill=allbirdsmr$bird), show.legend = T) +
  scale_fill_manual(values = c("magenta","pink","hotpink","red","yellow","orange"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("magenta","pink","hotpink","red","yellow","orange"), name="Classification", drop=F)+
  ggtitle("Number of track 11-22 October 2018") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Day") + ylab("Number of track")+ 
scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date("2018-10-11"), as.Date("2018-10-22")))


##############################################################
########USING JUST AIRSPEED AS A FILTER#######################
##############################################################

track <- sqlQuery(Rdb, "select id,timestamp_start,classification_id,airspeed, extract(year from timestamp_start) as year,extract (month from timestamp_start) as month, 
                  extract (day from timestamp_start) as day,extract (hour from timestamp_start) as hour  from public.track 
                  where timestamp_start between date '2018-09-21' and date'2018-09-27'
                  and extract(hour from timestamp_start) in (19,20,21,22,23,0,1,2,3,4,5,6,7)
                  and airspeed<27 
                  order by timestamp_start")

library(lubridate)
for (k in 1:length(track)){
  track$date <- with(track, ymd(paste(year,month,day, sep=' ')))
  track$timestamp2 <- with(track, ymd_h(paste(year,month,day, hour, sep= ' ')))
}

for (k in 1:length(track)){
  trackphr <- aggregate(track$id, by = list(track$timestamp2), FUN="length")
  speedr <- aggregate(track$airspeed,by=list(track$timestamp2), FUN="mean")
  dater <- aggregate(track$date, by=list(track$timestamp2), FUN="mean")
  firstr <- merge(speedr,dater,by="Group.1", sort=TRUE)
  birds_meanr <- merge(trackphr,firstr,by="Group.1", sort=TRUE)
  names(birds_meanr)[c(1,2,3,4)]<-paste(c("Timestamp","trackph","speed", "Date"))
}

track$bird <- cut(track$airspeed, breaks=c(5,18,27),
                          labels=c("Small birds", "Other birds"))

birds_meanr$bird <- cut(birds_meanr$speed, breaks=c(5,18,27),
                  labels=c("Small birds", "Other birds"))

ggplot(birds_meanr, aes(Date,trackph)) +
  geom_bar(stat="identity",aes(colour=birds_meanr$bird, fill=birds_meanr$bird), show.legend = T) +
  scale_fill_manual(values = c("yellow","orange"), name="Classification", drop=F)+
  scale_colour_manual(values  = c("yellow","orange"), name="Classification", drop=F)+
  ggtitle("Number of track 21-27 September 2018") + 
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Day") + ylab("Number of track")+ 
  scale_x_date(date_breaks="days", date_labels="%d", limits=c(as.Date("2018-09-21"), as.Date("2018-09-27")))


library(scales)
library(ggplot2)
ggplot(track,aes(x=date, group=bird, fill=bird))+
  stat_bin(colour="black", binwidth=1,
           position="identity") + 
  scale_fill_manual(values = c("yellow","orange"), name="Classification", drop=F)+
  xlab("Date")+
  ylab("Number of tracks")+
  scale_x_date(breaks=date_breaks("days"), labels=date_format("%d %b %y"))


#########################################################
#######################ALTITUDE##########################

vertical <- sqlQuery(Rdb, "select id,timestamp_end as timestamp,classification_id,
                     ST_Z(ST_EndPoint(trajectory)) as alt
                     from public.track
                     WHERE classification_id>4
                     and timestamp_start between '2019-03-29 20:00:00' and '2019-03-30 05:00:00'
                     and tracktype in('RaEl')
                     order by timestamp_end")
#WHERE exists (
#  SELECT * FROM unnest(assignable_properties) n
#  WHERE n in('STRAIGHT')) 
#AND airspeed between '8' and '15'
###altitude is being stored above ellipsoid, therefore we need to add 43.3705 m to get the altitude amsl
vertical$real.alt <- vertical$alt + 43.3705
breaks <- seq(0,3000,200)


require(ggplot2)
windows(10,8)
ggplot(vertical, aes(alt)) +
  geom_histogram(fill="#7a0177",colour="black",
                 breaks=breaks) +
  #scale_fill_manual(values=c("#7a0177"))+
  ggtitle("Flight altitudes Borssele Spring 2020") +
  coord_flip()+
  theme_bw()+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=14), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size=18), plot.title = element_text(size = 18, face = "bold"),panel.grid = element_blank())+
  xlab("Altitudes") + ylab("Number of tracks")+ #ylim(0,180000) +
  scale_x_continuous("",limits=c(0,3000), breaks = breaks)

breaks <- seq(0,300,20)
windows(10,8)
ggplot(vertical, aes(alt)) +
  geom_histogram(fill="goldenrod",colour="black",
                 breaks=breaks) +
  #scale_fill_manual(values=c("#7a0177"))+
  ggtitle("Flight altitudes Borssele Autumn 2019") +
  coord_flip()+
  theme_bw()+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=14), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        axis.title.x = element_text(size=18), plot.title = element_text(size = 18, face = "bold"),panel.grid = element_blank())+
  xlab("Altitudes") + ylab("Number of tracks")+ ylim(0,30000) +
  scale_x_continuous("",limits=c(0,300), breaks = breaks)


track$timestamp_start <- as.POSIXct(track$timestamp_start)
class(oneday$timestamp_start)
track$tc <- cut(track$timestamp_start, breaks = "15 min")
track$tc <- as.POSIXct(track$tc)
