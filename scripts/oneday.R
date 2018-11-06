library(dplyr)
library(ggplot2)
library(zoo)

db.file <-"MyRobinData" #provide the name of the database or file for ODBC driver
library(RODBC) 
Rdb <- odbcConnect(db.file)#establish connection with database


track <- sqlQuery(Rdb, "select id,timestamp_start,classification_id,airspeed, extract(year from timestamp_start) as year,extract (month from timestamp_start) as month, 
extract (day from timestamp_start) as day,extract (hour from timestamp_start) as hour  from public.track 
                        where timestamp_start between date '2018-09-01' and date'2018-10-31'
                        and extract(hour from timestamp_start) in (16,17,18,19,20,21,22,23,0,1,2,3,4,5,6)
                        and classification_id>4
                        order by timestamp_start")
Sys.setenv(TZ="UTC")
track$localtime <- format(track$timestamp_start,tz="Europe/Amsterdam",usetz=TRUE)

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


oneday <- subset(birds_meanr, Timestamp>'2018-10-28 15:00:00' & Timestamp<'2018-10-29 10:00:00',select=Timestamp:Date)


ggplot(oneday, aes(Timestamp,trackph)) +
  #geom_line(colour="red")+
  geom_smooth(se=FALSE, method="loess", span=0.5,colour="yellow")+
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Hour") + ylab("Number of tracks") +theme_black() 
#scale_x_date(date_breaks="hours",labels="%h", limits=c(as.Date("2018-09-24"), as.Date("2018-09-25")))
